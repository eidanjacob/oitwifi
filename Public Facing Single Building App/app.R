library(shiny)
library(readr)
library(raster)
library(deldir)
library(dplyr)
library(rgeos)
library(ggplot2)
library(maps)

### ### ### Everything you'd want to mess with should be in here
threshold <- 10 # APs with this many or fewer events will display 0 
refresh <- 30 # Refresh rate in minutes
orientation <- c("left" = "To Chapel",
                 "right" = "To Bostock",
                 "down" = "To Abele Quad",
                 "up" = "To CIEMAS")
building <- "Perkins Library"
height <- 600 # in px
txtscl <- 1 # Text scaling factor (reduce if text too large)
### ### ###

# Global Variables
apsdf <- read_csv("./apData.csv")
wallsdf <- read_csv("./buildingData.csv")
floors = unique(apsdf$floor)
reportFile = "reportFile.txt"
con <- file(reportFile, "w")
cat("Access Points in Building Data that are missing from Event Data\n", file = con)
close(con)

ui <- fluidPage(
  plotOutput("myPlot", height = height)
)

server <- function(input, output){
  
  # Finds the largest floor's span along each dimension.
  xRange <- max(
    sapply(floors, function(f){
      max(wallsdf$X[wallsdf$floor == f]) - 
        min(wallsdf$X[wallsdf$floor == f])
    })
  )
  yRange <- max(
    sapply(floors, function(f){
      max(wallsdf$Y[wallsdf$floor == f]) - 
        min(wallsdf$Y[wallsdf$floor == f])
    })
  )
  
  # Ranges are used to calculate offsets to neatly arrange floors.
  offsets <- data.frame(
    t(
      sapply(1:length(floors), function(i){
        xOff <- (xRange) * ((i - 1) %% 2)
        yOff <- (yRange * 0.8) * floor(i/2 - 1/2)
        return(c(xOff, yOff))
      })
    )
  )
  rownames(offsets) <- as.character(floors)
  colnames(offsets) <- c("xOff", "yOff")
  
  # (xMin, yMin) is a sort of 'origin' point
  xMin <- min(wallsdf$X)
  yMin <- min(wallsdf$Y)
  
  # Want to label floors, so calculate location of labels: horizontally centered and above corresponding floors.
  labs <- offsets %>%
    mutate(xOff = xOff + xMin + xRange/2) %>%
    mutate(yOff = yOff + yMin + yRange*1.1)
  names(labs) <- c("X", "Y")
  labs$text <- paste("Floor", floors)
  
  # Add the offsets to the wall and ap coordinates by floor.
  wallsdf <- wallsdf %>%
    mutate(X = X + offsets[as.character(wallsdf$floor), "xOff"]) %>%
    mutate(Y = Y + offsets[as.character(wallsdf$floor), "yOff"])
  apsdf <- apsdf %>%
    mutate(X = X + offsets[as.character(apsdf$floor), "xOff"]) %>%
    mutate(Y = Y + offsets[as.character(apsdf$floor), "yOff"])
  
  # Draw a polygon around each floor.
  borderList <- lapply(floors, function(f){
    fwalls <- wallsdf %>% filter(floor == f)
    border <- Polygons(list(Polygon(fwalls[,c("X", "Y")])),as.character(f))
  })
  joined <- SpatialPolygons(borderList)
  
  # Draw voronoi cells around each Access Point.
  fcells <- deldir(data.frame(x = apsdf$X, y = apsdf$Y))
  w = tile.list(fcells)
  polys = vector(mode = "list", length = length(w))
  for (j in seq(along=polys)) {
    pcrds <- cbind(w[[j]]$x, w[[j]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[j]] <- Polygons(list(Polygon(pcrds)), ID=as.character(j))
  }
  SP <- SpatialPolygons(polys)
  for(x in 1:nrow(apsdf)){
    SP@polygons[[x]]@ID <- as.character(x)
  }
  SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(
    x = apsdf$X, 
    y = apsdf$Y))
  # Tag voronoi cells with ap name
  SPDF@data$id = as.character(apsdf[, "ap"][[1]])
  sapply(1:length(apsdf[, "ap"]), function(x){
    SPDF@polygons[[x]]@ID <- as.character(apsdf[, "ap"][[1]][x])
    SPDF <<- SPDF
  })
  
  # Intersect voronoi cells with polygons of floors
  voronoiSPDF <- raster::intersect(SPDF, joined)
  
  observe({
    invalidateLater(refresh * 60000) # Will auto-update at least once per [refresh rate] minutes
    
    # Count the number of events per access point. This is a crude measure of network load but it's good enough for students.
    eventsdf <- read_csv("./eventData.csv")
    chartData <- summarise(group_by(eventsdf, ap), n())
    
    # Are there access points with no events? If so, make a note in reportFile.
    missing <- apsdf$ap[which( !(apsdf$ap %in% chartData$ap))]
    if(length(missing) != 0){
      missdf <- data.frame(missing, 0)
      for(missingAP in missing){
        con <- file(reportFile, "a")
        cat(missingAP, as.character(Sys.time()), "\n", file = con, append = TRUE)
        close(con)
      }
      names(missdf) <- names(chartData)
      chartData <- rbind(chartData, missdf)
    }
    
    # If there are 10 or fewer events, display 0 events. (Don't want to point out where people may be sitting alone).
    chartData$Utilization <- chartData$`n()` * as.numeric((chartData$`n()` > threshold))
    
    # Convert polygons to ggplot2-usable format and save in list
    fortified <- fortify(voronoiSPDF, region = "id")
    ready <- left_join(fortified, chartData, by = c("id" = "ap"))
    
    # Code to generate plot
    output$myPlot <- renderPlot({
      ggplot() + 
        # Draw polygons, fill according to number of events, draw borders
        geom_polygon(data = ready, aes(fill = Utilization, x = long, y = lat, group = group)) +
        scale_fill_gradient(low = "#e3e4e8", high = "#001A57", guide = FALSE) +
        geom_path(data = ready, aes(x = long, y = lat, group = group), color = "black", size = 0.5) +
        # Lock aspect ratio, add title, subtitle
        coord_fixed() + ggtitle(paste("WiFi Utilization in", building), 
                                subtitle = "Wireless access points in darker locations are experiencing greater activity.") + 
        # Set options for axis labels. Eventually we want to use them to help the user orient themselves on the map.
        theme(axis.title.x = element_text(face = "bold", size = 20 * txtscl, family = "serif"), 
              axis.text.x = element_blank(), axis.ticks.x = element_blank(),
              axis.title.y = element_text(face = "bold", size = 20 * txtscl, family = "serif"), 
              axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              plot.title = element_text(size = 30 * txtscl, hjust = 0.5),
              plot.subtitle = element_text(size = 20 * txtscl, hjust = 0.5),
              panel.background = element_rect(fill = "white")) +
        # Add labels on floors and axes.
        geom_text(aes(x = X, y = Y, label = text), data = labs, size = 8 * txtscl) +
        scale_x_continuous(orientation["down"], sec.axis = sec_axis(~ . * 1, name = orientation["up"])) +
        scale_y_continuous(orientation["left"], sec.axis = sec_axis(~ . * 1, name = orientation["right"]))
    })
  })
}

shinyApp(ui = ui, server = server)