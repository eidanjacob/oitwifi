library(shiny)
library(readr)
library(raster)
library(sp)
library(deldir)
library(lubridate)
library(dplyr)
# library(rgeos)
library(ggplot2)
library(leaflet)
library(animation)
library(gganimate) ## NEED DIFFERENT VERSION
# install with devtools::install_github('nteetor/gganimate')
library(tidyr)
library(RColorBrewer)
library(maps)
library(tweenr)

# Global Vars
wallsdf <- apsdf <- eventsdf <- voronoiSPDF <- floors <- offsets <- plotsP <- plotsM <- labelLocations <- NULL
timeSteps <- c("30 min" = 1800, "1 hr" = 3600, "2 hr" = 7200)
ani.options(convert = "C:/Autodesk/ImageMagick-7.0.8-Q16/convert")

ui <- fluidPage(
  
  navbarPage("Single Building Wireless", id = "tabs",
             tabPanel("File Upload", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("shape", "Floor Shapes"),
                          fileInput("aps", "Access Point Positions"),
                          fileInput("events", "Event Data"),
                          actionButton("read", "Go")
                        ),
                        mainPanel(
                          htmlOutput("diagnostic")
                        )
                      )
             ),
             tabPanel("Confirm Upload",
                      plotOutput("floorPlan"),
                      actionButton("plotsOk", "Looking Good")
             ),
             tabPanel("Summary",
                      wellPanel(tableOutput("summaryTable"))),
             tabPanel("GGanimate",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("stepSize", "Time Step", 
                                      choices = names(timeSteps)),
                          sliderInput("frameDelay", "Frame Delay (ms)",
                                      min = 100, max = 5000, value = 1000,
                                      step = 50),
                          checkboxInput("displayMacs", "Display Macs", value = TRUE),
                          actionButton("generateGIF", "Run Animation")
                        ),
                        mainPanel(
                          imageOutput("gif")
                        )
                      )
             )
  )
)

server <- function(input, output){
  
  options(shiny.maxRequestSize=1024^6)
  hideTab("tabs", "Confirm Upload")
  hideTab("tabs", "Summary")
  hideTab("tabs", "GGanimate")
  
  observeEvent(input$read, {
    # The user has indicated they are ready to proceed. 
    # Read the submitted data frames and output diagnostics.
    req(input$shape)
    req(input$aps)
    req(input$events)
    tryCatch(
      {
        wallsdf <<- read_csv(input$shape$datapath)
        apsdf <<- read_csv(input$aps$datapath)
        eventsdf <<- read_csv(input$events$datapath)
      },
      error = function(e) {stop(safeError(e))}
    )
    # Null or non-csv inputs will not make it past this point. 
    # Check that columns name match.
    if(!all(c("transX", "transY", "floor") %in% names(wallsdf)) | 
       !all(c("transX", "transY", "floor", "ap") %in% names(apsdf))){
      warning("Invalid Input! Fix before Proceeding.")
    } else {
      # Passed basic sanity check, run more advanced?
      # eventsdf <- eventsdf %>% filter(ap %in% apsdf$ap)
      floors <<- sort(unique(wallsdf$floor))
      if(any(floors != sort(unique(apsdf$floor)))){
        warning("Files contain information on different floors.")
      }
      msgText <- paste(as.character(nrow(apsdf)), "APs on", 
                       as.character(length(floors)), "floors.",
                       "<br/> Events:", as.character(nrow(eventsdf)))
      output$diagnostic <- renderUI(HTML(msgText))
    }
    # ###########
    # apsdf <- read_csv("./perkins_aps")
    # wallsdf<- read_csv("./perkins_walls")
    fn = length(floors)
    xRange <- max(
      sapply(floors, function(f){
        max(wallsdf$transX[wallsdf$floor == f]) - 
          min(wallsdf$transX[wallsdf$floor == f])
      })
    )
    yRange <- max(
      sapply(floors, function(f){
        max(wallsdf$transY[wallsdf$floor == f]) - 
          min(wallsdf$transY[wallsdf$floor == f])
      })
    )
    offsets <<- data.frame(
      t(
        sapply(1:fn, function(i){
          xOff <- (xRange+20) * ((i - 1) %% 2)
          yOff <- (yRange) * floor(i/2 - 1/2)
          return(c(xOff, yOff))
        })
      )
    )
    rownames(offsets) <- as.character(floors)
    colnames(offsets) <- c("xOff", "yOff")
    
    xMin <- min(wallsdf$transX)
    yMin <- min(wallsdf$transY)
    labelLocations <<- offsets %>%
      mutate(xOff = xOff + xMin + xRange/2) %>%
      mutate(yOff = yOff + yMin + yRange + 50)
    
    wallsdf <- wallsdf %>%
      mutate(transX = transX + offsets[as.character(wallsdf$floor), "xOff"]) %>%
      mutate(transY = transY + offsets[as.character(wallsdf$floor), "yOff"])
    
    apsdf <- apsdf %>%
      mutate(transX = transX + offsets[as.character(apsdf$floor), "xOff"]) %>%
      mutate(transY = transY + offsets[as.character(apsdf$floor), "yOff"])
    
    # Split stuff by floor and draw some polygons.
    borderList <- lapply(floors, function(f){
      fwalls <- wallsdf %>% filter(floor == f)
      border <- Polygons(list(Polygon(fwalls[,c("transX", "transY")])),as.character(f))
    })
    joined <- SpatialPolygons(borderList)
    fcells <- deldir(data.frame(x = apsdf$transX, y = apsdf$transY))
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
      x = apsdf$transX, 
      y = apsdf$transY))
    # tag polygons with ap name
    SPDF@data$id = as.character(apsdf[, "ap"][[1]])
    sapply(1:length(apsdf[, "ap"]), function(x){
      SPDF@polygons[[x]]@ID <- as.character(apsdf[, "ap"][[1]][x])
      SPDF <<- SPDF
    })
    voronoiSPDF <<- raster::intersect(SPDF, joined)
    output$floorPlan <- renderPlot({
      plot(voronoiSPDF, main = "Floor Plan Generated from Uploaded Files")})
    output$diagnostic <- renderUI(HTML(
      paste(msgText, "<br/> Polygons drawn, please confirm.")))
    showTab("tabs", "Confirm Upload")
  })
  
  suppressWarnings(observeEvent(input$plotsOk, {
    showTab("tabs", "GGanimate")
    showTab("tabs", "Summary")
    binByAp <- eventsdf %>% count(ap, sort = TRUE)
    
    output$summaryTable <- renderTable({
      # some summary statistics
      t(
        data.frame(
          "Unique APs" = length(unique(eventsdf$ap)),
          "Unique MACs" = length(unique(eventsdf$macaddr)),
          "Most Total Events" = paste(binByAp[1,1], ", n =", binByAp[1,2]),
          "First Event" = min(eventsdf$`_time`),
          "Last Event" = max(eventsdf$`_time`),
          "Duration" = max(eventsdf$`_time`) - min(eventsdf$`_time`)
        )
      )
    }, rownames = TRUE, colnames = FALSE)
    
    # Make calculations for choropleths.
    startTime <- min(eventsdf$`_time`)
    endTime <- min(eventsdf$`_time`)
    
    # convert polys to ggplot2 usable format and save in list
    fortified <- fortify(voronoiSPDF, region = "id")
    
    plotsP <<- lapply(timeSteps, function(delta){
      binnedEvents <- eventsdf # match events to appropriate bin
      binnedEvents$bin <- cut_interval(as.numeric(eventsdf$`_time`), 
                                       length = delta, ordered_result = TRUE)
      
      chartData <- summarise(group_by(binnedEvents, ap, bin), n()) %>%
        complete(bin, ap) %>%
        replace_na(list(`n()` = 0))
      ready <- left_join(fortified, chartData, by = c("id" = "ap"))
      ready$`n()`[which(is.na(ready$`n()`))] <- 0
      p <- ggplot() +
        geom_polygon(data = ready, aes(fill = `n()`,
                                       x = long,
                                       y = lat,
                                       frame = as.numeric(bin),
                                       group = group)
        ) +
        scale_fill_gradient(low = "#e6e6Fa", high = "#4b0082") +
        coord_fixed() +
        geom_path(data = ready, aes(x = long,
                                    y = lat,
                                    group = group),
                  color = "gray",
                  size = 1) + 
        theme_bw() + xlab("Feet") + ylab("Feet") +
        geom_text(aes(x = labelLocations[,1],
                      y = labelLocations[,2],
                      label = paste("Floor", floors)))

      return(p)
    })
    
    plotsM <<- lapply(timeSteps, function(delta){
      binnedEvents <- eventsdf # match events to appropriate bin
      binnedEvents$bin <- cut_interval(as.numeric(eventsdf$`_time`), 
                                       length = delta, ordered_result = TRUE)
      
      # Preparing the points data.
      macNum <- length(unique(binnedEvents$macaddr)) # number of devices to display on map
      binnedEvents <- binnedEvents %>% 
        filter(macaddr %in% unique(binnedEvents$macaddr)[1:macNum])
      
      # Matching aps to coordinates.
      binnedEvents$bin <- as.numeric(binnedEvents$bin)
      frameNum <- max(as.numeric(binnedEvents$bin))
      centers <- voronoiSPDF@data 
      binnedEvents <- full_join(binnedEvents, centers, by = c("ap" = "id"))
      binnedEvents <- binnedEvents[which(!is.na(binnedEvents$x)), ]
      binnedEvents <- binnedEvents[which(!is.na(binnedEvents$macaddr)), ]
      binnedEvents <- binnedEvents %>%
        mutate(ease = "linear")
      
      binnedEvents <- binnedEvents %>% # tweenr requires columns to be trimmed
        dplyr::select(x, y, bin, macaddr, ease)
      events_tween <- tween_elements(binnedEvents, "bin", "macaddr", "ease", nframes = frameNum)
      events_tween$bin <- round(events_tween$bin)
      
      q <- geom_point(data = events_tween, aes(x = x, y = y, group = .group, frame = bin), color = "blue", alpha = 0.1)
      
      return(q)
    })
    
    names(plotsP) <- names(timeSteps)
    hideTab("tabs", "File Upload")
    hideTab("tabs", "Confirm Upload")
  }))
  
  observeEvent(input$generateGIF, {
    ani.options(interval = input$frameDelay / 1000)
    fileName <- "timeLapse.gif"
    bins <- seq(min(eventsdf$`_time`), max(eventsdf$`_time`), timeSteps[input$stepSize])
    plot <- plotsP[[as.character(input$stepSize)]]
    if(input$displayMacs){
      plot <- plot + plotsM[[as.character(input$stepSize)]]
    }
    gg_animate(plot, 
              filename = fileName,
              title_frame = ~ bins[.])
    output$gif <- renderImage({
      list(src = fileName)
    })
  })
  
}

shinyApp(ui = ui, server = server)
