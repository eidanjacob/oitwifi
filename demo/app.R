# This is a demo of the maps we have made from the wireless data.

# Load packages:
library(readr) # for reading csv files
library(leaflet) # for mapping
library(dplyr) # for data wrangling
library(shiny) # for interactive maps
library(deldir) # for voronoi cell calculations
library(sp) # for drawing polygons
library(rgdal) # for drawing polygons
library(lubridate) # for easy handling of times and dates
library(geosphere) # for haversine formula (calculate distance on sphere)
library(raster)
library(ggplot2)
library(animation)
library(gganimate) ## NEED DIFFERENT VERSION
# install with devtools::install_github('nteetor/gganimate')
library(tidyr)
library(RColorBrewer)
library(maps)
library(tweenr)
library(mapview)

# For Campus Maps
# ###################
# reading in data (project folder is working directory)
coord <- read_csv("../locationsToCoordinates.csv")
coord <- coord[order(coord$location),] # alphabetize location - coordinate dictionary
validLocations <- read_csv("../allAPs.csv") # aps <-> locations
dukeShape <- read_csv("../dukeShape.txt", col_names = FALSE)

numAPs <- validLocations %>% # number of APs per location
  group_by(location) %>%
  summarise(num = n())

# splunkData <- read_csv("../eventData.csv")
#
# # match aps to locations, merge for coordinates
# df <- splunkData[!is.na(splunkData$ap),] # remove observations with no ap
#
# # Some aps are in splunk data with name, some with number - code below matches location using whichever is available
# nameMatch = which(validLocations$APname %in% df$ap) # find which aps have their name in the data
# numMatch = which(validLocations$APnum %in% df$ap) # find which aps have their number in the data
# validLocations$ap = c(NA) # new "flexible" column to store either name or number
# validLocations$ap[nameMatch] = validLocations$APname[nameMatch]
# validLocations$ap[numMatch] = validLocations$APnum[numMatch]
#
# validLocations <- merge(coord, validLocations) # link coordinates to locations
# # use the new "flexible" ap variable to merge coordinates onto df
# df <- merge(df, validLocations, by = "ap") # this is the slow step
#
# # merge with OUI table to identify manufacturers
# df$prefix <- sapply(df$macaddr, function(mac){
#   str <- substr(mac, 1, 8)
#   return(gsub(":", "-", toupper(str)))
# })
# oui <- read_csv("../ouiDF.txt")
# df <- merge(df, oui)
# write.csv(df, "../mergedData.csv")

# reading in data from directory
df <- NULL
directory <- "../data" # name of directory with data
files <- list.files(directory, full.names = TRUE)
lapply(files, function(fname) {
  df <<- rbind(df, read_csv(paste0(fname)))
})

df$`_time` <- force_tz(ymd_hms(df$`_time`), "EST")

# draw duke border
p = Polygon(dukeShape)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))

# remove out-of-bounds locations
inBounds <- sapply(1:nrow(coord), function(x) {
  point.in.polygon(coord$long[x], coord$lat[x], unlist(dukeShape[,1]), unlist(dukeShape[,2]))
})
coord <- coord[inBounds == 1,]
coord <- merge.data.frame(coord,numAPs)
N = nrow(coord)
# calculating voronoi cells and converting to polygons to plot on map
z <- deldir(coord$long, coord$lat) # computes cells
# convert cell info to spatial data frame (polygons)
w <- tile.list(z)
polys <- vector(mode="list", length=length(w))
for (i in seq(along=polys)) {
  pcrds <- cbind(w[[i]]$x, w[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1,])
  polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
}
SP <- SpatialPolygons(polys)
SP <- intersect(SP, sps)
for(x in 1:nrow(coord)){
  SP@polygons[[x]]@ID <- as.character(x)
}
SPDF <- SpatialPolygonsDataFrame(SP, data=data.frame(x=coord[,2], y=coord[,3]))
# tag polygons with location name
SPDF@data$ID = coord$location
sapply(1:length(coord$location), function(x){
  SPDF@polygons[[x]]@ID <- coord$location[x]
  SPDF <<- SPDF
})

# Default coordinates that provide overview of entire campus
defLong <- -78.932525 # default longitude
defLati <- 36.002145 # default latitude
zm <- 15 # default zoom level

# Coordinates for West
wLati <- 36.0003456
wLong <- -78.939647
wzm <- 16

# Coordinates for East
eLati <- 36.0063344
eLong <- -78.9154213
ezm <- 17

# Coordinates for Central
cLati <- 36.0030883
cLong <- -78.9258819
czm <- 17


# Areas of polygons were calculated in original units (degrees). The code below approximates a sq. meter measure to a square degree (In Durham)
p1 <- c(defLong, defLati)
degScale = -3
p2 <- c(defLong + 10 ^ degScale, defLati)
p3 <- c(defLong, defLati + 10 ^ degScale)
# The Haversine formula calculates distances along a spherical surface.
areaConvert = distHaversine(p1, p2) * distHaversine(p1, p3) # = square meters per 10^degScale square degrees (in Durham)
areaConvert = areaConvert / 10^(2 * degScale) # square meters per square degree

# ------------------------------
# Mess with these numbers if you want.
timeSteps = c("1hr" = 60*60, "2hr" = 2*60*60, "4hr" = 4*60*60) # in seconds
# timeSteps = c("4 hr" = 4*60*60)
delay = 2700 # in milliseconds
# ------------------------------

start.time = (min(df$`_time`))
end.time = (max(df$`_time`))

# # Filtering for all macaddrs that moved/was registered within a certain period of the start time
# # to limit the size of the data set and prevent RStudio from crashing
# # It then samples num random macaddrs later in the code
num <- 1000 # number of macaddrs to visualize
currMacs <- NULL # just to initialize so it exists globally

popDensityList <- list()
paletteList <- list()
macsToLocList <- list()

end.times <- rep(end.time, length(timeSteps))

colorPal <- "Purples"

for(i in 1:length(timeSteps)){
  timeStep <- timeSteps[i]
  # Bin populations, calculate densities at each timestep, and cache for future plotting
  time.windowStart = start.time # time.window for selection
  populationDensities <- NULL
  macsToLoc <- NULL

  while(end.time > time.windowStart){

    # Filter for time interval
    selInt = interval(time.windowStart, time.windowStart + timeStep)
    thisStep <- df %>%
      filter(`_time` %within% selInt)

    # Calculate Population Densities
    locationBinnedPop <- data.frame("location" = coord$location, "pop" = c(0))
    # For each location, count the number of unique devices (MAC addresses) that are present during the time time.window.
    locationBinnedPop$pop <- sapply(locationBinnedPop$location, function(x) {length(unique(thisStep$macaddr[thisStep$`location.y` == x]))})

    # Calculate a measure of people / (100 sq meters)
    densities_area  <- sapply(1:N, function(x) {100 * locationBinnedPop$pop[x] / (SPDF@polygons[[x]]@area * areaConvert)})
    densities_aps   <- sapply(1:N, function(x) {locationBinnedPop$pop[x] / coord$num[x]})
    densities_both  <- sapply(1:N, function(x) {locationBinnedPop$pop[x] / SPDF@polygons[[x]]@area / areaConvert / coord$num[x]})
    info <- c(densities_area, densities_aps, densities_both, locationBinnedPop$pop)
    type <- c(rep(1, N), rep(2, N), rep(3, N), rep(4, N))
    densitiesToSave <- data.frame("location" = locationBinnedPop$location,
                                  #"pop" = locationBinnedPop$pop,
                                  "ap_num" = coord$num,
                                  "info" = info,
                                  "type" = type,
                                  "time.window" = c(time.windowStart))
    populationDensities <- rbind(populationDensities, densitiesToSave)

    # For each macaddr, keep track of where it currently is
    macs <- data.frame("macaddr" = thisStep$macaddr,
                       "location" = thisStep$location.y,
                       "campus" = thisStep$campus,
                       "long" = thisStep$long,
                       "lat" = thisStep$lat,
                       "time.window" = c(time.windowStart),
                       "realTime" = c(thisStep$`_time`))
    macs <- macs[order(macs$realTime), ]
    macsToLoc <- rbind(macsToLoc, macs)

    end.times[i] <- time.windowStart
    time.windowStart = time.windowStart + timeStep
  }

  # setting up for chloropleth
  palette_area <- colorNumeric(colorPal, (populationDensities %>% filter(type == 1))$info)
  palette_aps  <- colorNumeric(colorPal, (populationDensities %>% filter(type == 2))$info)
  palette_both <- colorNumeric(colorPal, (populationDensities %>% filter(type == 3))$info)
  palette_raw  <- colorNumeric(colorPal, (populationDensities %>% filter(type == 4))$info)
  palette_area_log <- colorNumeric(colorPal, log((populationDensities %>% filter(type == 1))$info+1))
  palette_aps_log  <- colorNumeric(colorPal, log((populationDensities %>% filter(type == 2))$info+1))
  palette_both_log <- colorNumeric(colorPal, log((populationDensities %>% filter(type == 3))$info+1))
  palette_raw_log  <- colorNumeric(colorPal, log((populationDensities %>% filter(type == 4))$info+1))

  thisStepPaletteList <- list(palette_area, palette_aps, palette_both, palette_raw,
                              palette_area_log, palette_aps_log, palette_both_log, palette_raw_log)

  # Cache these guys away for later
  popDensityList[[i]] <- populationDensities
  paletteList[[i]] <- thisStepPaletteList
  macsToLocList[[i]] <- macsToLoc
}

legendTitles <- c("Population Density (area)",
                  "Population Density (aps)",
                  "Population Density (both)",
                  "Population (raw count)")
###################

# For Perkins Map
###################
# Global Vars
wallsdf <- apsdf <- eventsdf <- voronoiSPDF <- floors <- offsets <- plotsP <- plotsM <- labelLocations <- NULL
timeStepsPerk <- c("30 min" = 1800, "1 hr" = 3600, "2 hr" = 7200)
ani.options(convert = "C:/Autodesk/ImageMagick-7.0.8-Q16/convert")
###################

# For Plots
###################
mainSSID <- c("Dukeblue", "DukeVisitor", "DukeOpen", "eduroam") # most "important" ssids
dorms <- c("Edens", "Wannamaker", "Kilgo", "Few", "Keohane", "CrowellQuad", "Craven",
           "Jarvis", "Trinity", "Giles", "Wilson", "Brown", "Alspaugh", "Pegram", 
           "EastHouse", "Randolph", "Epworth", "Southgate", "GilbertAddoms", "Bassett")
# For SSID usage by campus
dfssid <- df %>% # seeing if ssid events change by campus
  group_by(ssid, campus) %>% 
  summarise(n = n())
dfssidlocs <- df %>% # seeing if ssid events change by locations
  group_by(ssid, location.y, campus) %>%
  summarise(n = n())
dfVO <- dfssidlocs %>% # seeing if ssid events change by off campus locations
  filter(campus == "Off")

# For top 5 APs by location
dfap <- df %>% # seeing top events by location by ap
  group_by(ap, location.y, campus) %>%
  summarise(n = n())
dfaptrun <- dfap %>%
  group_by(location.y) %>%
  top_n(n=5, wt = n)
locs <- dfaptrun %>% 
  group_by(location.y) %>% 
  summarise(n = n())
locs <- locs[locs$n == 5, ]
dfaptrun <- dfaptrun[dfaptrun$location.y %in% locs$location.y, ] # getting rid of locations that don't have at least 5 aps
dfaptrun <- dfaptrun[order(dfaptrun$n,dfaptrun$location.y),]
dfaptrun$ap <- factor(dfaptrun$ap, levels = dfaptrun$ap[order(dfaptrun$n)]) # reordering factors for nicer looking graph

# For slotnum usage by campus
dfapslots <- df %>% 
  group_by(ap, slotnum, campus, location.y) %>% 
  summarise(n = n()) 

# Dukeblue / DukeVisitor ratio
dfblue <- dfssidlocs %>% 
  filter(ssid == "Dukeblue")
dfvisit <- dfssidlocs %>% 
  filter(ssid == "DukeVisitor")
dfboth <- merge.data.frame(dfblue, dfvisit, by = c("location.y", "campus"))
dfboth <- dfboth %>% 
  mutate(ratio = n.x / n.y) # num of dukeblue events / num of dukevisitor
dfboth$location.y <- factor(dfboth$location.y, levels = dfboth$location.y[order(dfboth$ratio)]) # reorder graph

# For slotnum usage by campus
dfapslotsno2 <- dfapslots[dfapslots$slotnum != 2, ] # filtering out slotnum 2 because it's much rarer
dfapslotsno2 <- merge.data.frame(dfapslotsno2[dfapslotsno2$slotnum == 0, ], 
                                 dfapslotsno2[dfapslotsno2$slotnum == 1, ],
                                 by = "ap", all = TRUE)
dfapslotsno2 <- dfapslotsno2 %>% # view the ratio of slot 0 / slot 1 events
  mutate(ratio = n.x / n.y) %>% 
  dplyr::select(ap, slotnum.x, campus.x, location.y.x, n.x, slotnum.y, n.y, ratio)
dfapslotsno2$n.x[is.na(dfapslotsno2$n.x) ] <- 0
dfapslotsno2$n.y[is.na(dfapslotsno2$n.y)] <- 0
isDorm <- lapply(dfapslotsno2$location.y.x, function(x) { # labeling if location is dorm
  if(x %in% dorms) {
    return("dorm")
  } else {
    return("notdorm")
  }
})
dfapslotsno2$isDorm <- isDorm
###################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage("Data+ Map Demo",
             tabPanel("Public Campus Map",
                      imageOutput("pubmap")
             ),
             tabPanel("Internal Campus Map",
                      
                      titlePanel("Duke Wireless Data"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          # input a time to show temporally close records on map
                          selectInput("timeStepSelection", "Time Step", choices = timeSteps, selected = timeSteps[1]),
                          uiOutput("ui"),
                          selectInput("select", "View:", choices = c("Population Density (area)" = 1, "Population Density (aps)" = 2, 
                                                                     "Population Density (both)" = 3, "Population (raw)" = 4), selected = 1),
                          radioButtons("focus", "Zoom View", choices = c("All", "East", "Central", "West"), selected = "All"),
                          checkboxInput("log", "Log Scale", value = FALSE),
                          checkboxInput("flow", "Track flow", value = FALSE),
                          conditionalPanel( 
                            condition = "input.flow",
                            checkboxInput("removeEW", "Remove Cross Campus Lines", value = FALSE),
                            checkboxInput("cluster", "Enable clustering", value = FALSE)
                          ),
                          checkboxInput("track", "Track Single Macaddr", value = FALSE),
                          conditionalPanel( 
                            condition = "input.track",
                            textInput("mac", "Track", value = "00:b3:62:16:56:05"),
                            actionButton("submitMac", "Submit"),
                            p(),
                            htmlOutput("canTrack")
                          )
                        ),
                        
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Map", leafletOutput("map", height = 850)), # output the map, should check if height is ok with different screens
                                      tabPanel("Table", dataTableOutput("genT")),
                                      tabPanel("Mac Table", dataTableOutput("macT"))
                          )
                        )
                      )
             ),
             tabPanel("Perkins Map", 
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
                                                          choices = names(timeStepsPerk)),
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
             ),
             tabPanel("Plots",
                      plotOutput("ssidcampus", height = 800),
                      plotOutput("ssidofflocs", height = 800),
                      plotOutput("bluevisit", height = 800),
                      plotOutput("slotnumcampus", height = 800),
                      plotOutput("slotnumcampusdorms", height = 800),
                      plotOutput("topap", width = 3956, height = 2100))
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # For Public Campus Map
  ##################
  output$pubmap <- renderImage({
  # This filtering here does not exist in actual public map. It is just here to save runtime.
  populationDensities <- popDensityList[[1]]
  thisStep <- populationDensities %>%
    filter(time.window == max(populationDensities$time.window)) %>% 
    filter(type == 1)
  myPaletteList <- paletteList[[1]]
  myPalette <- myPaletteList[[1]]
  
  # A temp file to save output
  outfile <- tempfile(fileext = ".png")
  
  # Getting the marker color depending on its population density
  getColor <- function(step) {
    sapply(step$info, function(val) {
      if(val < summary(step$info)[[2]]) {
        "green"
      } else if(val > summary(step$info)[[5]]) {
        "red" 
      } else {
        "orange"
      } 
    })
  }
  # Icon image
  icons <- awesomeIcons(
    icon = 'wifi',
    iconColor = 'black',
    library = 'fa',
    markerColor = getColor(thisStep)
  )
  
  # Creating map
  m <- leaflet() %>%
    setView("pubmap", lng = defLong, lat = defLati, zoom = zm) %>% # sets initial map zoom & center
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>% 
    addPolygons(data = SPDF, 
                layerId = thisStep$location,
                weight = 1,
                color = "black",
                fillOpacity = 0.5,
                fillColor = ~myPalette(thisStep$info)) %>% 
    addAwesomeMarkers(lng = coord$long, 
                      lat = coord$lat, 
                      icon = icons,
                      options = markerOptions())
  
  # Save to temp file
  mapshot(m, file = outfile)
  
  list(src = outfile,
       contentType = 'image/png',
       width = 850,
       height = 637)
  
  }, deleteFile = TRUE)
  ##################
  
  # For Internal Campus Map
  ##################
  
  include <- reactiveValues(poly = coord$location, # list of locations to be included
                            singleMac = "00:b3:62:16:56:05", # default macaddr to track
                            removeEW = FALSE) 
  
  # Colors for movement lines
  from <- 'red'
  to <- 'blue'
  stationary <- 'blue'
  highlight <- 'black'
  
  borderInclude <- 'black'
  borderUninclude <- 'white'
  
  flowOp <- 0.3
  trackOp <- 0.5
  polyOp <- 0.5
  
  # Seeing if something was clicked and acting as needed
  observeEvent(input$map_shape_click, {
    clickedGroup <- input$map_shape_click$'group'
    clickedID <- input$map_shape_click$'id'
    if(clickedGroup == "shapes") { # Polygon clicked
      if(clickedID %in% include$poly) {
        include$poly <- include$poly[!include$poly %in% clickedID] # taking out of included polygons
      }
      else {
        include$poly <- c(include$poly, clickedID) # putting it back into included polygons
        include$poly <- sort(include$poly) # need to sort so that shapes drawn have correct id
      }
    }
    else if(clickedGroup == "severals" | clickedGroup == "singles") { # Line clicked
      macLocs <- currMacs %>% 
        filter(macaddr == clickedID) %>%
        filter(time.window == input$time)
      macLocs <- macLocs[order(macLocs$realTime), ]
      output$macT <- renderDataTable(macLocs) # view its visited locations
    }
  })
  
  observeEvent(input$submitMac, { # changing which macaddr to track
    include$singleMac <- input$mac 
    if(include$singleMac %in% currMacs$macaddr) {
      output$canTrack <- renderUI({
        HTML(paste0(unique((currMacs %>% 
                              filter(macaddr == include$singleMac))$time.window), sep = '<br/>'))
      })
    } else {
      output$canTrack <- renderText("Macaddr not found. Note that macaddr must be lowercase.")
    }
  })
  
  # Creates the initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView("map", lng = defLong, lat = defLati, zoom = zm) %>% # sets initial map zoom & center
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) # adds Open Street Map info (otherwise just a gray box)
  })
  
  output$ui <- renderUI({
    # input a time to show temporally close records on map
    sliderInput("time", "Time", min = start.time, max = end.times[which(timeSteps == input$timeStepSelection)],
                value = start.time, animate = animationOptions(interval=delay),
                step = dseconds(input$timeStepSelection))
  })
  
  observe({
    #Filters for records within timeStep of the input time.
    if(is.null(input$time) | is.null(input$timeStepSelection)){
      return()
    }
    populationDensities <- popDensityList[[which(timeSteps == input$timeStepSelection)]]
    currMacs <<- macsToLocList[[which(timeSteps == input$timeStepSelection)]]
    if(!any(populationDensities$time.window == input$time)){
      return()
    }
    
    thisStep <- populationDensities %>%
      filter(time.window == input$time) %>% 
      filter(location %in% include$poly) %>%
      filter(type == as.numeric(input$select))
    
    myPaletteList <- paletteList[[unname(which(timeSteps == input$timeStepSelection))]]
    myPalette <- myPaletteList[[as.numeric(input$select)]]
    if(input$log){
      thisStep$info <- log(thisStep$info + 1)
      myPalette <- myPaletteList[[as.numeric(input$select) + length(myPaletteList)/2]]
    }
    # Setting up for hover tooltips
    labels <- sprintf("<strong>%s</strong><br/ >%g APs<br/ >%g value",
                      thisStep$location, # location
                      thisStep$ap_num,
                      thisStep$info) %>% # plotted value
      lapply(htmltools::HTML)
    
    # Adds polygons and colors by population density.
    leafletProxy("map") %>%
      clearGroup("shapes") %>%
      clearControls() %>%
      addPolygons(data = SPDF[SPDF@data$ID %in% thisStep$location, ], # draws the included polygons
                  layerId = thisStep$location,
                  group = "shapes",
                  weight = 1,
                  color = borderInclude,
                  fillOpacity = polyOp,
                  fillColor = ~myPalette(thisStep$info),
                  label = labels)
    leafletProxy("map") %>%
      addPolygons(data = SPDF[!SPDF@data$ID %in% thisStep$location, ], # draws the unincluded polygons
                  layerId = coord$location[!coord$location %in% thisStep$location],
                  group = "shapes",
                  weight = 1.5,
                  color = borderUninclude,
                  opacity = 0.2,
                  fillOpacity = 0)
    legendVals <- (populationDensities %>% filter(type == as.numeric(input$select)))$info
    if(input$log){
      legendVals <- log(1 + legendVals)
    }
    leafletProxy("map") %>%
      addLegend(pal = myPalette, 
                values = legendVals,
                position = "topright",
                title = legendTitles[as.numeric(input$select)])
    
  })
  
  # Visualizing the flow of people
  observe({
    
    observeEvent(input$flow, { # sampling num amount of macaddrs
      uniqMacs <<- unique(currMacs$macaddr)
      uniqMacs <<- as.character(uniqMacs)
      uniqMacs <<- sample(uniqMacs, num)
    }, ignoreInit = TRUE, priority = 1)
    
    include$removeEW <- input$removeEW
    
    observeEvent({input$time}, {}, priority = -1) # here just to trigger this observe; there probably exists a better way to do this
    
    if(input$flow) {
      noMove <- NULL
      
      leafletProxy("map") %>% 
        clearGroup("severals") %>% 
        clearGroup("cluster")
      
      withProgress(message = "Loading...", {
        # looping through each macaddr to determine its movement
        for(i in 1:length(uniqMacs)) { 
          if(i == num) { # to prevent stuff from crashing
            break
          }
          incProgress(amount = 1/(num+100))
          # filtering to find each location a macaddr has visited
          macsTime <- currMacs %>% 
            filter(macaddr == uniqMacs[[i]]) %>% 
            filter(time.window == input$time)
          macLocs <- macsTime %>% # this is to determine whether a macaddr has moved at all
            group_by(location) %>%
            summarise(num = n())
          
          macLabels <- sprintf("macaddr: %s",
                               macsTime$macaddr) %>%
            lapply(htmltools::HTML)
          
          if(length(macLocs$location) == 1) { # stores the macaddrs in a dataframe so that they can be clustered later
            noMoveMacs <- data.frame(long = macsTime$long[[1]],
                                     lat = macsTime$lat[[1]],
                                     macaddr = macsTime$macaddr[[1]],
                                     location = macsTime$location[[1]])
            noMove <- rbind(noMove, noMoveMacs)
          } else if(length(macsTime$location) != 0) { # drawing movement lines
            # drawing "broken" paths by checking if one of the locations to be not included was visited by the mac
            # or the mac went to a different campus. Note that nothing will be drawn if the mac only went to one location on another campus.
            if((TRUE %in% (!macsTime$location %in% include$poly)) | (length(unique(macsTime$campus)) > 1 & include$removeEW)   ) {
              n <- 1 # unique index tied to groups of locations
              i <- 1 # row index
              
              # function that adds a column that has numbers that will increment when a value before it changes
              if(TRUE %in% (!macsTime$location %in% include$poly)) {
                col <- rleid(macsTime$location)
              } else {
                col <- rleid(macsTime$campus)
              }
              macsTime$id <- col
              splitPaths <- split(macsTime, f = macsTime$id) # list of individual paths that resulted from breaking it up into pieces
              allLines <- NULL
              if(length(splitPaths) == 0) {
                next
              }
              for(j in 1:length(splitPaths)) { # making lines and putting them into a SLDF in order to be associated with the correct macaddr
                l <- Line(subset(splitPaths[[j]], select=c("long", "lat")))
                allLines <- c(allLines, l)
              }
              l2 <- Lines(allLines, ID = macsTime$macaddr[[1]])
              SL <- SpatialLines(c(l2))
              
              leafletProxy("map") %>%
                addPolylines(data = SL,
                             layerId = macsTime$macaddr,
                             group = "severals",
                             weight = 2,
                             opacity = flowOp,
                             label = macLabels,
                             highlightOptions = highlightOptions(
                               weight = 5,
                               color = highlight,
                               fillOpacity = 1,
                               bringToFront = TRUE))
              next
            }
            
            leafletProxy("map") %>% 
              addPolylines(lng = macsTime$long, 
                           lat = macsTime$lat,
                           layerId = macsTime$macaddr,
                           group = "severals",
                           weight = 2,
                           opacity = flowOp,
                           label = macLabels,
                           color = to,
                           highlightOptions = highlightOptions(
                             weight = 5,
                             color = highlight,
                             fillOpacity = 1,
                             bringToFront = TRUE)) %>% 
              addCircles(lng = macsTime$long[1], # red is from
                         lat = macsTime$lat[1],
                         group = "severals",
                         weight = 2,
                         opacity = flowOp,
                         color = from) %>% 
              addCircles(lng = macsTime$long[length(macsTime$long)], # blue is to
                         lat = macsTime$lat[length(macsTime$lat)],
                         group = "severals",
                         weight = 2,
                         opacity = flowOp,
                         color = to)
          }
        } 
        
        # Controls to enable clustering of people who don't move during the given timeframe
        if(input$cluster) {
          leafletProxy("map") %>% 
            addCircleMarkers(lng = noMove$long,
                             lat = noMove$lat,
                             group = "cluster",
                             color = stationary,
                             radius = 5,
                             weight = 2,
                             opacity = flowOp,
                             clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = FALSE,
                                                                   maxClusterRadius = 10))
        } else {
          leafletProxy("map") %>% 
            addCircles(lng = noMove$long,
                       lat = noMove$lat,
                       group = "cluster",
                       color = stationary,
                       radius = 5,
                       weight = 2,
                       opacity = flowOp)
        }
      })
    }
  })
  
  # Tracking a single macaddr
  observe({
    if(input$track) {
      leafletProxy("map") %>%
        clearGroup("singles")
      
      macTime <- currMacs %>% 
        filter(macaddr == include$singleMac) %>% 
        filter(time.window == input$time) %>% 
        filter(location %in% include$poly) 
      macLocs <- macTime %>% 
        group_by(location) %>%
        summarise(num = n())
      
      # draws a circle if there is no movement
      if(length(macLocs$location) == 1) { 
        leafletProxy("map") %>% 
          addCircles(lng = macTime$long,
                     lat = macTime$lat,
                     group = "singles",
                     radius = 3,
                     weight = 2,
                     opacity = trackOp,
                     color = stationary)
        # drawing movement lines
      } else if(length(macTime$location) != 0) { 
        leafletProxy("map") %>% 
          addPolylines(lng = macTime$long, 
                       lat = macTime$lat,
                       layerId = include$singleMac,
                       group = "singles",
                       weight = 5,
                       opacity = trackOp,
                       color = to,
                       highlightOptions = highlightOptions(
                         weight = 8,
                         color = highlight,
                         fillOpacity = 1,
                         bringToFront = TRUE)) %>% 
          addCircles(lng = macTime$long[1], # from
                     lat = macTime$lat[1],
                     group = "singles",
                     weight = 2,
                     opacity = trackOp,
                     color = from) %>% 
          addCircles(lng = macTime$long[length(macTime$long)], # to
                     lat = macTime$lat[length(macTime$lat)],
                     group = "singles",
                     weight = 2,
                     opacity = trackOp,
                     color = to)  
      }
    }
  })
  
  # Clearing the map of various elements when they are selected/deselected
  observeEvent({input$flow; input$time}, priority = 1, ignoreInit = TRUE, 
               handlerExpr =  {
                 leafletProxy("map") %>% 
                   clearGroup("severals") %>% 
                   clearGroup("cluster")
               })
  observeEvent({input$cluster; input$time}, priority = 1, ignoreInit = TRUE, 
               handlerExpr =  {
                 leafletProxy("map") %>% 
                   clearGroup("cluster")
               })
  observeEvent({input$track; input$time}, priority = 1, ignoreInit = TRUE, 
               handlerExpr =  {
                 leafletProxy("map") %>% 
                   clearGroup("singles")
               })
  
  observe({
    if(input$focus == "West"){
      leafletProxy("map") %>% 
        flyTo(wLong, wLati, wzm)
    }
    if(input$focus == "East"){
      leafletProxy("map") %>% 
        flyTo(eLong, eLati, ezm)
    }
    if(input$focus == "Central"){
      leafletProxy("map") %>% 
        flyTo(cLong, cLati, czm)
    }
    if(input$focus == "All"){
      leafletProxy("map") %>% 
        flyTo(defLong, defLati, zm)
    }
  })
  
  output$genT <- renderDataTable(currMacs %>% filter(time.window == input$time))
  ##################
  
  # For Perkins Map
  ##################
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
    
    plotsP <<- lapply(timeStepsPerk, function(delta){
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
    
    plotsM <<- lapply(timeStepsPerk, function(delta){
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
    
    names(plotsP) <- names(timeStepsPerk)
    hideTab("tabs", "File Upload")
    hideTab("tabs", "Confirm Upload")
  }))
  
  observeEvent(input$generateGIF, {
    ani.options(interval = input$frameDelay / 1000)
    fileName <- "timeLapse.gif"
    bins <- seq(min(eventsdf$`_time`), max(eventsdf$`_time`), timeStepsPerk[input$stepSize])
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
  ##################
  
  # For Plots
  ##################
  output$ssidcampus <- renderPlot(ggplot(data = dfssid %>% 
                                           filter(ssid %in% mainSSID), aes(x=ssid,y=n)) + 
                                    geom_col(aes(fill = campus)) +
                                    labs(title = "SSID Usage by Campus", y = "Num of Events") + 
                                    facet_wrap(.~campus, scales = "free_y") + # notice that y scale is slightly different for each campus
                                    theme_bw())
  output$ssidofflocs <- renderPlot(ggplot(data = dfVO %>%
                                            filter(ssid %in% mainSSID), aes(x=ssid, y= n)) + 
                                     geom_col(aes(fill = location.y)) +
                                     facet_wrap(.~location.y, scales = "free_y") + # notice that y scale is slightly different for each location
                                     labs(title = "SSID Usage by Off Campus Locations", y = "Num of Events") +
                                     theme_bw())
  output$bluevisit <- renderPlot(ggplot(dfboth, aes(x = location.y, y = ratio)) +
                                   geom_col(aes(fill = campus)) +
                                   coord_flip() +
                                   geom_hline(yintercept = 1) +
                                   scale_y_continuous(breaks = sort(c(seq(0, as.integer(max(dfboth$ratio)), length.out = 5), 1))) + 
                                   theme_bw() +
                                   facet_wrap(.~campus, scales = "free") + # this splits the graph into four 
                                   labs(y = "Dukeblue / DukeVisitor events", 
                                        subtitle = "Higher ratio implies more Dukeblue events than DukeVisitor"))
  output$slotnumcampus <- renderPlot(ggplot(data = dfapslotsno2, aes(x=n.x, y= n.y)) + 
                                       geom_point(aes(color = campus.x)) + 
                                       labs(x="Num of Slot 0 Events", y= "Num of Slot 1 Events", title = "Comparing Slot 0 and Slot 1 Events by Campus") + 
                                       coord_fixed() + 
                                       facet_wrap(.~campus.x) + 
                                       theme_bw())
  output$slotnumcampusdorms <- renderPlot(ggplot(dfapslotsno2, aes(x=n.x, y=n.y)) + geom_point(aes(color = campus.x)) + 
                                            labs(x="Num of Slot 0 Events", y= "Num of Slot 1 Events", title = "Separating by Dorms") + 
                                            coord_fixed() + 
                                            facet_wrap(.~as.character(isDorm)) + 
                                            theme_bw())
  output$topap <- renderPlot(ggplot(dfaptrun, aes(x=ap,y=n)) + 
                               geom_col(aes(fill=location.y)) + 
                               labs(title = "Top 5 Events of Each Location by AP", y = "Num of Events") +
                               theme_bw() + 
                               coord_flip() + 
                               facet_wrap(.~location.y, scales = "free") + # notice differing scales
                               theme(legend.position = "bottom"))
  ##################
  
}

# Run the application 
shinyApp(ui = ui, server = server)

