###################################################
# Application for Football Stadium + Surroundings #
###################################################

options(repos = "http://archive.linux.duke.edu/cran/")

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(readr)){
  install.packages("readr")
  library(readr)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
}
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}
if(!require(jpeg)){
  install.packages("jpeg")
  library(jpeg)
}
if(!require(grid)){
  install.packages("grid")
  library(grid)
}

# Global Variables
building_folder <- "wallaceWade" # Name of building data folder
height <- 600 # in px
txtscl <- 1 # Text scaling factor (reduce if text too large)
# apsdf <- read_csv(paste0("../data/",
#                          building_folder,
#                          "/apData.csv"))
reportFile = "reportFile.txt"
con <- file(reportFile, "w")
cat("Access Points in Building Data that are missing from Event Data\n", file = con)
close(con)
backImage <- readJPEG("../data/wallaceWade/backImage.jpg")

ui <- fluidPage(
  plotOutput("myPlot", height = height)
)

server <- function(input, output){
  
  observe({
    # # Count the number of events per access point. This is a crude measure of network load but it's good enough for students.
    # eventsdf <- read_csv(paste0("../data/",building_folder,"/eventData.csv"))
    # chartData <- summarise(group_by(eventsdf, ap), n())
    # 
    # # Are there access points with no events? If so, make a note in reportFile.
    # missing <- apsdf$ap[which( !(apsdf$ap %in% chartData$ap))]
    # if(length(missing) != 0){
    #   missdf <- data.frame(missing, 0)
    #   for(missingAP in missing){
    #     con <- file(reportFile, "a")
    #     cat(missingAP, as.character(Sys.time()), "\n", file = con, append = TRUE)
    #     close(con)
    #   }
    #   names(missdf) <- names(chartData)
    #   chartData <- rbind(chartData, missdf)
    # }
    
    # Code to generate plot
    output$myPlot <- renderPlot({
      ggplot() + 
        # Lock aspect ratio, add title, subtitle
        coord_fixed() + 
        ggtitle("WiFi Utilization in Wallace Wade", 
                subtitle = "Wireless access points in darker locations are experiencing greater activity.") + 
        # Set options for axis labels. Eventually we want to use them to help the user orient themselves on the map.
        theme(axis.title.x = element_text(face = "bold", size = 20 * txtscl, family = "serif"), 
              axis.text.x = element_blank(), axis.ticks.x = element_blank(),
              axis.title.y = element_text(face = "bold", size = 20 * txtscl, family = "serif"), 
              axis.text.y = element_blank(), axis.ticks.y = element_blank(),
              plot.title = element_text(size = 30 * txtscl, hjust = 0.5),
              plot.subtitle = element_text(size = 20 * txtscl, hjust = 0.5)) +
        annotation_custom(rasterGrob(backImage,
                                     width = unit(1, "npc"),
                                     height = unit(1, "npc")),
                          -Inf, Inf, -Inf, Inf)
    })
  })
}

shinyApp(ui = ui, server = server)