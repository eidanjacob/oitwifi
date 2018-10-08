Wifi Weather Maps using R and Data from Splunk logging server
------

##### Use Case
------
This project was based off of a collaberation with Duke University OIT and it's students.
The base premise is to track down pathways for emergency service issues, and expantion of wifi coverage.
The largest complication with most networks is tracking and automation. When only 10% of all network teams use automation how is there a way to see if there is something really wrong?
Since all Cisco Equipment provides ns-logging as an option back to a server, a set of ns-logging servers was configured just to watch the overall trafficing and equipment issues. 
This lead to finding that the heaviest usage on the network (like most networks) was on the wifi.
Once the base research was done on the preliminary issues with regular usage that was consitered priority the deeper dive was created to take this a step further.


##### Required software
------
The basic needs for this project to work are listed below.

1. Ubuntu OS

2. Ubuntu Packages:

 + gdal
 + proj
 + geos
 + python & python-devel
 + udunits2 & udunits2-devel
 + cario-devel
 + openssl-devel
 + libpng-devel
 + libcurl-devel


3. Open source R-studio Server Software

4. R Packages:

 + shiny
 + readr
 + leaflet
 + tidyverse
 + rgeos
 + pacman
 + jpeg
 + devtools
 + grid
 + raster
 + deldir
 + dplyr
 + ggplot2
 + maps
 + sp
 + lubridate
 + animation
 + gganimate
Install with devtools::install_github('nteetor/gganimate')
 + RColorBrewer
 + tweenr

##### Instructions
------
