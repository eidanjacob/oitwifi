### 

Single-Building, Public-Facing Wifi Map Visualizer

###

This application generates a real-time representation of the utilization of different wireless access points within a building. 
Designed for quick use by students/faculty/etc looking for a nearby place with good WiFi connection.

###

Requirements:

R Packages:
shiny
readr
raster
sp
deldir
dplyr
rgeos
ggplot2
maps

In the same directory as the Application, three csv files must exist with the following (case sensitive) column names:

1) apData.csv
	- ap = name of access point
	- X = X coordinate of locations
	- Y = Y coordinate of locations
	- floor = Name of floor (will be coerced to character - numeric values ok)
2) buildingData.csv
	- X = X coordinate of points defining border of building
	- Y = Y coordinate of points defining border of building
	- floor = Name of floor (needs to match apData.csv)
3) eventData.csv
	- event data can be taken directly from Splunk
	- "ap" variable must match apData.csv and buildingData.csv

Extra columns will be ignored. Sample files for Perkins library are provided.

###

Tips for creating apData.csv and buildingData.csv:

If you already have floor-by-floor images of the building with its access points, you need to set a consistent coordinate system on the image.
Many online services can do this: To produce the maps for Perkins floors 0 to 3, I used https://www.mobilefish.com/services/record_mouse_coordinates/record_mouse_coordinates.php, but other tools may be more useful.
If you are using multiple floors, it's important that the coordinate system is consistent along all the floors. If the images are not the same shape or size this may not be the case. One solution is to choose a pair of 'reference' points on each image (say, the NW and SE corners of the building or of a room) and transform the coordinates you generate for each image based off of that. This is annoying but only needs to be done once!

Details on Transforming coordinates:

Refer to the images in Perkins_Sample_Images. The two floors are not on the same scale. Conveniently, they share a system of grid lines. As reference points we select the NW most grid point (on floor two, near 7704-perkins-233-c-12-3802i) and the grid point to the SE (on floor two, near 7704-perkins-217-c-15-3802i). The locations directly below those are easily identifiable on the first floor (near 7704-perkins-109-c-v13-3802i and 7704-perkins-102-v08-3802i, respectively).
Using our tool to identify coordinates of positions on the image, we record 1) the coordinates of the reference points, 2) the coordinates of the wireless access points, and 3) the corners of the walls (see annotated image: wall points are marked with 'W' and reference points are marked with 'R').
Use a linear regressions (one for X and one for Y) on each floor's reference points to fit them to a set of consistent coordinates. Then feed the coordinates of the APs and walls for each floor into its corresponding linear regression and use the resulting coordinates.
(Don't worry that the coordinates for different floors overlap - The tool will automatically move the floors so they are not displayed on top of each other.)

I've made another app to make this easier (see 'CoordinateTransformer'). Upload a csv of coordinates (needs 2 columns named X and Y, with the first two rows specifying the reference points' untransformed coordinates). Indicate the reference points' "true" coordinates and a file name for the output. The output is a copy of the input, replacing the coordinates with transformed values.

###

HOW TO USE: Run the application. When you wish to update the image, simply overwrite eventData.csv with more recent data.
Copy the directory for each building display you wish to create.

###

Last Update: 27 July 2018

Eidan Jacob (ej68@duke.edu)