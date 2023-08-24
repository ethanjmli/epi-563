library(tidyverse)
library(sf)
library(tmap)
library(here)

GA.mvc.data <- st_read(here::here("Data","Week 0 Data","ga_mvc.gpkg"))

####Task 1####
st_geometry(GA.mvc.data) #get geometry of GA.mvc

#Multipolygon


####Task 2####
GA.mvc.data <- GA.mvc.data[order(GA.mvc.data$MVCDEATHS_05,decreasing=T),] #Order data by decreasing number of MVC deaths in 2005
max(GA.mvc.data$MVCDEATHS_05)
GA.mvc.data[GA.mvc.data$MVCDEATHS_05==105,"County"]


GA.mvc.data %>% slice_max(MVCDEATHS_05) #Tidyverse method

#Gwinnett County


####Task 3####
GA.mvc.data[GA.mvc.data$MVCRATE_17==max(GA.mvc.data$MVCRATE_17),"County"]

GA.mvc.data %>% slice_max(MVCRATE_17) #Tidyverse method

#Webster County


####Task 4####
GA.mvc.data.rural <- GA.mvc.data %>% filter(rural == "non-Rural")
table(GA.mvc.data.rural$rural)

GA.mvc.data %>% count(rural == "non-Rural")

table(GA.mvc.data$rural)

#102 counties are non-rural


####Task 5####
st_crs(GA.mvc.data)

#WGS 84


####Task 6####
st_drivers()$name
grep("ESRI",st_drivers()$long_name,ignore.case=T)

#71 formats