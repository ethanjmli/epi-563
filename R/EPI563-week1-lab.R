library(tidyverse)
library(sf)
library(tmap)
library(here)
library(viridis)

GA.hwy.data <- st_read(here::here("Data","Week 1 Data","ga_hwy.gpkg"))
GA.mvc.data <- st_read(here::here("Data","Week 1 Data","ga_mvc.gpkg"))
GA.trauma.data <- st_read(here::here("Data","Week 1 Data","trauma_centers.gpkg"))

head(GA.hwy.data)
head(GA.mvc.data)
head(GA.trauma.data)

st_crs(GA.hwy.data) #4326
st_crs(GA.mvc.data) #4326
st_crs(GA.trauma.data) #4269

GA.mvc.data.aea <- st_transform(GA.mvc.data,5070)
GA.hwy.data.aea <-st_transform(GA.hwy.data,5070)
GA.trauma.data.aea <- st_transform(GA.trauma.data,5070)

tm_shape(GA.mvc.data.aea)+
  tm_fill(col = "MVCRATE_17",
          style="quantile",
          palette = "Blues",
          title = "MCV Rate 2017"
    
  )+
  tm_borders()


myLabels <- c("Low (Q1)","Q2","Q3","Q4","High (Q5)")

tm_shape(GA.mvc.data.aea)+
  tm_fill(col = "MVCRATE_17",
          style="quantile",
          palette = "Blues",
          title = "MCV Rate 2017",
          n=5,
          labels = myLabels
          
  )+
  tm_borders()+
tm_shape(GA.hwy.data.aea)+
  tm_lines(lwd=2,col="red")+
tm_shape(GA.trauma.data.aea)+
  tm_bubbles(shape = "LEVEL")+
tm_layout(title = "Motor Vehicle Crashes in Georgia, 2017",
          legend.outside=T,
          frame=F)+
tm_credits("Source: Georgia OASIS, retrieved 2019")


MVCRATE_17.map <-
  tm_shape(GA.mvc.data.aea)+
    tm_fill(col = "MVCRATE_17",
          style="quantile",
          palette = "Blues",
          title = "MCV Rate 2017"
          
    )+
    tm_borders()

MVCRATE_05.map <- 
  tm_shape(GA.mvc.data.aea)+
    tm_fill(col = "MVCRATE_05",
          style="quantile",
          palette = "Blues",
          title = "MCV Rate 2005"
          
    )+
    tm_borders()

tmap_arrange(MVCRATE_05.map,MVCRATE_17.map)    


####Week 1 Deliverable####
GA.covid.data <- st_read(here::here("Data","Week 1 Data","covid-ga.gpkg"))
head(GA.covid.data)
st_crs(GA.covid.data) #4269, unprojected

GA.covid.data.aea <- st_transform(GA.covid.data,5070) #Reproject covid data to AEA
GA.covid.data.aea <- GA.covid.data.aea %>% mutate(cfr = deaths/cases)

GA.covid.cfr.map <- tm_shape(GA.covid.data.aea)+
  tm_fill(col = "cfr",
          style = "cont",
          palette = (plasma(500)),
          title = "Case Fatality Rate")+
  tm_borders(col="black")+
tm_shape(GA.hwy.data.aea)+
  tm_lines(lwd=2,
           col="green")+
tm_compass()+
tm_scale_bar()+
tm_layout(legend.outside=T,
          title = "COVID Case Fatality Rate\nby County, Georgia",
          frame=F)

tmap_save(GA.covid.cfr.map, here::here("Documents","Week 1 Documents","Week 1 Deliverable Map.png"))
  

GA.cfr.median <- quantile(GA.covid.data.aea$cfr,probs=.5)
GA.covid.data.aea[which(GA.covid.data.aea$cfr ==GA.cfr.median),"county"] #Webster County

GA.cfr.max <- max(GA.covid.data.aea$cfr)
GA.covid.data.aea[which(GA.covid.data.aea$cfr ==GA.cfr.max),"county"] #Hancock




