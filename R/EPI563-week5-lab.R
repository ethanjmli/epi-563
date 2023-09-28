pacman::p_load(tidyverse,  # general data management
               sf,         # spatial data classes
               #DCluster,   # Function achisq.test for homogeneity
               spdep,      # spatial analysis package supports probability maps
               #SpatialEpi, # spatial analysis package with Empirical Bayes
               tmap,
               spdep,
               here) 

ppor <- st_read(here::here("Data","Week 4 Data","ga_ppor.gpkg"))

queen_nb <- spdep::poly2nb(ppor, queen = TRUE) #Create Queen continuity object from ppor
summary(queen_nb) #Average number of links: 5.408805
                  #1 least connected region: 41 with 1 link
                  #2 most connected regions: 53 60 with 10 links
is.symmetric.nb(queen_nb) #Symmetric = True


#3.3 Creating Graph-based triangle neighbor objects
# Create a new sf object that holds the x,y coordinates of the centroids:
ppor_cent <- st_centroid(st_geometry(ppor))
