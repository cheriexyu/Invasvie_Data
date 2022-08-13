#1. Filter IUCN range data with global habitat map to determine suitability habitat for mammals

install.packages("gtools")
library(gtools)
library(raster)
library(data.table)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(rgdal)
library(sf) #done
library(dplyr)
library(stringr)

alien<-fread("../data/DAMA_table.csv") #DAMA Invasive Mammals Database
DAMA_names<-unique(alien$Binomial) #230 invasive mammals from DAMA

disease<-fread("../data/trefle.csv") #Disease hosts froom VIRION
colnames(disease)[2]<-"Binomial"
dataframe<-inner_join(disease, alien) #join alien mammals and viruses they have 
#Total 438850 rows 
write.csv(dataframe,"../results/DAMA_pathogen.csv", row.names=F)




#3. Use Species Habitat Preference Information - Level 2
# edit habitat csv, replace all habitats with numerical value 
number<-c()
no<-seq.int(100,109)
number<-c(number,no)
no<-c(200,201,202)
number<-c(number,no)
no<-seq.int(300,308)
number<-c(number,no)
no<-seq.int(400,407)
number<-c(number,no)
no<-seq.int(500,518)
number<-c(number,no)
no<-c(600,800:803,900,901,908,909)
number<-c(number,no)
no<-seq.int(1000,1004)
number<-c(number,no)
no<-seq.int(1100,1106)
number<-c(number,no)
no<-c(1200,1206,1207,1400:1405)
number<-c(number,no)

file<-list.files(path = "../data/terrestrialhabitat_raster/", pattern = NULL, all.files = FALSE)
file<-mixedsort(sort(file))
for (x in 1:79){ #change base habitat raster to value of 1 and na 
  print(x)
  raster_lvl2<-raster(paste0("../data/terrestrialhabitat_raster/",file[x]))
  raster_lvl2[!is.na(raster_lvl2)]<-1
  writeRaster(raster_lvl2,paste0("../data/terrestrialhabitat_raster/",number[x],".tif"),options=c('TFW=YES'))
  removeTmpFiles(h=0)
}

habitat<-fread("../data/habitat_both.csv") #habitat preference csv 
marginal<-habitat %>% select(Species,Level_1_Habitat_Class,Level_2_Habitat_Class,Suitable) %>% filter(Suitable == "Marginal")
suitable<-habitat %>% select(Species,Level_1_Habitat_Class,Level_2_Habitat_Class,Suitable) %>% filter(Suitable == "Suitable")

habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class == "Rocky areas (eginland cliffs, mountain peaks)"] <- 600
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Forest - Boreal"] <- 101
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Forest - Subarctic"] <- 102
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class=="Forest - Subantarctic"] <- 103
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Forest - Temperate"] <- 104
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Forest - Subtropical/Tropical Dry"] <- 105
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Forest - Subtropical/Tropical Moist Lowland"] <- 106
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Forest - Subtropical/Tropical Mangrove Vegetation Above High Tide Level"] <- 107
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Forest - Subtropical/Tropical Swamp"] <- 108
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Forest - Subtropical/Tropical Moist Montane"] <- 109
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Oceanic - Epipelagic (-m)"] <- 1001
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Oceanic - Mesopelagic (-m)"] <- 1002
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Oceanic - Bathypelagic (-m)"] <- 1003
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Intertidal - Rocky Shoreline"] <- 1200
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Intertidal - Sandy Shoreline and/or Beaches, Sand Bars, Spits, Etc"] <- 1200
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Intertidal - Shingle and/or Pebble Shoreline and/or Beaches"] <- 1200
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Intertidal - Mud Flats and Salt Flats"] <- 1200
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Intertidal - Salt Marshes (Emergent Grasses)"] <- 1200
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Intertidal - Tidepools"] <- 1200
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Intertidal - Mangrove Submerged Roots"] <- 1200
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Artificial/Terrestrial - Arable Land"] <- 1401
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Artificial/Terrestrial - Pastureland"] <- 1402
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Artificial/Terrestrial - Plantations"] <- 1403
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Artificial/Terrestrial - Rural Gardens"] <- 1404
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Artificial/Terrestrial - Urban Areas"] <- 1405
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Savanna - Dry"] <- 201
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Savanna - Moist"] <- 202
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Shrubland - Subarctic"] <- 301
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Shrubland - Subantarctic"] <- 302
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Shrubland - Boreal"] <- 303
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Shrubland - Temperate"] <- 304
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Shrubland - Subtropical/Tropical Dry"] <- 305
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Shrubland - Subtropical/Tropical Moist"] <- 306
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Shrubland - Subtropical/Tropical High Altitude"] <- 307
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Shrubland - Mediterranean-type Shrubby Vegetation"] <- 308
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Grassland - Tundra"] <- 401
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Grassland - Subarctic"] <- 402
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Grassland - Subantarctic"] <- 403
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Grassland - Temperate"] <- 404
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Grassland - Subtropical/Tropical Dry"] <- 405
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Grassland - Subtropical/Tropical Seasonally Wet/Flooded"] <- 406
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Grassland - Subtropical/Tropical High Altitude"] <- 407
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Permanent Rivers/Streams/Creeks (includes waterfalls)"] <- 501
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Tundra Wetlands (inclpools and temporary waters from snowmelt)"] <- 510
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Alpine Wetlands (includes temporary waters from snowmelt)"] <- 511
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Geothermal Wetlands"] <- 512
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Permanent Inland Deltas"] <- 513
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Permanent Saline, Brackish or Alkaline Lakes"] <- 514
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Seasonal/Intermittent Saline, Brackish or Alkaline Lakes and Flats"] <- 515
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Seasonal/Intermittent Saline, Brackish or Alkaline Marshes/Pools"] <- 517
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Permanent Saline, Brackish or Alkaline Marshes/Pools"] <- 516
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Karst and Other Subterranean Hydrological Systems (inland)"] <- 518
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Seasonal/Intermittent/Irregular Rivers/Streams/Creeks"] <- 502
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Shrub Dominated Wetlands"] <- 503
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Bogs, Marshes, Swamps, Fens, Peatlands"] <- 504
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Permanent Freshwater Lakes (over ha)"] <- 505
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Seasonal/Intermittent Freshwater Lakes (over ha)"] <- 506
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Permanent Freshwater Marshes/Pools (under ha)"] <- 507
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Seasonal/Intermittent Freshwater Marshes/Pools (under ha)"] <- 508
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Wetlands (inland) - Freshwater Springs and Oases"] <- 509
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Desert - Hot"] <- 801
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Desert - Temperate"] <- 802
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Desert - Cold"] <- 803
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Neritic - Pelagic"] <- 901
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Neritic - Coral Reef"] <- 908
habitat$Level_2_Habitat_Class[habitat$Level_2_Habitat_Class =="Marine Neritic - Seagrass (Submerged)"] <- 909
write.csv(habitat,'../results/habitat_both_edit.csv')

habitat<-fread("../results/habitat_both_edit.csv")
for (x in 1:245){ #create a directory with a csv for each species documenting habitat preference, habitat in number
  print(x)
  out <- habitat %>% select(Species,Level_1_Habitat_Class,Level_2_Habitat_Class,Suitable) %>% filter(Species == DAMA_names[x])
  write.csv(out, paste0("../results/DAMA_habitat/habitat_", DAMA_names[x],".csv"))
}

