library(data.table)
library(rgdal)
library(units)
library(raster)
library(sf) #vector data
library(dplyr) 
library(stringr)
library(maptools)
library(fasterize)
library(terra)
data(wrld_simpl)

#1. Using information from IUCN and DAMA, can we combine these invasive databases to get a master list of invasive species

IUCN_range_poly <- st_read("../data/polygons/MAMMALS.shp") #range polygon from IUCN of all mammals 
IUCN_names<-IUCN_range_poly$binomial
IUCN_names<-unique(names) #5885

#How many invasive species are there in IUCN out of 5885 species?
introduced_vector<-c("Possibly Extant & Introduced (resident)","Probably Extant & Introduced (resident)", "Extant & Introduced (resident)"  
                     ,"Extinct & Introduced", "Possibly Extinct & Introduced")
extinct_vector<-c("Possibly Extinct","Presence Uncertain & Origin Uncertain","Presence Uncertain & Vagrant","Presence Uncertain",
                  "Extinct","Presence Uncertain & Assisted Colonisation","Extinct & Origin Uncertain")
dir.create("../results/IUCN_invasive_polygons/",showWarnings = TRUE, recursive = FALSE)

for ( a in 1:length(IUCN_names)){
  print(species)
  print(a)
  species <- IUCN_names[a] #species
  range_poly<-IUCN_range_poly %>% filter(binomial == species)
  legend <- range_poly$legend
  
  if (any(legend %in% introduced_vector) == T) { #if any is an invasive species 
    range_poly <- range_poly[! (range_poly$legend %in% extinct_vector), ] #delete extinct polygons
    st_write(range_poly, paste0("../results/IUCN_invasive_polygons/",species,".shp"))}
}

invasive_file<-list.files(path = "../results/IUCN_invasive_polygons/", pattern = "shp", all.files = FALSE) 
invasive_file<-tools::file_path_sans_ext(invasive_file) #111 invasives are found from IUCN website
#How many invasive species are there in DAMA? 230

#How many are missing from DAMA? 15 What are they? 
missing<-setdiff(invasive_file,DAMA_names) #15 are missing from DAMA

for (x in 1:length(DAMA_names)){ #230
  print(x)
  species <- DAMA_names[x] #species
  print(species)
  range_poly<-IUCN_range_poly %>% filter(binomial == species)
  range_poly <- range_poly[! (range_poly$legend %in% extinct_vector), ] #delete extinct polygons
  st_write(range_poly, paste0("../results/DAMA_polygons/polygon_", species,".shp"))
}

for (x in 1:length(missing)){ #230+15=245 # Add those missing from IUCN polygon into DAMA polygo
  print(x)
  species <- missing[x] #species
  print(species)
  range_poly<-IUCN_range_poly %>% filter(binomial == species)
  range_poly <- range_poly[! (range_poly$legend %in% extinct_vector), ] #delete extinct polygons
  st_write(range_poly, paste0("../results/DAMA_polygons/polygon_", species,".shp"))
}
DAMA_names <- append(DAMA_names,missing) #245  

#results in DAMA_polygon directory with all invasive species from IUCN and DAMA 

DAMA_polygon<-list.files(path = "../data/GIS_data/DAMA_polygons/", pattern = "shp", all.files = FALSE) #111 invasives 
#245

#2. Use Disease Interpoalted Database and split to invasive and noninvasive 

disease<-fread("../data/trefle.csv")
disease<-unique(disease$host) #1081

#DAMA_names<-There are 245 invasive species by combining DAMA (230) and IUCN (15) 
disease_invasive <- intersect(disease,DAMA_names) #interpolated database are invasive 

#144 disease hosts that are also invasive according to 
disease_non_invasive<-setdiff(disease,disease_invasive) #921 disease hosts that are not invasive

for (a in 1:length(disease_invasive)){
  dir.create(paste0("../results/all_disease_hosts/invasive/",disease_invasive[a]),showWarnings = TRUE, recursive = FALSE)
}

for (a in 1:length(disease_non_invasive)){
  dir.create(paste0("../results/all_disease_hosts/non-invasive/",disease_non_invasive[a]),showWarnings = TRUE, recursive = FALSE)
}

# 3. create global suitable habitats for all 

#########  invasive_diseases #########

#SUITABLE HABITAT CODE
habitat<-fread("../results/habitat_both_edit.csv")
for (y in 1:length(disease_invasive)){
  print(y)
  print(name)
  name <- disease_invasive[y]
  habitat<-fread(paste0("../results/DAMA_habitat/habitat_",name,".csv"),header=T)
  
  if (nrow(habitat)==0){ #if known species habitat dataframe is empty 
    next
  }
  
  habitat_suitable <- habitat %>% filter(Suitable == "Suitable") %>% select(Level_2_Habitat_Class)
  if (nrow(habitat_suitable)==0){ #if known species habitat dataframe is empty for suitability
    next
  }
  
  vector<-habitat_suitable$Level_2_Habitat_Class
  print(vector)
  dir.create(paste0("../results/all_disease_hosts/invasive/",name,"/"),showWarnings = TRUE, recursive = FALSE)
  
  combine_raster<-c()
  for (x in 1:length(vector)){ #101,102,140,150
    #print(x)
    #print(vector[x]) #101
    if (vector[x] %in% number == "TRUE"){ #if it is a number in the row = has a raster
      combine_raster<-c(combine_raster,paste0("../data/terrestrialhabitat_raster/",vector[x],".tif"))}
  }
  
  print("done")
  
  if ( length(combine_raster) > 1){
    load_raster<- lapply(combine_raster, raster)
    load_raster<- stack(load_raster)
    final_raster <- calc(load_raster, base::sum, na.rm=TRUE)
    final_raster[final_raster > 1] <- 1 #global suitable habitat is 1 
  }
  
  if ( length(combine_raster) == 1){
    final_raster<-raster(combine_raster)
  }#polygon is 1
  
  writeRaster(final_raster,paste0("../results/all_disease_hosts/invasive/",name,"/habitat_pref_",name,".tif"),options=c('TFW=YES'))
  removeTmpFiles(h=0)
}

#MARGINAL HABITAT CODE
for (y in 1:length(disease_invasive)){
  print(y)
  print(name)
  name <- disease_invasive[y]
  habitat<-fread(paste0("../results/DAMA_habitat/habitat_",name,".csv"),header=T)
  
  if (nrow(habitat)==0){ #if known species habitat dataframe is empty 
    next
  }
  
  #sort to those that are suitable 
  habitat_suitable <- habitat %>% filter(Suitable == "Marginal") %>% select(Level_2_Habitat_Class)
  if (nrow(habitat_suitable)==0){ #if known species habitat dataframe is empty for suitability
    next
  }
  
  
  vector<-habitat_suitable$Level_2_Habitat_Class
  print(vector)
  
  if (length(vector) == 1 && vector[x] %in% number == "FALSE"){
    next}
  
  combine_raster<-c()
  for (x in 1:length(vector)){ #101,102,140,150
    #print(x)
    #print(vector[x]) #101
    if (vector[x] %in% number == "TRUE"){ #if it is a number in the row = has a raster
      combine_raster<-c(combine_raster,paste0("../data/terrestrialhabitat_raster/",vector[x],".tif"))}
  }
  
  print("done")
  
  if ( length(combine_raster) > 1){
    load_raster<- lapply(combine_raster, raster)
    load_raster<- stack(load_raster)
    final_raster <- calc(load_raster, base::sum, na.rm=TRUE)
    final_raster[final_raster > 1] <- 1 #global suitable habitat is 1 
  }
  
  if ( length(combine_raster) == 1){
    final_raster<-raster(combine_raster)}#polygon is 1
  
  writeRaster(final_raster,paste0("../results/all_disease_hosts/invasive/",name,"/habitat_pref_marginal",name,".tif"),options=c('TFW=YES'))
  removeTmpFiles(h=0)
}

##COMBINED HABITAT CODE
for (y in 1:length(disease_invasive)){
  print(y)
  name <- disease_invasive[y]
  file<-list.files(path = paste0("../results/all_disease_hosts/invasive/",name,"/"), pattern = ".tif", all.files = FALSE)
  
  if (dir.exists(paste0("../results/all_disease_hosts/invasive/",name,"/")) == F){
    next
  }
  
  if (length(file) == 1){ #only one suitable
    next}
  
  print(length(file))
  
  if (length(file) == 2){
    suitable_ras<-raster(paste0("../results/all_disease_hosts/invasive/",name,"/habitat_pref_",name,".tif"))
    suitable_ras[is.na(suitable_ras[])] <- 0 #replace all NA with 0 
    marginal_ras<-raster(paste0("../results/all_disease_hosts/invasive/",name,"/habitat_pref_marginal",name,".tif"))
    marginal_ras[marginal_ras > 0] <- 0.25 #replace all more than 0 to 0.25
    marginal_ras[is.na(marginal_ras[])] <- 0 #replace all NA with 0 
    
    load_raster<- stack(marginal_ras,suitable_ras)
    combined_raster<-calc(load_raster, base::sum, na.rm=TRUE) # 0_1 raster + 0_0.25 raster -> 0,0.25,1,1.25 raster
    combined_raster[combined_raster > 1] <- 1 
  }
  
  writeRaster(combined_raster,paste0("../results/all_disease_hosts/invasive/",name,"/habitat_pref_",name,"_combined.tif"),options=c('TFW=YES'))
  removeTmpFiles(h=0)
}

#########  non_invasive_diseases #########

#SUITABLE HABITAT CODE
species_eliminate<-c()
for (y in 1:length(disease_non_invasive)){
  print(y)
  print(name)
  name <- disease_non_invasive[y]
  habitat_suitable<-habitat %>% filter(Species == disease_non_invasive[y] & Suitable == "Suitable") %>% select(Level_2_Habitat_Class)
  
  if (nrow(habitat_suitable)==0){ #if known species habitat dataframe is empty 
    species_eliminate<-c(species_eliminate,name)
    next
  }
  
  vector<-habitat_suitable$Level_2_Habitat_Class
  print(vector)
  
  if (vector %in% number == "FALSE"){
    species_eliminate<-c(species_eliminate,name) #vector of those without habitat
    next
  }
  
  combine_raster<-c()
  for (x in 1:length(vector)){ #101,102,140,150
    #print(x)
    #print(vector[x]) #101
    if (vector[x] %in% number == "TRUE"){ #if it is a number in the row = has a raster
      combine_raster<-c(combine_raster,paste0("../data/terrestrialhabitat_raster/",vector[x],".tif"))}
  }
  
  print("done")
  
  if ( length(combine_raster) > 1){
    load_raster<- lapply(combine_raster, raster)
    load_raster<- stack(load_raster)
    final_raster <- calc(load_raster, base::sum, na.rm=TRUE)
    final_raster[final_raster > 1] <- 1 #global suitable habitat is 1 
  }
  
  if ( length(combine_raster) == 1){
    final_raster<-raster(combine_raster)
  }#polygon is 1
  
  writeRaster(final_raster,paste0("../results/all_disease_hosts/non-invasive/",name,"/habitat_pref_",name,".tif"),options=c('TFW=YES'))
  removeTmpFiles(h=0)
}

write.csv(species_eliminate,"../results/all_disease_hosts/non_invasive_species_eliminate.csv")

#MARGINAL HABITAT CODE
species_eliminate<-c()
for (y in 1:length(disease_non_invasive)){
  print(y)
  print(name)
  name <- disease_non_invasive[y]
  if (name %in% species_eliminate == TRUE){ #if known species habitat dataframe is empty 
    next}
  
  habitat_suitable<-habitat %>% filter(Species == disease_non_invasive[y] & Suitable == "Marginal") %>% select(Level_2_Habitat_Class)
  
  if (nrow(habitat_suitable)==0){ #if known species habitat dataframe is empty 
    next
  }
  
  vector<-habitat_suitable$Level_2_Habitat_Class
  print(vector)
  
  if (vector %in% number == "FALSE"){
    next
  }
  
  combine_raster<-c()
  for (x in 1:length(vector)){ #101,102,140,150
    #print(x)
    #print(vector[x]) #101
    if (vector[x] %in% number == "TRUE"){ #if it is a number in the row = has a raster
      combine_raster<-c(combine_raster,paste0("../data/terrestrialhabitat_raster/",vector[x],".tif"))}
  }
  
  print("done")
  
  if ( length(combine_raster) > 1){
    load_raster<- lapply(combine_raster, raster)
    load_raster<- stack(load_raster)
    final_raster <- calc(load_raster, base::sum, na.rm=TRUE)
    final_raster[final_raster > 0] <- 0.25 #need to re load marginal and convert to 0.25
  }
  
  if ( length(combine_raster) == 1){
    final_raster<-raster(combine_raster)
    final_raster[final_raster > 0] <- 0.25
  }#polygon is 1
  
  writeRaster(final_raster,paste0("../results/all_disease_hosts/non-invasive/",name,"/habitat_pref_marginal",name,".tif"),options=c('TFW=YES'))
  removeTmpFiles(h=0)
}

##COMBINED HABITAT CODE
for (y in 1:length(disease_non_invasive)){
  print(y)
  print(name)
  name <- disease_non_invasive[y]
  file<-list.files(path = paste0("../results/all_disease_hosts/non-invasive/",name,"/"), pattern = ".tif", all.files = FALSE)
  
  if (dir.exists(paste0("../results/all_disease_hosts/non-invasive/",name,"/")) == F){
    next
  }
  
  if (length(file) <= 1){ #only one suitable
    next}
  
  if (length(file) == 2){
    suitable_ras<-raster(paste0("../results/all_disease_hosts/non-invasive/",name,"/habitat_pref_",name,".tif"))
    suitable_ras[is.na(suitable_ras[])] <- 0 #replace all NA with 0 
    marginal_ras<-raster(paste0("../results/all_disease_hosts/non-invasive/",name,"/habitat_pref_marginal",name,".tif"))
    marginal_ras[is.na(marginal_ras[])] <- 0 #replace all NA with 0 
    
    load_raster<- stack(marginal_ras,suitable_ras)
    combined_raster<-calc(load_raster, base::sum, na.rm=TRUE) # 0_1 raster + 0_0.25 raster -> 0,0.25,1,1.25 raster
    combined_raster[combined_raster > 1] <- 1 
  }
  
  print("saving")
  writeRaster(combined_raster,paste0("../results/all_disease_hosts/non-invasive/",name,"/",name,"habitat_pref_",name,"_combined.tif"),options=c('TFW=YES'))
  removeTmpFiles(h=0)
}

# 4. Get Range Polygons from the Invasive-disease and noninvasive-disease species

dama_poly<-st_read(paste0("../data/polygons/DAMA/",name,".shp")) #Get DAMA polygons 
disease_invasive #144 species 
#how many invasive polygons are there from dama_poly? Get the species name that does not have polygon from DAMA
no_DAMA_poly<-c()
for (m in 1: length(disease_invasive)){
  name<-disease_invasive[m]
  file <-list.files(path = "../data/polygons/DAMA" , pattern = paste0(name,".shp") , recursive = TRUE, full.names = TRUE)
  
  if (length(file) == 0){ #if polygon does not exist
    no_DAMA_poly<-c(no_DAMA_poly,name)}
}
#7 species does not have polygons on DAMA: Antilocapra americana,Bandicota bengalensis,
#Equus grevyi, Leptailurus serval, Mus musculus, Rattus rattus, Syncerus caffer


# 5.if DAMA has no polygons, find it on IUCN 
invasive_vector<-c("Possibly Extant & Introduced (resident)","Probably Extant & Introduced (resident)", "Extant & Introduced (resident)")
for (n in 1:length(no_DAMA_poly)){
  print(n)
  name<-no_DAMA_poly[n]
  range_poly<-IUCN_range_poly %>% filter(binomial == name)
  legend <- range_poly$legend 
  
  if (any(legend %in% invasive_vector) == T) { #if any is an invasive species 
    invasive<-IUCN_range_poly %>% filter(binomial == name & legend %in% invasive_vector)
  }
  st_write(invasive, paste0(".../data/polygons/DAMA/IUCN/",name,".shp"))
}
# I have got all invasive polygons for invasive host species in DAMA polygon directory
# Now need to isolate resident polygons for all host species

# 6. Now I should Isolate extant, native polygons for Invasive host species and non invasive host species

resident_vector<-c("Extant (resident)", "Possibly Extant (resident)", "Probably Extant (resident)",
                   "Possibly Extant & Vagrant (non-breeding)", "Possibly Extant (seasonality uncertain)", 
                   "Extant & Vagrant (resident)","Extant & Reintroduced (resident)", 
                   "Extant & Vagrant (seasonality uncertain)", "Extant & Origin Uncertain (resident)",
                   "Possibly Extant & Origin Uncertain (resident)")

#########  invasive_diseases #########
missing_resident_poly <- c()
for (q in 1:length(disease_invasive)){
  name<-disease_invasive[q]
  range_poly<-IUCN_range_poly %>% filter(binomial == name)
  legend <- range_poly$legend 
  
  print(q)
  print(name)
  
  if (any(legend %in% resident_vector) == T) { #if any is an invasive species 
    resident<-IUCN_range_poly %>% filter(binomial == name & legend %in% resident_vector)
    st_write(resident, paste0("../results/all_disease_hosts/invasive/",name,"/",name,"_resident_poly.shp"))
  }
  
  if (any(legend %in% resident_vector) == F) { #if any is an invasive species 
    missing_resident_poly <- c(missing_resident_poly,name)
  }
  
}

#########  non_invasive_diseases #########
missing_resident_poly <- c()
for (q in 1:length(disease_non_invasive)){
  name<-disease_non_invasive[q]
  range_poly<-IUCN_range_poly %>% filter(binomial == name)
  legend <- range_poly$legend 
  
  print(q)
  print(name)
  
  if (name %in% species_eliminate){ #non_invasive_species without habitat suitbility info are removed 
    next
  }
  
  if (any(legend %in% resident_vector) == T) { #if any is an invasive species 
    resident<-IUCN_range_poly %>% filter(binomial == name & legend %in% resident_vector)
    st_write(resident, paste0("../results/all_disease_hosts/non-invasive/",name,"/",name,"_resident_poly.shp"))
  }
  
  if (any(legend %in% resident_vector) == F) { #if any is an invasive species 
    missing_resident_poly <- c(missing_resident_poly,name)
  }
  
}

# Now for invasive disease -> I have resident and invasive polygons. 
# For non invasive disease -> I have resident polygons
# I also have habitat raster for all species 

# 7. Create a dataframe for invasives diseases and non invasive disease with cell id of where in the world they located with native and invasive range

#########  invasive_diseases #########

disease<-fread("./data/trefle.csv")
disease_invasive<-list.files(path = "../results/all_disease_hosts/invasive/", pattern = "", all.files = FALSE) 

disease_non_invasive<-setdiff(disease$host,disease_invasive) #937 disease hosts that are not invasive

no_DAMA_poly <- c("Antilocapra americana","Bandicota bengalensis","Equus grevyi", 
                  "Leptailurus serval", "Mus musculus", "Rattus rattus", "Syncerus caffer")

ws2<-NULL

disease_invasive<-sample(disease_invasive)

for (l in 1:length(disease_invasive)){
  df_combined<-data.frame()
  df_pathogen<-data.frame()
  print(l)
  name<-disease_invasive[l]
  print(name)
  
  ##check if done
  if(paste("../results/invasive/dataframe_",name,".csv") %in% list.files("../results/invasive/",full.names=TRUE)){next}
  
  #load global raster
  file<-list.files(path = paste0("../results/all_disease_hosts/invasive/",name,"/"), pattern = ".tif", all.files = FALSE)
  
  if (length(file) == 1){
    habitat<-rast(paste0("../results/all_disease_hosts/invasive/",name,"/habitat_pref_",name,".tif"))}
  
  if (length(file) == 3){
    habitat<-rast(paste0("../results/all_disease_hosts/invasive/",name,"/habitat_pref_",name,"_combined.tif"))}
  
  habitat2<-terra::aggregate(habitat,fact=8,fun=mean,na.rm=TRUE)
  
  ## INTRODUCED RANGE
  #load polygon 
  if (name %in% no_DAMA_poly ){
    polygon_shape <- vect(paste0("../data/polygons/DAMA/IUCN/",name,".shp"))}
  
  if (!(name %in% no_DAMA_poly)){
    polygon_shape <- vect(paste0("../data/polygons/DAMA/",name,".shp"))}
  
  df_I <- terra::extract(habitat2, polygon_shape,cells=TRUE,df=TRUE)
  range <- rep("I", nrow(df_I)) # I is introduced
  df_I$range<-range
  df_I$species<-rep(name, nrow(df_I))
  
  ## NATIVE RANGE
  #load polygon 
  polygon_shape_2<- vect(paste0("../results/all_disease_hosts/invasive/",name,"/",name,"_resident_poly.shp"))
  df_N <- terra::extract(habitat2, polygon_shape_2,cells=TRUE,df=TRUE)
  range_2 <- rep("N", nrow(df_N)) # I is introduced
  df_N$range<-range_2
  df_N$species<-rep(name, nrow(df_N))
  
  ##get rid of combined
  names(df_N)[3]<-gsub("_combined","",names(df_N)[3])
  names(df_I)[3]<-gsub("_combined","",names(df_I)[3])
  
  #Combine dataframe
  df_combined<-rbind(df_I,df_N)
  df_combined <- select(df_combined, -1) #delete id column 
  names(df_combined)[1] <- "habitat_pref"
  df_combined<-df_combined[!(df_combined$habitat_pref == 0),] #delete all rows of habitat pref = 0 
  
  removeTmpFiles(h=0)
  
  ###rasterize world simple
  if(is.null(ws2)){ws2<-fasterize(st_as_sf(wrld_simpl),raster(habitat2),field="UN")}
  
  #ws2<-rast(ws2)
  
  ###find country per cell id
  df_combined$country_UN<-raster::extract(ws2,df_combined$cell)
  
  ##Pathogens
  pathogen_no <- disease %>% filter(host == name)  %>% select(virus)
  #path1<-data.frame(species=df_combined$species[1],pathogen=pathogen_no)
  #setDT(path1)
  #setkey(path1,species)
  
  df_combined$pathogens<-paste(pathogen_no$virus,collapse=";")
  setDT(df_combined)
  #setkey(df_combined,species)
  
  #df_combined<-df_combined[path1,allow.cartesian=TRUE]
  
  fwrite(df_combined, paste("../results/invasive/dataframe_",name,".csv"),row.names = FALSE)
}


#########  Non-invasive_diseases #########

disease_non_invasive<-setdiff(disease$host,disease_invasive) #937 disease hosts that are not invasive

ws2<-NULL

disease_non_invasive<-sample(disease_non_invasive)

for (l in 1:length(disease_non_invasive)){
  df_combined<-data.frame()
  df_pathogen<-data.frame()
  print(l)
  name<-disease_invasive[l]
  print(name)
  
  ##check if done
  if(paste("../results/non_invasive/dataframe_",name,".csv") %in% list.files("../results/non_invasive/",full.names=TRUE)){next}
  
  #load global raster
  file<-list.files(path = paste0("../results/all_disease_hosts/non-invasive/",name,"/"), pattern = ".tif", all.files = FALSE)
  
  if (length(file) == 1){
    habitat<-rast(paste0("../results/all_disease_hosts/non-invasive/",name,"/habitat_pref_",name,".tif"))}
  
  if (length(file) == 3){
    habitat<-rast(paste0("../results/all_disease_hosts/non-invasive/",name,"/habitat_pref_",name,"_combined.tif"))}
  
  habitat2<-terra::aggregate(habitat,fact=8,fun=mean,na.rm=TRUE)
  
  ## NATIVE RANGE
  #load polygon 
  polygon_shape_2<- vect(paste0("../results/all_disease_hosts/non-invasive/",name,"/",name,"_resident_poly.shp"))
  df_N <- terra::extract(habitat2, polygon_shape_2,cells=TRUE,df=TRUE)
  range_2 <- rep("N", nrow(df_N)) # I is introduced
  df_N$range<-range_2
  df_N$species<-rep(name, nrow(df_N))
  
  ##get rid of combined
  names(df_N)[3]<-gsub("_combined","",names(df_N)[3])

  #Combine dataframe
  df_combined<-df_N
  df_combined <- select(df_combined, -1) #delete id column 
  names(df_combined)[1] <- "habitat_pref"
  df_combined<-df_combined[!(df_combined$habitat_pref == 0),] #delete all rows of habitat pref = 0 
  
  removeTmpFiles(h=0)
  
  ###rasterize world simple
  if(is.null(ws2)){ws2<-fasterize(st_as_sf(wrld_simpl),raster(habitat2),field="UN")}
  
  #ws2<-rast(ws2)
  
  ###find country per cell id
  df_combined$country_UN<-raster::extract(ws2,df_combined$cell)
  
  ##Pathogens
  pathogen_no <- disease %>% filter(host == name)  %>% select(virus)
  #path1<-data.frame(species=df_combined$species[1],pathogen=pathogen_no)
  #setDT(path1)
  #setkey(path1,species)
  
  df_combined$pathogens<-paste(pathogen_no$virus,collapse=";")
  setDT(df_combined)
  #setkey(df_combined,species)
  
  #df_combined<-df_combined[path1,allow.cartesian=TRUE]
  
  fwrite(df_combined, paste("../results/non_invasive/dataframe_",name,".csv"),row.names = FALSE)
}


# 7. A code to create a massive dataframe with showing what pathogens are there in each 8x8 cell if, if its
#invasive, native pathogens
#colnames: "cellid_range" "dummy" "pathogensU" "cell_id" "range"     
#for two csv output, invasive disease and non invasive disease species

#########  invasive_diseases #########

files<-list.files("../results/invasive/",pattern=".csv",full.names = T) #Need to change the path

#df <- data.table() #Output DATAFRAME 
for (a in 1:length(files)){
  print(a)
  csv_file<-fread(files[a])
  csv_file<-subset(csv_file, habitat_pref > 0.5) #only saving rows with habitat preference is larger than 0.5
  csv_file[,cellid_range:=paste(cell,range,sep="_")] #cellid_range
  csv_file<-csv_file[,(names(csv_file)[!names(csv_file) %in% c("pathogens","cellid_range")]):=NULL]  
  #rownames(csv_file) <- csv_file$cellid_range
  #csv_file <- csv_file[-2]
  #setkey(csv_file,cellid_range)
  
  if (a==1){ df<-csv_file;next}#need this for first loop
  #  df$pathogens<-paste(df$pathogens.x, df$pathogens.y,sep=";")#combine two columns together
  #  df<-subset(df, select=-c(pathogens.x,pathogens.y))
  #}
  #rownames(df) <- df$Row.names
  #df<-subset(df, select=-c(Row.names))#only get pathogen column
  
  
  ##merge
  df <- merge(x = df, y = csv_file,by="cellid_range" , all = TRUE) 
  
  ##merge columns
  df[,pathogens:=paste(pathogens.x,pathogens.y,sep=";")]
  
  ##remove columns
  df<-df[,(names(df)[!names(df) %in% c("pathogens","cellid_range")]):=NULL]  
  
}

write.csv(df,"../results/invasive_uniquesALLMORE.csv") 

#########  Non-invasive_diseases #########

files<-list.files("../results/non_invasive/",pattern=".csv",full.names = T) #Need to change the path
df <- data.table() #Output DATAFRAME 
for (a in 1:length(files)){
  print(a)
  csv_file<-fread(files[a])
  csv_file<-subset(csv_file, habitat_pref > 0.5) #only saving rows with habitat preference is larger than 0.5
  csv_file[,cellid_range:=paste(cell,range,sep="_")] #cellid_range
  csv_file<-csv_file[,(names(csv_file)[!names(csv_file) %in% c("pathogens","cellid_range")]):=NULL]  
  #rownames(csv_file) <- csv_file$cellid_range
  #csv_file <- csv_file[-2]
  #setkey(csv_file,cellid_range)
  
  if (a==1){ df<-csv_file;next}#need this for first loop
  #  df$pathogens<-paste(df$pathogens.x, df$pathogens.y,sep=";")#combine two columns together
  #  df<-subset(df, select=-c(pathogens.x,pathogens.y))
  #}
  #rownames(df) <- df$Row.names
  #df<-subset(df, select=-c(Row.names))#only get pathogen column
  
  
  ##merge
  df <- merge(x = df, y = csv_file,by="cellid_range" , all = TRUE) 
  
  ##merge columns
  df[,pathogens:=paste(pathogens.x,pathogens.y,sep=";")]
  
  ##remove columns
  df<-df[,(names(df)[!names(df) %in% c("pathogens","cellid_range")]):=NULL]  
  
}
write.csv(df,"../results/noninvasive_uniquesALLMORE.csv") 







