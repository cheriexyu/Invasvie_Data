# 1. To get the raster global range for each reported disease using country ISO 

library(maptools)
library(raster)
library(data.table)
library(sf)
library(terra)
library(fasterize)
data("wrld_simpl")

##In total 257 human diseases
global_raster<-raster("../data/base_raster.tif")
human_df<-fread("../data/human_disease_rory_dark.csv")

dir.create("../results/country_ranges_2/",showWarnings = TRUE, recursive = FALSE)

for (h in 1:nrow(human_df)){
  disease_row<-human_df[h,]
  print(h)
  print(disease_row$human_interpolated_disease_c)
  name<-disease_row$human_interpolated_disease_c
  countries<-unlist(strsplit(disease_row$CountryISO_EID2Wertheim, split = ","))
  
  SPDF <- subset(wrld_simpl, wrld_simpl$ISO2 %in% countries)
  SPDF <- st_as_sf(SPDF)
  df<-fasterize(SPDF,global_raster)
  df<-terra::aggregate(df,fact=8,fun=mean,na.rm=TRUE)
  
  df2<-(1:ncell(df))[!is.na(values(df))]
  write.csv(df2,paste("../results/country_ranges_2/cellid_",name,".csv"),row.names=F)
}

# 2. Combinine #1 into one large dataframe

files<-list.files("../results/country_ranges_2",pattern=".csv",full.names = T) 
name<-list.files("../results/country_ranges_2",pattern=".csv",full.names = F)

cellid_allpathogens<-data.table()
for (a in 1:length(files)){
  print(a)
  disease<-name[a]
  read_in_df<-fread(files[a])
  splitname<-strsplit(disease, '[.]')[[1]]
  splitname<-splitname[1]
  splitname<-str_split(splitname, " ", 2)
  splitname<-splitname[[1]][2]
  read_in_df$pathogens<-rep(splitname,nrow(read_in_df))
  
  cellid_allpathogens<-rbind(cellid_allpathogens,read_in_df)
}
#each original df is the cell id of the reported countries of each human pathogen

write.csv(cellid_allpathogens,"../results/reportedcountry_cellid_combined.csv",row.names=F)
unlink("../results/country_ranges_2/", recursive = TRUE)



