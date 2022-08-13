##Combining All Dataframes into a dataframe with all information

#1. Create Novel pathogen columns 
library(data.table)
library(maptools)
library(fasterize)
library(tidyverse)
library(raster)
library(sf)
library(terra)

data("wrld_simpl")

invasive_unique_df<-fread("../results/invasive_uniquesALLMORE.csv") 
noninvasive_unique_df<-fread("../results/noninvasive_uniquesALLMORE.csv") 
colnames(invasive_unique_df)
invasive_unique_df <- invasive_unique_df %>% rename(Invasive_Pathogens = pathogensU)
noninvasive_unique_df <- noninvasive_unique_df %>% rename(Native_Pathogens = pathogensU)

invasive_unique_df <- invasive_unique_df[,-1]
invasive_unique_df <- invasive_unique_df[,-4]

invasive_noninvasive<-merge(invasive_unique_df,noninvasive_unique_df,by="cell_id",all = TRUE)

##make dummy
invasive_noninvasive[, dummy:=(1:nrow(invasive_noninvasive))]

##apply function
invasive_noninvasive[, NpathogensI:=get_intersection(xxx=Invasive_Pathogens,yyy=Native_Pathogens),by=dummy]
#get the pathogens that are new novel pathogens 


invasive_noninvasive_noNA<-invasive_noninvasive%>%filter(!Invasive_Pathogens=="NA")
#df without NA 

#function
rm_NA<-function(x) x[x!="NA"]
strsplit2<-function(x) paste(rm_NA(unique(strsplit(x,";",fixed=TRUE)[[1]])),collapse=";")

###get intersection
get_intersection<-function(xxx,yyy) {
  x1<-strsplit(xxx,";",fixed=TRUE)[[1]]  
  y1<-strsplit(yyy,";",fixed=TRUE)[[1]]      
  return(length(x1[!x1 %in% y1])) #return all invasives that are not in native aka native pathogens 
}

### plot
library(maps)
library(maptools)
install.packages("viridis")
library(viridis)

countries <- map("world", plot=FALSE) 
countries <- map2SpatialLines(countries, proj4string = CRS("+proj=longlat"))

habitat2<-raster("../data/habitat2.tif")
wrld_simpl<-wrld_simpl[wrld_simpl$NAME!="Antarctica",] #delete antarctica
ws2<-fasterize(st_as_sf(wrld_simpl),raster(habitat2),field="UN")

template<-raster(habitat2) 
values(template)=0 #change a base raster to 0
template<-mask(template,(ws2)) #put world simple on top of base raster 
template[invasive_noninvasive_noNA$cell_id]<-invasive_noninvasive_noNA$NpathogensI
template<-focal(template,matrix(rep(1/25,25),5,5),na.rm=TRUE) 
template<-mask(template,(ws2))

plot(template,col=c("light grey",rev(viridis(50,option="F"))),main="Areas of Novel Pathogens due to Invasives Species")
plot(countries,add=T, lw=0.4)

#2. Create columns for reported human pathogens only
wellknown<-fread("../data/human_disease_rory_dark.csv",header=T)
wellknown<-wertheim$human_interpolated_disease_c #reported human pathogens

invasive_noninvasive_noNA[, Invasive_Pathogens_edited:=remove_wellknown(xxx=Invasive_Pathogens),by=dummy]
invasive_noninvasive_noNA[, Native_Pathogens_edited:=remove_wellknown(xxx=Native_Pathogens),by=dummy]
invasive_noninvasive_noNA[, NpathogensI_edited:=get_intersection(xxx=Invasive_Pathogens_edited,yyy=Native_Pathogens_edited),by=dummy]

remove_wellknown<-function(xxx){
  x1<-strsplit(xxx,";",fixed=TRUE)[[1]]
  x1<-tolower(x1)
  x1<-x1[x1 %in% wellknown]
  x1<-paste(x1, collapse=";")
  return(x1)
}

### plot 
template_2<-raster(habitat2) 
values(template_2)=0 #change a base raster to 0
template_2<-mask(template_2,(ws2)) #put world simple on top of base raster 
template_2[allcells$x]<-allcells$NpathogensI_edited
template_2<-focal(template_2,matrix(rep(1/25,25),5,5),na.rm=TRUE) #
template_2<-mask(template_2,(ws2))

plot(template_2,col=c("light grey",rev(viridis(50,option="F"))),main="Areas of Novel Human Pathogens due to Invasives Species")
plot(countries,add=T, lw=0.4)

#3. Get richness of disease just from invasive species
invasive_unique_df<-fread("../results/invasive_uniquesALLMORE.csv") 

get_richness<-function(xxx) {
  x1<-unique(strsplit(xxx,";",fixed=TRUE)[[1]])
  return(length(x1))  
}

invasive_noninvasive_noNA[,no_invasive_pathogens:=get_richness(xxx=Invasive_Pathogens),by=dummy]

### plot
template_3<-raster(habitat2) 
values(template_3)=0 #change a base raster to 0
template_3<-mask(template_3,(ws2)) #put world simple on top of base raster 
template_3[invasive_noninvasive_noNA$cell_id]<-invasive_noninvasive_noNA$no_invasive_pathogens
template_3<-focal(template_3,matrix(rep(1/25,25),5,5),na.rm=TRUE) 
template_3<-mask(template_3,(ws2))

plot(template_3,col=c("light grey",rev(viridis(50,option="F"))),main="Richness of Global Invasive Pathogens from Invasive Species")
plot(countries,add=T, lw=0.4)

#4. Add country name column into the dataframe based on cell id 
global_raster<-raster("../data/base_raster.tif")

ctsb<-rasterize(wrld_simpl,global_raster,field="UN") ##forgot it needed to be numeric, wish I had used this all along
cts<-resample(ctsb,habitat2,method="ngb")
ctsids<-data.frame(invasive_noninvasive_noNA=1:ncell(habitat2),country=values(cts))
ctsids<-ctsids[!is.na(ctsids$country),]

allcells<-merge(invasive_noninvasive_noNA,ctsids,by.x="cell_id",by.y="cellid",all.x=TRUE,all.y=FALSE) #countries

#5. Getting column of human diseases that are novel + not reported
library(stringr)

country<-fread("../results/reportedcountry_cellid_combined.csv", encoding = 'UTF-8') #all reported pathogens df
country$pathogens<-str_trim(country$pathogens)
report_df_cell2<-country[,listpathrep:=paste(pathogens,collapse=","),by=x] 
report_df_cell2<-report_df_cell2[!duplicated(as.vector(x)),]

main_df<-allcells #df with cell id and pathogens

setkey(main_df,cell_id)

setkey(report_df_cell2,x)

###write function to get intersection of invasive, native and reported list or length
get_intersection<-function(xxx,yyy,zzz,length=FALSE) {
  
  x1<-strsplit(xxx,";",fixed=TRUE)[[1]]  #invasive
  
  y1<-strsplit(yyy,";",fixed=TRUE)[[1]]      #native
  
  z1<-strsplit(zzz,",",fixed=TRUE)[[1]]      #reported
  
  x2<-x1[!x1 %in% y1] #invasive cannot be in native = novel
  
  x2<-tolower(x2)
  
  #here its novel cannot be in reported 
  if(length==FALSE) {return(paste(x2[!x2 %in% z1],collapse=";"))} else {return(length(x2[!x2 %in% z1]))}
  
}


##apply function
main_df[, dummy:=(1:nrow(main_df))]
main_df[, pathogensInotrep:=get_intersection(xxx=Invasive_Pathogens_edited,yyy=Native_Pathogens_edited,zzz=listpathrep),by=dummy]


get_length<-function(xxx) {
  x1<-strsplit(xxx,";",fixed=TRUE)[[1]]  #invasive
  return (length(x1))
}


##apply function
main_df[, lenpathInorep:=get_length(xxx=pathogensInotrep),by=dummy]

### plot
template_4<-raster(habitat2) 
values(template_4)=0 #change a base raster to 0
template_4<-mask(template_4,(ws2)) #put world simple on top of base raster 

template_4[allcells$x]<-allcells$lenpathInorep
template_4<-focal(template_4,matrix(rep(1/25,25),5,5),na.rm=TRUE) #WHAT IS THIS FOR
template_4<-mask(template_4,(ws2))

plot(template_4,col=c("light grey",rev(viridis(140,option="F"))),main="Areas with invasive novel, not reported human diseases")
plot(countries,add=T, lw=0.4)

#6. Getting column of area not reported with disease, native species and invasive both found with pathogen,
#area with high capacity of disease, increase disease surveillance 
get_intersection4<-function(xxx,yyy,zzz) {
  x1<-strsplit(xxx,";",fixed=TRUE)[[1]]  #invasive_pathogens_edited
  y1<-strsplit(yyy,";",fixed=TRUE)[[1]] #native_pathogens_edited
  z1<-strsplit(zzz,",",fixed=TRUE)[[1]]   #listpathrep
  
  x2<-x1[!x1 %in% z1] #all invasive pathogens that are not reported 
  y2<-y1[!y1 %in% z1] #all native pathogens that are not reported 
  xy<-intersect(x2,y2) #get those with same pathogens in both native and invasive, that are not reported
  return(length(xy))
}

main_df[, len_areanorep_IandN:=get_intersection4(xxx=Invasive_Pathogens_edited,yyy=Native_Pathogens_edited,zzz=listpathrep),by=dummy]

### plot
template_5<-raster(habitat2) 
values(template_5)=0 #change a base raster to 0
template_5<-mask(template_5,(ws2)) #put world simple on top of base raster 

template_5[allcells$x]<-allcells$len_areanorep_IandN
template_5<-focal(template_5,matrix(rep(1/25,25),5,5),na.rm=TRUE) #WHAT IS THIS FOR
template_5<-mask(template_5,(ws2))

plot(template_5,col=c("light grey",rev(viridis(140,option="F"))),main="Areas with high capacity of disease")
plot(countries,add=T, lw=0.4)

#7. Bolstering transmission = areas reported with disease, both native and invasive species are found to have it as well 
get_intersection5<-function(xxx,yyy,zzz) {
  x1<-strsplit(xxx,";",fixed=TRUE)[[1]]  #invasive_pathogens_edited
  y1<-strsplit(yyy,";",fixed=TRUE)[[1]] #native_pathogens_edited
  z1<-strsplit(zzz,",",fixed=TRUE)[[1]]   #listpathrep
  
  x2<-x1[x1 %in% z1] #all invasive pathogens that are reported 
  y2<-y1[y1 %in% z1] #all native pathogens that are reported 
  xy<-intersect(x2,y2) #get those with same pathogens in both native and invasive, that are  reported
  return(length(xy))
}

main_df[, len_bolsterpathogens:=get_intersection5(xxx=Invasive_Pathogens_edited,yyy=Native_Pathogens_edited,zzz=listpathrep),by=dummy]

### plot
template_6<-raster(habitat2) 
values(template_6)=0 #change a base raster to 0
template_6<-mask(template_6,(ws2)) #put world simple on top of base raster 

template_6[allcells$x]<-allcells$len_bolsterpathogens
template_6<-focal(template_6,matrix(rep(1/25,25),5,5),na.rm=TRUE) #WHAT IS THIS FOR
template_6<-mask(template_6,(ws2))

plot(template_6,col=c("light grey",rev(viridis(140,option="F"))),main="Areas with invasive species that bolster disease transmission")
plot(countries,add=T, lw=0.4)

#8. Areas of novel reported pathogens introduced by invasives , no native found to have it
get_intersection6<-function(xxx,yyy,zzz) {
  x1<-strsplit(xxx,";",fixed=TRUE)[[1]]  #Invasive_Pathogens_edited
  y1<-strsplit(yyy,";",fixed=TRUE)[[1]]   #Native_Pathogens_edited
  z1<-strsplit(zzz,",",fixed=TRUE)[[1]]     #listpathrep

  x2<-x1[!x1 %in% y1] #invasives cannot be in native = novel
  xy<-x2[x2 %in% z1] #all novel pathogens that are reported 
  return(length(xy))
  
}

main_df[, len_pathIrep:=get_intersection6(xxx=Invasive_Pathogens_edited,yyy=Native_Pathogens_edited,zzz=listpathrep),by=dummy]

### plot
template_7<-raster(habitat2) 
values(template_7)=0 #change a base raster to 0
template_7<-mask(template_7,(ws2)) #put world simple on top of base raster 

template_7[allcells$x]<-allcells$len_pathIrep
template_7<-focal(template_7,matrix(rep(1/25,25),5,5),na.rm=TRUE) #WHAT IS THIS FOR
template_7<-mask(template_7,(ws2))

plot(template_7,col=c("light grey",rev(viridis(140,option="F"))),main=". Global areas where reported human pathogens are introduced by invasive species, but absent in natives")
plot(countries,add=T, lw=0.4)

####### SAVE FINAL CSV
write.csv(main_df,"../results/allinformation.csv")

#9. Create a new dataframe of countries and the invasive novel not reported diseases to look out for
library(maptools)
wrld_simpl@data #get country

habitat2<-raster("../data/habitat2.tif")
wrld_simpl<-wrld_simpl[wrld_simpl$NAME!="Antarctica",] #delete antarctica
ctsb<-fasterize(st_as_sf(wrld_simpl),raster(habitat2),field="UN")
ctsids<-data.frame(cellid=1:ncell(habitat2),country=values(ctsb))
ctsids<-ctsids[!is.na(ctsids$country),]

allcells<-merge(main_df,ctsids,by.x="cell_id",by.y="cellid",all.x=TRUE)
df1<-data.table(allcells$x,allcells$pathogensInotrep,allcells$country)
colnames(df1) <- c("cellid","pathogensInotrep","country")

df2 <- df1 %>% group_by(country) %>% dplyr::summarize(all_pathogensInotrep = paste(pathogensInotrep, collapse = ";"))

get_uniques<-function(xxx) {
  x1<-strsplit(xxx,";",fixed=TRUE)[[1]] 
  x2<-unique(x1)
  x2<-x2[x2!= ""]
  return(list(paste(x2,collapse=";"),length(x2)))
}

df2<-as.data.table(df2)
df2[, dummy:=(1:nrow(df2))]
df2[, c("new_pathogen","lennew_pathogen"):=get_uniques(xxx=all_pathogensInotrep),by=dummy]

df3<-data.table(df2$country,df2$new_pathogen,df2$lennew_pathogen)
out<-merge(df3,wrld_simpl[,c("UN","NAME")],by.x="V1",by.y="UN",all.x=TRUE) #merge with world simple UN code to name 
colnames(out) <- c("UN","Novel Non Reported Invasive Pathogens","n","Country")

library(sjPlot)
out<-out[-1,] #delete NA
out<-out[,-1] #delete NA
colnames(out) <- c("Novel Non Reported Invasive Pathogens","n","Country")
setcolorder(out, c("Country", "Novel Non Reported Invasive Pathogens", "n"))
out<-out %>% arrange(desc(n))

#10. Add a column of proportion of countries affected by invasive novel non reported pathogens 
ws2<-fasterize(st_as_sf(wrld_simpl),raster(habitat2),field="UN")

template<-raster(habitat2) 
values(template)=0 #change a base raster to 0
template<-mask(template,(ws2)) #put world simple on top of base raster 
template[invasive_noninvasive_noNA$cell_id]<-invasive_noninvasive_noNA$NpathogensI

v<-extract(template, st_as_sf(wrld_simpl),na.rm=TRUE) #extract all cells, excluding NA 
v_edited_No_NA<-lapply(v, function(x) x[!is.na(x)])
v_edited_No_NA_2<-unlist(lapply(v_edited_No_NA, function(x) length(x[x>0]))) #extract all cells that are more than 0

final_length<-unlist(lapply(v, function(x) length(x)))
proportion<-v_edited_No_NA_2/final_length

proportion_df<-data.table(wrld_simpl$NAME,proportion)
out2<-merge(out,proportion_df,by.x="Country",by.y="V1",all.x=TRUE)
fwrite(out2, file = "../results/country_info.csv", row.names = F)

out3<-out2[1:90]
out3<-out3[,-2]

fwrite(out3, file = "../results/proportion_table3.csv", row.names = F)

