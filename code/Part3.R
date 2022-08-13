#1. Is the overlap driven by traits? Sourcing All Trait Data
remotes::install_github("ropensci/rfishbase") #download fishbase database
library("rfishbase")
library(data.table)
library(dplyr)

##1A. Sourcing trait data

#Mammals
mammal_ncbi <- fread("../results/mammal_species_ncbi.csv") #5483
mammal_ncbi <- mammal_ncbi[,3]

pantheria_2 <- fread("../data/trait_data/PanTHERIA_1-0_WR93_Aug2008.txt") #4629
pantheria_2  = select(pantheria_2,-1:-4)
names(pantheria_2)[1] <- "species"
pantheria_2$species= tolower(pantheria_2$species) #10045
pantheria_2[pantheria_2 == -999.0] <- NA
pantheria_2[pantheria_2 == -999.00] <- NA
pantheria_2[pantheria_2 == -999.000] <- NA

phylacine <- fread("../data/trait_data/phylacine_Trait_data.csv") #5831
phylacine$species <- paste(phylacine$Genus.1.2, phylacine$Species.1.2, sep=" ")
phylacine  = select(phylacine,-1:-5)
phylacine$species= tolower(phylacine$species) 

mammal_traits<-merge(mammal_ncbi,phylacine,by.x="species", by.y="species",all.x=T) #5483
mammal_traits<-merge(mammal_traits,pantheria_2,by.x="species", by.y="species",all.x=T)
mammal_traits<-mammal_traits[rowSums(is.na(mammal_traits)) != 69, ]#Delete rows with all NA, 4628
mammal_traits<-mammal_traits[rowSums(is.na(mammal_traits)) != 70, ]#Delete rows with all NA, 4628
mammal_traits<-mammal_traits[rowSums(is.na(mammal_traits)) != 68, ]#Delete rows with all NA, 4628

group<-rep("N", each = 4630) #Add a column with I = Invasive, H = Host, N = Neither , HI = Invasive and Host 
mammal_traits$group<-group
host<-fread("../results/filling_in_class_order.csv") #virion 
host<-host[-1,]
host <- host$V2
host_mammals<-intersect(host,mammal_traits$species) #815 
mammal_traits$host<-rep(NA,4569)
for (i in 1:4569){
  x <- mammal_traits$species[i]
  y <- x %in% host_mammals
  if (y==TRUE){ #if it is a host then change group N to H 
    mammal_traits$host <- replace(mammal_traits$host,i,'H')}
}

invasive <- fread("../data/SInAS_AlienSpeciesDB_2.4.1.csv")
invasive$name_lower = tolower(invasive$Taxon) #lowercase
invasive <- invasive$name_lower
invasive <- unique(invasive)
invasive_mammals<-intersect(invasive,mammal_traits$species) #337
mammal_traits$invasive<-rep(NA,4569)
for (i in 1:4569){
  x <- mammal_traits$species[i]
  y <- x %in% invasive_mammals
  if (y==TRUE){ #if it is a invasive then change group N to I 
    mammal_traits$invasive <- replace(mammal_traits$invasive,i,'I')}
}

mammal_traits$host_invasive<-rep(NA,4569)
for (i in 1:4569){
  print(i)
  if ((is.na(mammal_traits$host[i]) == F) && (is.na(mammal_traits$invasive[i]) == F)){
    mammal_traits$host_invasive <- replace(mammal_traits$host_invasive,i,'HI')} #if it is a invasive host then change group N to HI 
}

mammal_traits$none<-rep(NA,4569)
for (i in 1:4569){
  if (is.na(mammal_traits$host[i]) == T && is.na(mammal_traits$invasive[i]) == T ){ #if not invasive or host change to none
    mammal_traits$none <- replace(mammal_traits$none,i,'None')}
}
mammal_traits <- mammal_traits%>%relocate(group, .after=species)

write.csv(mammal_traits,"../results/edited_mammal_traits.csv",row.names = FALSE)

#birds
birds_ncbi <- fread("../results/birds_species_ncbi.csv") #10,097
birds_ncbi <- birds_ncbi[,3]

global_HWI <- fread("../data/trait_data/Dataset_HWI.csv") #10,340
global_HWI$IUCN_name= tolower(global_HWI$IUCN_name)

amniote <- fread("../data/trait_data/Amniote_Database_Aug_2015.csv")
amniote <- amniote %>% filter(class=="Aves") #9802 
amniote  = select(amniote,4:5,9,17,23:25)
amniote[amniote == -999.0] <- NA
amniote[amniote == -999.00] <- NA
amniote[amniote == -999.000] <- NA
amniote$specie <- paste(amniote$genus, amniote$species, sep=" ")
amniote[ , c('genus', 'species')] <- list(NULL)
amniote$specie= tolower(amniote$specie)

bird_traits <- merge(birds_ncbi,global_HWI,by.x="species", by.y="IUCN_name")
bird_traits <- merge(bird_traits,elton_traits,by.x="species", by.y="Scientific") #7266 
bird_traits <- merge(bird_traits,amniote,by.x="species", by.y="specie") #6791

host_birds<-intersect(host,bird_traits$species) #269 #Add a column with I = Invasive, H = Host, N = Neither , HI = Invasive and Host 
bird_traits$host<-rep(NA,6791)

for (i in 1:6791){
  x <- bird_traits$species[i]
  y <- x %in% host_birds
  if (y==TRUE){ #if it is a host then change group N to H 
    bird_traits$host <- replace(bird_traits$host,i,'H')}
}

invasive_birds<-intersect(invasive,bird_traits$species) #1003
bird_traits$invasive<-rep(NA,6791)
for (i in 1:6791){
  x <- bird_traits$species[i]
  y <- x %in% invasive_birds
  if (y==TRUE){ #if it is a invasive then change group N to I 
    bird_traits$invasive <- replace(bird_traits$invasive,i,'I')}
}

bird_traits$host_invasive<-rep(NA,6791)
for (i in 1:6791){
  print(i)
  if ((is.na(bird_traits$host[i]) == F) && (is.na(bird_traits$invasive[i]) == F)){
    bird_traits$host_invasive <- replace(bird_traits$host_invasive,i,'HI')} #if it is a invasive host then change group N to HI 
}

bird_traits$none<-rep(NA,6791)
for (i in 1:6791){
  if (is.na(bird_traits$host[i]) == T && is.na(bird_traits$invasive[i]) == T ){ #if not invasive or host change to none
    bird_traits$none <- replace(bird_traits$none,i,'None')}
}

bird_traits<-select(bird_traits,-2)
bird_traits <- bird_traits %>% relocate(groups, .after=species)
write.csv(bird_traits,"../results/edited_bird_traits.csv",row.names = FALSE)

#fish
fish_ncbi <- fread("../results/fish_species_ncbi.csv") #21,075
fish_ncbi <- fish_ncbi[,3]

fish_names<-str_split_fixed(fish_ncbi$species, " ", 2) #for fishbase genus is captalized so have to split it apart first then capitalize then search in database 
fish_names<-as.data.frame(fish_names)
fish_names$V1 <- str_to_title(fish_names$V1) 
fish_names$species <- paste(fish_names$V1, fish_names$V2, sep=" ") # combine genus and species names 
fish <- fish_names$species
morph   = "Length"
fish_traits <- fb_tbl("species") %>% mutate(sci_name = paste(Genus, Species)) %>%  filter(sci_name %in% fish) %>% select(sci_name,morph)
fish_traits$sci_name<-tolower(fish_traits$sci_name)

group<-rep("N", each = nrow(fish_traits)) #Add a column with I = Invasive, H = Host, N = Neither , HI = Invasive and Host 
fish_traits$group<-group
fish_traits <- fish_traits%>%relocate(group, .after=sci_name)
host_fish<-intersect(host,fish_traits$sci_name) #271

for (i in 1:19162){
  x <- fish_traits$sci_name[i]
  y <- x %in% host_fish
  if (y==TRUE){ #if it is a host then change group N to H 
    fish_traits$group <- replace(fish_traits$group,i,'H')}
}

invasive_fish<-intersect(invasive,fish_traits$sci_name) #1380
for (i in 1:19162){
  x <- fish_traits$sci_name[i]
  y <- x %in% invasive_fish
  if (y==TRUE){ #if it is a invasive then change group N to I 
    fish_traits$group <- replace(fish_traits$group,i,'I')}
}

invasive_host_fish<-intersect(invasive_host$x,fish_traits$sci_name) #265
for (i in 1:19162){
  x <- fish_traits$sci_name[i]
  y <- x %in% invasive_fish
  if (y==TRUE){ #if it is a host and Invasive then change group N to HI
    fish_traits$group <- replace(fish_traits$group,i,'HI')}
}

fish_traits$none<-rep(NA,19162)
for (i in 1:19162){
  if (is.na(fish_traits$host[i]) == T && is.na(fish_traits$invasive[i]) == T ){ #if not invasive or host change to none
    fish_traits$none <- replace(fish_traits$none,i,'None')}
}

fish_traits <- fish_traits %>% relocate(groups, .after=species)
write.csv(fish_traits,"../results/edited_fish_traits.csv",row.names = FALSE)















