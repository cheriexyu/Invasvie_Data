#1. Isolating from NCBI
library(data.table)
library(dplyr)

#Mammals
mammals<-read.csv("../data/NCBI/taxids1.csv")
mammals<-setDT(mammals)

#Need to strip first into 3 columns, id, rank, others
mammal_vector<-mammals$X40674..class..Mammalia

firsttwo<-function(x) strsplit(x," ")[[1]] #split to character vectors
firsttwo_synonyms_mammals<-lapply(mammal_vector,FUN = firsttwo)

first_four_mammals<-list()
for (m in 1:13047){
  first_four_mammals[[m]] <- firsttwo_synonyms_mammals[m][[1]][1:4]
}

mammals_DT<- as.data.frame(first_four_mammals) #13047
mammals_DT<- t(mammals_DT)
mammals_DT<- as.data.frame(mammals_DT)
out15<-as.data.frame(table(mammals_DT$V4)) #count to eliminate
eliminate<- mammals_DT %>% select(V1,V2,V3,V4) %>% filter(V4 == "sp." | V4 == "cf." | V4 == "unclassified" | V4 == "aff."| V4 == "environmental") #3661
mammals_DT <- mammals_DT[!(mammals_DT$V4 == "sp." | mammals_DT$V4 == "cf." | mammals_DT$V4 == "unclassified" | mammals_DT$V4 == "aff."| mammals_DT$V4 == "environmental"| mammals_DT$V4 == "gen."| mammals_DT$V4 == "gr."| mammals_DT$V4 == "sp.'c'"),]
#9374
mammals_DT<- na.omit(mammals_DT)  #7674
mammals_DT$species <- paste(mammals_DT$V3, mammals_DT$V4, sep=" ") # combine genus and species names 
mammals_DT <- mammals_DT[c("V1","V2","species")]

mammal_species_ncbi <- mammals_DT  %>% select(V1,V2,species) %>% filter(V2 == "[species]") #5682
mammal_species_ncbi <- distinct(mammal_species_ncbi, species, .keep_all = TRUE) #5483
mammal_species_ncbi$species = tolower(mammal_species_ncbi$species)
mammal_species_ncbi$species<-gsub("\\(|\\]", "", mammal_species_ncbi$species)
write.csv(mammal_species_ncbi,"../results/mammal_species_ncbi.csv", row.names = FALSE)


#Birds

birds<-read.csv("../data/NCBI/taxids2.csv")
birds<-setDT(birds)
birds_vector<-birds$X8782..class..Aves #get into a vector
firsttwo_synonyms_birds<-lapply(birds_vector,FUN = firsttwo) #isolate into individual vector words

first_four_birds<-list() #isolate first 4 vectors words
for (m in 1:17991){
  first_four_birds[[m]] <- firsttwo_synonyms_birds[m][[1]][1:4]
}

birds_DT<- t(as.data.frame(first_four_birds))
birds_DT <- as.data.frame(birds_DT)

out15<-as.data.frame(table(birds_DT$V4)) #count the things I need to eliminate
eliminate<- birds_DT %>% select(V1,V2,V3,V4) %>% filter(V4 == "sp." | V4 == "cf." | V4 == "unclassified" | V4 == "aff."| V4 == "environmental"| V4 == "gen.") #638

birds_DT <- birds_DT[!(birds_DT$V4 == "sp." | birds_DT$V4 == "cf." | birds_DT$V4 == "unclassified" | birds_DT$V4 == "aff."| birds_DT$V4 == "environmental"| birds_DT$V4 == "gen."),]
#17353
birds_DT<- na.omit(birds_DT)  #14,643
birds_DT$species <- paste(birds_DT$V3, birds_DT$V4, sep=" ") # combine genus and species names 
birds_DT <- birds_DT[c("V1","V2","species")]
birds_species_ncbi <- birds_DT  %>% select(V1,V2,species) %>% filter(V2 == "[species]") #10194
birds_species_ncbi <- distinct(birds_species_ncbi, species, .keep_all = TRUE) #10,097
birds_species_ncbi$species = tolower(birds_species_ncbi$species)
write.csv(birds_species_ncbi,"../results/birds_species_ncbi.csv", row.names = FALSE)


#Fishes
fish<-read.csv("../data/NCBI/taxids3.csv")
fish_vector <- fish$X7898..superclass..Actinopterygii
firsttwo_synonyms_fish<-lapply(fish_vector,FUN = firsttwo)

first_four_fish<-list() #isolate first 4 vectors words
for (m in 1:50945){
  first_four_fish[[m]] <- firsttwo_synonyms_fish[m][[1]][1:4]
}

fish_DT<-t(as.data.frame(first_four_fish))
fish_DT <- as.data.frame(fish_DT)

out2<-as.data.frame(table(fish_DT$V4)) #count the things I need to eliminate

fish_DT <- fish_DT[!(fish_DT$V4 == "sp." | fish_DT$V4 == "cf." | fish_DT$V4 == "unclassified" | fish_DT$V4 == "aff."| fish_DT$V4 == "environmental"| fish_DT$V4 == "gen."
                     | fish_DT$V4 == "gr." | fish_DT$V4 == "n." |fish_DT$V4 == "cf.perusii" |fish_DT$V4 == "sp.'minami-iso-suzumedai'" |fish_DT$V4 == "sp.R0101_021"
                     |fish_DT$V4 == "spp." | fish_DT$V4 == "spn." | fish_DT$V4 == "'giardi'" | fish_DT$V4 == "(Parauchenipterus)" | fish_DT$V4 == "Cross'" 
                     | fish_DT$V4 == "'polyacanthus"| fish_DT$V3 == "'Haplochromis'"| fish_DT$V3 == "cf." ),]
#27,485

fish_DT<- na.omit(fish_DT)  #22,060
fish_DT$species <- paste(fish_DT$V3, fish_DT$V4, sep=" ") # combine genus and species names 
fish_DT <- fish_DT[c("V1","V2","species")]

fish_species_ncbi <- fish_DT  %>% select(V1,V2,species) %>% filter(V2 == "[species]")
fish_species_ncbi$species<-gsub("\\(|\\]", "", fish_species_ncbi$species)#21398
fish_species_ncbi <- distinct(fish_species_ncbi, species, .keep_all = TRUE) #21,075
fish_species_ncbi$species = tolower(fish_species_ncbi$species)
write.csv(fish_species_ncbi,"../results/fish_species_ncbi.csv", row.names = FALSE)

#2. Is the overlap of invasive and host by random chance?

invasve_hosts<-read.csv("../results/invasive_hosts.csv") 
sinas_hosts<-invasve_hosts$x
sinas_hosts<-unique(SINAS_db$Taxon)
sinas_hosts<-tolower(sinas_hosts)

#mammal 
mammal_species_ncbi<-fread("../results/mammal_species_ncbi.csv")
mammal_species_ncbi$species

mammal_matched<-c() #number of matched species compared to SINAS database, after sampling 1000 times 
for (n in 1:1000){
  print(n)
  mammal_sample<-sample(mammal_species_ncbi$species,1655,replace=F)
  mammal_matched_number<-intersect(mammal_sample,sinas_hosts)
  mammal_matched<-c(mammal_matched,length(mammal_matched_number)) #append number to new variable
}

#birds
birds_species_ncbi<-fread("../results/birds_species_ncbi.csv")
birds_matched<-c() #number of matched species compared to SINAS database, after sampling 1000 times 
for (n in 1:1000){
  print(n)
  birds_sample<-sample(birds_species_ncbi$species,1063,replace=F)
  birds_matched_number<-intersect(birds_sample,sinas_hosts)
  birds_matched<-c(birds_matched,length(birds_matched_number)) #append number to new variable
}

#fish
fish_species_ncbi<-fread("../results/fish_species_ncbi.csv")
fish_matched<-c() #number of matched species compared to SINAS database, after sampling 1000 times 
for (n in 1:1000){
  print(n)
  fish_sample<-sample(fish_species_ncbi$species,618,replace=F)
  fish_matched_number<-intersect(fish_sample,sinas_hosts)
  fish_matched<-c(fish_matched,length(fish_matched_number)) #append number to new variable
}

mammal_matched <- as.data.frame(mammal_matched)
mammal_matched$group<-rep("Mammal",nrow(mammal_matched))
names(mammal_matched)[1] <- "Matched"
birds_matched<-as.data.frame(birds_matched)
birds_matched$group<-rep("Bird",nrow(birds_matched))
names(birds_matched)[1] <- "Matched"
fish_matched<-as.data.frame(fish_matched)
fish_matched$group<-rep("Fish",nrow(fish_matched))
names(fish_matched)[1] <- "Matched"

matched_df<-as.data.frame(mammal_matched)
matched_df<-rbind(matched_df,birds_matched)
matched_df<-rbind(matched_df,fish_matched)

cols <- c("#C0C0C0", "#808080", "#000000")
line<-c(293,526,276)
line<-as.data.frame(line)
group<-c("Mammal","Bird","Fish")
line<-cbind(line,group)

pdf(file = "~/results/plots/distribution.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches
ggplot(matched_df, aes(x=Matched, color=group, fill=group)) +
  geom_histogram(binwidth=2, alpha=0.5, position="identity")+
  scale_color_manual(values=cols)+ scale_fill_manual(values=cols)+
  theme_classic() + theme(legend.position="top") +
  geom_vline(data=line, aes(xintercept=line, color=group),
             linetype="dashed", size=0.5) +
  labs(x="number of species matched as an invasive host per each random sample", y = "frequency")
dev.off()





















