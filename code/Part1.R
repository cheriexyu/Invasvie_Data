#1. Finding the Class in Virion 
install.packages("data.table")
install.packages("vroom")
install.packages("tidyverse")
install.packages("taxize")

library(vroom)
library(tidyr)
library(data.table)
library(dplyr)
library(taxize)

#VIRION DataBase
virion <- vroom("../data/Virion.csv", na =c("","NA")) #load host disease database, replace all "" with NA
virion<-data.frame(virion$Host,virion$Virus,virion$HostClass)
virion<-virion %>% drop_na(virion.Host) #472981 , Drop all NA in Host 
virion<-virion %>% drop_na(virion.Virus) #471166 , Drop all NA in Virus

number_uniques<-unique(virion$virion.Host) #3673 unique disease hosts
host_class<-virion %>% distinct(virion.Host, .keep_all = TRUE) #Get a dataframe of the 3673 uniques with host class information
host_class<-as.data.table(host_class)
host_class[, .(.N), by = .("virion.HostClass")] #What are the class of 3,673 unique hosts?
fwrite(host_class,"../results/filling_in_class_order.csv")
#Filled in NA Manually, saved as filling_in_class.csv

overlap_2<-fread("../results/filling_in_class_order_complete.csv") #virion with class 
overlap_2<-overlap_2[-1,]
virion_class<-overlap_2[, .(.N), by = .(V4)]
virion_class$database<-rep("VIRION (Disease Hosts)",nrow(virion_class))

#2. Get Synonyms in Virion
hostnames<-unique(virion$virion.Host) #all 3673 host names, in a vector

##itis Database
itis_df3 = data.frame()
for(x in 1:3717){
  print(x)
  output<-synonyms(hostnames[x],db="itis")
  if (anyNA(output) == TRUE){ #NA if the database does not have any information on the species
    df3<-data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL, c(".id","sub_tsn","acc_tsn","syn_author","syn_name","syn_tsn"))))
    df3[1,]<-c(hostnames[x],'NA','NA','NA','NA','NA') 
  }
  else{df3<-synonyms_df(output)}
  if (dim(df3)[1] == 0 && dim(df3)[2] == 0){ #if the database does have information but no synonyms
    df3<-data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL, c(".id","sub_tsn","acc_tsn","syn_author","syn_name","syn_tsn"))))
    df3[1,]<-c(hostnames[x],'No_Synonym','No_Synonym','No_Synonym','No_Synonym','No_Synonym') 
  }
  
  itis_df3 <- bind_rows(itis_df3,df3)
}

#For error codes from hostname 2962 to 2968
test2<-data.frame(matrix(ncol=8,nrow=0, dimnames=list(NULL, c(".id","sub_tsn","acc_tsn","syn_author","syn_name","syn_tsn","acc_name","acc_author"))))
test2[1,]<-c("python anchietae",'No_Synonym','No_Synonym','No_Synonym','No_Synonym','No_Synonym','NA','NA')
test2[2,]<-c("python bivittatus",'No_Synonym','No_Synonym','No_Synonym','No_Synonym','No_Synonym','NA','NA')
test2[3,]<-c("python breitensteini",1094051,1094051,"Steindachner, 1880", "Python curtus breitensteini",635073,"Python breitensteini","Steindachner, 1880")
test2[4,]<-c("python brongersmai",1094052,1094052, "Stull, 1938","Python curtus brongersmai",635074,"Python brongersmai","Stull, 1938")
test2[5,]<-c("python curtus",634783,634783,"Schlegel, 1872","Python curtus curtus",635072,"Python curtus","Schlegel, 1872")
test2[6,]<-c("python molurus",202187,202187,"Linnaeus, 1758","Python molurus molurus",635271,"Python molurus","Linnaeus, 1758")
test2[7,]<-c("python molurus",202187,202187,"Linnaeus, 1758","Python molurus",1094067,"Python molurus","Linnaeus, 1758")
test2[8,]<-c("python regius",634784,634784,"Shaw, 1802","Boa regia",1094068,"Python regius","Shaw, 1802")

# Concatenate dataframes together to form final itis synonym master dataframe

final <- bind_rows(final,test2)
final <- bind_rows(final,itis_df3)
final <- head(final,-1)

write.csv(final,"../results/itis_data_synonyms_master.csv", row.names = FALSE)

itis_synonyms<-data.frame(final$.id,final$syn_name)
itis_synonyms<-itis_synonyms %>% mutate(index= as.numeric(factor(final..id))) #index based on hosts
itis_synonyms <- itis_synonyms %>% select(index, everything())
colnames(itis_synonyms)[2] <- "hosts"
colnames(itis_synonyms)[3] <- "itis_synonyms"
write.csv(itis_synonyms,"../results/itis_data_synonyms.csv", row.names = FALSE)

##eol Database
hostnames[60]
pageid <- eol_search(hostnames[60])$pageid[1] #taxon id of species on eol page 
is.na(pageid)
x <- eol_pages(taxonconceptID = pageid,synonyms = TRUE) 
x$synonyms #scientific names 
newdf1 <- as.data.frame(x[['synonyms']]) #extract synonym

eol_df1 = data.frame()
for(x in 1:3717){
  print(x)
  pageid<-eol_search(hostnames[x])$pageid[1]
  if(is.na(pageid) == TRUE){
    eol_synonym_df<-data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("hosts","synonym","relationship","resource"))))
    eol_synonym_df[1,]<-c(hostnames[x],'NA','NA','NA') 
  }
  else{
    out <- eol_pages(taxonconceptID = pageid,synonyms = TRUE) 
    eol_synonym_df <- as.data.frame(out[['synonyms']]) #extract synonym as a dataframe
    length_row<-nrow(eol_synonym_df) #get the number of rows
    hosts<-rep(hostnames[x],each=length_row) #repeat original names by number of rows
    eol_synonym_df <- cbind(hosts,eol_synonym_df) #add original names to the synonym dataset
  }
  
  eol_df1 <- bind_rows(eol_df1,eol_synonym_df) #bind to previous loop
}

write.csv(eol_df1,"../results/eol_data_synonyms_master.csv", row.names = FALSE) #Only 20 NA 

eol_data_synonyms <- data.frame(eol_df1$hosts,eol_df1$synonym)
eol_synonyms<-eol_data_synonyms[!duplicated(eol_df1$synonym), ] #delete duplicates
eol_synonyms<-cbind(1:29552,eol_synonyms)
colnames(eol_synonyms)[1] <- "index"
colnames(eol_synonyms)[2] <- "hosts"
colnames(eol_synonyms)[3] <- "eol_synonyms"

eol_synonyms<-eol_synonyms %>% mutate(index= as.numeric(factor(hosts))) # add index
write.csv(eol_synonyms,"../results/eol_data_synonyms.csv", row.names = FALSE)

#merge synonyms of eol and itis
extract_data<-eol_synonyms[[3]] #extract
firsttwo<-function(x) strsplit(x," ")[[1]][1:2] #split to character vectors, Isolate first two name from synonym columns 

firsttwo_synonyms<-lapply(extract_data,FUN = firsttwo)

return = c()
for (l in 1:length(firsttwo_synonyms)){
  out<- str_c(firsttwo_synonyms[[l]], collapse = " ")
  return <- c(return,out)
}

eol_synonyms[[3]] <- return
eol_synonyms<-eol_synonyms[!duplicated(eol_synonyms$eol_synonyms), ] #delete duplicates
write.csv(eol_synonyms,"../results/eol_data_synonyms_final.csv", row.names = FALSE)

itis_synonyms[[4]]<-rep("itis",8924)
setnames(itis_synonyms, "V4", "database")
eol_synonyms[[4]]<-rep("eol",13773)
setnames(eol_synonyms, "V4", "database")

Result <- merge(itis_synonyms,eol_synonyms, all=TRUE) #merge 
Result[,synonyms := na.omit(c(itis_synonyms,eol_synonyms)), 
       by = hosts][,c("itis_synonyms","eol_synonyms"):=NULL] #combine two columns omitting NA
Result<-Result[!duplicated(Result$synonyms), ] #delete duplicates
write.csv(Result,"../results/merged_synonyms.csv", row.names = FALSE)

#merge synonyms with SINAS database to find overlap of invasive hosts
all_synonyms  <- fread("../results/merged_synonyms.csv",header=TRUE)
all_synonyms$name_lower = tolower(all_synonyms$synonyms) #lowercase

SINAS_db <- fread("../data/SInAS_AlienSpeciesDB_2.4.1.csv")
SINAS_db$name_lower = tolower(SINAS_db$Taxon) #lowercase
SINAS_db<- SINAS_db %>% relocate(name_lower) #move to front of data.table 

overlap4<- semi_join(SINAS_db, all_synonyms, by = "name_lower") #10,249 introduction events are overlapped with a host with disease, SINASdb originally had 175,980 events
write.csv(overlap4,"../results/overlap.csv", row.names = FALSE) #SINAS events that overlaps with VIRION

invasive_hosts<-unique(overlap4$name_lower) #In the 10,249 introduction events, there are 1,253 unique species that are invasive and host to disease
fwrite(invasive_hosts,"../results/invasive_hosts.csv")

#3. Finding the Class in Invasive Hosts 
#Invasive Host Class
all_synonyms<-semi_join(x = all_synonyms, y = overlap_2, by = "hosts") 
out15<-left_join(x=overlap4,y=overlap_2, by = c("name_lower" = "hosts"))
write.csv(out15,"../results/overlap_with_class.csv", row.names = FALSE) #dataframe of SINAS overlap with invasive host + class information

overlap_w_class<-fread("../results/overlap_with_class.csv",header=TRUE) #invasive host with class info 
overlap_w_class<-data.table(overlap_w_class$name_lower,overlap_w_class$HostClass)
overlap_w_class<-overlap_w_class %>% distinct(V1, .keep_all = TRUE) 
overlap_class<-overlap_w_class[, .(.N), by = .(V2)] #What are the class of 1,253 unique hosts?
overlap_class$V2<-tolower(overlap_class$V2)
names(overlap_class)[1] <- "V4"
names(overlap_class)[2] <- "N"
overlap_class$database<-rep("Invasive Hosts",nrow(overlap_class))

#Plot
sinas_overlap<-rbind(virion_class,overlap_class)

pdf(file = "../results/plots/class_of_VIRION_and_hosts.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches
p<-ggplot(data=sinas_overlap, aes(x=V4, y=N, fill=database)) +
  geom_bar(stat="identity",position=position_dodge()) +
  theme_classic() + labs( x="class", y = "number of species")+
  theme(legend.position="top",legend.title=element_blank())+
  scale_fill_manual(values=c('black','lightgray'))
p
dev.off()

#4. Finding the number of invasive events based on country in SINAS
location<-SINAS_db[, .(.N), by = .(Location)]
location <- location[order(-N),]
bp<- ggplot(location[1:5,], aes(x="", y=N, fill=Location))+
  geom_bar(width = 1, stat = "identity")
bp
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
pie <- bp + coord_polar("y", start=0) + scale_fill_grey() +  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = N),position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Countries")) 
pie

pdf(file = "../results/plots/SINAS_events.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches
pie
dev.off()

#4. Finding the number of pathogens in each species 
disease<-fread("../data/trefle.csv") #Disease hosts froom VIRION
object1<-as.data.frame(table(disease$host))
object1 <- object1 %>% arrange(desc(Freq))
colnames(object1)[1]<-"Disease Host"
write.csv(object1,"../results/number_of_disease_in_host_interpolated.csv", row.names=F)

histogram<-ggplot(object1, aes(x=Freq)) + geom_histogram() + theme_classic() + 
  geom_histogram(color="black", fill="grey") +
  labs(x="total number of pathogens carried by a species", y = "frequency") +
  scale_x_continuous(breaks = seq(0,650, by = 100)) +
  scale_y_continuous(breaks = seq(0,200, by = 40)) 

pdf(file = "../results/plots/pathogen_species.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches
histogram
dev.off()

