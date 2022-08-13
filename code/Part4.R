#1. Is the overlap driven by traits? Creating random sample Distribution plots 
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
#########################

#Mammal
mammal_traits<-fread("../results/edited_mammal_traits.csv")

#Mammal, trait 1: mass
mass<-select(mammal_traits,1,2,70:73) #4630, 
mass$mass.g<-log(mass$mass.g)
mass<-mass %>% drop_na(mass.g) #drop na in all rows , 4521

Nothing <- mass %>% select(mass.g)%>% filter (mass$none == 'None') #3565
Host <- mass %>% select(mass.g)%>% filter(mass$host == 'H') #805
Host_Invasive <- mass %>% select(mass.g)%>%filter(mass$host_invasive == 'HI') #189
Invasive <- mass %>% select(mass.g)%>%filter(mass$invasive == 'I') #340

sample_mass_N<-c() 
sample_mass_H<-c() 
sample_mass_I<-c() 
sample_mass_HI<-c() 
for (a in 1:500){
  print(a)
  mass_N<-sample(Nothing$mass.g,80,replace=F)
  sample_mass_N<-c(sample_mass_N,mass_N) 
  
  mass_H<-sample(Host$mass.g,80,replace=F)
  sample_mass_H<-c(sample_mass_H,mass_H) 
  
  mass_I<-sample(Invasive$mass.g,80,replace=F)
  sample_mass_I<-c(sample_mass_I,mass_I) 
  
  mass_HI<-sample(Host_Invasive$mass.g,80,replace=F)
  sample_mass_HI<-c(sample_mass_HI,mass_HI) 
}

sample_mass_N<-as.data.frame(sample_mass_N)
colnames(sample_mass_N)<-'mass'
sample_mass_N$group<-rep('N',40000)
sample_mass_H<-as.data.frame(sample_mass_H)
colnames(sample_mass_H)<-'mass'
sample_mass_H$group<-rep('H',40000)
mass<-rbind(sample_mass_N,sample_mass_H)
sample_mass_I<-as.data.frame(sample_mass_I)
colnames(sample_mass_I)<-'mass'
sample_mass_I$group<-rep('I',40000)
mass<-rbind(mass,sample_mass_I)
sample_mass_HI<-as.data.frame(sample_mass_HI)
colnames(sample_mass_HI)<-'mass'
sample_mass_HI$group<-rep('HI',40000)
mass<-rbind(mass,sample_mass_HI)

mu <- ddply(mass, "group", summarise, grp.med=median(mass))

#Mammal, trait 2: gestation
gestation<-select(mammal_traits,1,30,70:73) #4630
gestation<-gestation %>% drop_na(`9-1_GestationLen_d`) #drop na in all rows , 4521
Nothing_gestation <- gestation %>% select(`9-1_GestationLen_d`)%>% filter (gestation$none == 'None') #692
Host_gestation <- gestation %>% select(`9-1_GestationLen_d`)%>% filter(gestation$host == 'H') #459
Host_Invasive_gestation <- gestation %>% select(`9-1_GestationLen_d`)%>%filter(gestation$host_invasive == 'HI') #165
Invasive_gestation <- gestation %>% select(`9-1_GestationLen_d`)%>%filter(gestation$invasive == 'I') #281

sample_ges_N<-c() 
sample_ges_H<-c() 
sample_ges_I<-c() 
sample_ges_HI<-c() 

for (a in 1:500){
  print(a)
  mass_N<-sample(Nothing_gestation$`9-1_GestationLen_d`,80,replace=F)
  sample_ges_N<-c(sample_ges_N,mass_N) 
  
  mass_H<-sample(Host_gestation$`9-1_GestationLen_d`,80,replace=F)
  sample_ges_H<-c(sample_ges_H,mass_H) 
  
  mass_I<-sample(Invasive_gestation$`9-1_GestationLen_d`,80,replace=F)
  sample_ges_I<-c(sample_ges_I,mass_I) 
  
  mass_HI<-sample(Host_Invasive_gestation$`9-1_GestationLen_d`,80,replace=F)
  sample_ges_HI<-c(sample_ges_HI,mass_HI) 
}

sample_ges_N<-as.data.frame(sample_ges_N)
colnames(sample_ges_N)<-'gestation'
sample_ges_N$group<-rep('N',40000)
sample_ges_H<-as.data.frame(sample_ges_H)
colnames(sample_ges_H)<-'gestation'
sample_ges_H$group<-rep('H',40000)
gestation<-rbind(sample_ges_N,sample_ges_H)
sample_ges_I<-as.data.frame(sample_ges_I)
colnames(sample_ges_I)<-'gestation'
sample_ges_I$group<-rep('I',40000)
gestation<-rbind(gestation,sample_ges_I)
sample_ges_HI<-as.data.frame(sample_ges_HI)
colnames(sample_ges_HI)<-'gestation'
sample_ges_HI$group<-rep('HI',40000)
gestation<-rbind(gestation,sample_ges_HI)
gestation$gestation <- log(gestation$gestation)

mu_2 <- ddply(gestation, "group", summarise, grp.med=median(`gestation`))

#Mammal, trait 3: litter size
litter_size<-select(mammal_traits,1,35,70:73) #4630
litter_size<-litter_size %>% drop_na(`15-1_LitterSize`) #drop na in all rows , 4521
Nothing_litter <- litter_size %>% select(`15-1_LitterSize`)%>% filter (litter_size$none == 'None') #692
Host_litter <- litter_size %>% select(`15-1_LitterSize`)%>% filter(litter_size$host == 'H') #459
Host_Invasive_litter <- litter_size %>% select(`15-1_LitterSize`)%>%filter(litter_size$host_invasive == 'HI') #165
Invasive_litter <- litter_size %>% select(`15-1_LitterSize`)%>%filter(litter_size$invasive == 'I') #281

sample_ges_N<-c() 
sample_ges_H<-c() 
sample_ges_I<-c() 
sample_ges_HI<-c() 

for (a in 1:500){
  print(a)
  mass_N<-sample(Nothing_litter$`15-1_LitterSize`,80,replace=F)
  sample_ges_N<-c(sample_ges_N,mass_N) 
  
  mass_H<-sample(Host_litter$`15-1_LitterSize`,80,replace=F)
  sample_ges_H<-c(sample_ges_H,mass_H) 
  
  mass_I<-sample(Invasive_litter$`15-1_LitterSize`,80,replace=F)
  sample_ges_I<-c(sample_ges_I,mass_I) 
  
  mass_HI<-sample(Host_Invasive_litter$`15-1_LitterSize`,80,replace=F)
  sample_ges_HI<-c(sample_ges_HI,mass_HI) 
}

sample_ges_N<-as.data.frame(sample_ges_N)
colnames(sample_ges_N)<-'litter_size'
sample_ges_N$group<-rep('N',40000)
sample_ges_H<-as.data.frame(sample_ges_H)
colnames(sample_ges_H)<-'litter_size'
sample_ges_H$group<-rep('H',40000)
litter_size<-rbind(sample_ges_N,sample_ges_H)
sample_ges_I<-as.data.frame(sample_ges_I)
colnames(sample_ges_I)<-'litter_size'
sample_ges_I$group<-rep('I',40000)
litter_size<-rbind(litter_size,sample_ges_I)
sample_ges_HI<-as.data.frame(sample_ges_HI)
colnames(sample_ges_HI)<-'litter_size'
sample_ges_HI$group<-rep('HI',40000)
litter_size<-rbind(litter_size,sample_ges_HI)
litter_size$litter_size <- log(litter_size$litter_size)

mu_3 <- ddply(litter_size, "group", summarise, grp.med=median(`litter_size`))

#Mammal, trait 4: max longevity
longevity<-select(mammal_traits,1,37,70:73) 
longevity<-longevity %>% drop_na(`17-1_MaxLongevity_m`) #drop na in all rows , 4521
Nothing_longevity <- longevity %>% select(`17-1_MaxLongevity_m`)%>% filter (longevity$none == 'None') #
Host_longevity <- longevity %>% select(`17-1_MaxLongevity_m`)%>% filter(longevity$host == 'H') #
Host_Invasive_longevity <- longevity %>% select(`17-1_MaxLongevity_m`)%>%filter(longevity$host_invasive == 'HI') #154
Invasive_longevity <- longevity %>% select(`17-1_MaxLongevity_m`)%>%filter(longevity$invasive == 'I') #

sample_ges_N<-c() 
sample_ges_H<-c() 
sample_ges_I<-c() 
sample_ges_HI<-c() 

for (a in 1:500){
  print(a)
  mass_N<-sample(Nothing_longevity$`17-1_MaxLongevity_m`,80,replace=F)
  sample_ges_N<-c(sample_ges_N,mass_N) 
  
  mass_H<-sample(Host_longevity$`17-1_MaxLongevity_m`,80,replace=F)
  sample_ges_H<-c(sample_ges_H,mass_H) 
  
  mass_I<-sample(Invasive_longevity$`17-1_MaxLongevity_m`,80,replace=F)
  sample_ges_I<-c(sample_ges_I,mass_I) 
  
  mass_HI<-sample(Host_Invasive_longevity$`17-1_MaxLongevity_m`,80,replace=F)
  sample_ges_HI<-c(sample_ges_HI,mass_HI) 
}

sample_ges_N<-as.data.frame(sample_ges_N)
colnames(sample_ges_N)<-'max_longevity'
sample_ges_N$group<-rep('N',40000)
sample_ges_H<-as.data.frame(sample_ges_H)
colnames(sample_ges_H)<-'max_longevity'
sample_ges_H$group<-rep('H',40000)
longevity<-rbind(sample_ges_N,sample_ges_H)
sample_ges_I<-as.data.frame(sample_ges_I)
colnames(sample_ges_I)<-'max_longevity'
sample_ges_I$group<-rep('I',40000)
longevity<-rbind(longevity,sample_ges_I)
sample_ges_HI<-as.data.frame(sample_ges_HI)
colnames(sample_ges_HI)<-'max_longevity'
sample_ges_HI$group<-rep('HI',40000)
longevity<-rbind(longevity,sample_ges_HI)
longevity$max_longevity <- log(longevity$max_longevity)

mu_4 <- ddply(longevity, "group", summarise, grp.med=median(`max_longevity`))

#Plotting
#N vs H
plot1a<-ggplot(subset(mass, group %in% c("N", "H")), aes(x=mass, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual( labels = c("Host", "None"),values = c("#31a354","#3182bd")) +xlab("Body mass(log)")
plot1a <- plot1a + geom_vline(data=subset(mu, group %in% c("N", "H") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Host", "None"),values=c("#31a354","#3182bd"))
plot1a <- plot1a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot2a<-ggplot(subset(gestation, group %in% c("N", "H")), aes(x=gestation, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +xlab("Days of Gestation Length(log)") + scale_fill_manual( labels = c("Host", "None"),values = c("#31a354","#3182bd"))
plot2a <- plot2a + geom_vline(data=subset(mu_2, group %in% c("N", "H")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Host", "None"),values=c("#31a354","#3182bd"))
plot2a <- plot2a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot3a<-ggplot(subset(litter_size, group %in% c("N", "H")), aes(x=litter_size, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +xlab("Number of Offspring Born per Litter per Female(log)") + scale_fill_manual( labels = c("Host", "None"),values = c("#31a354","#3182bd"))
plot3a <- plot3a + geom_vline(data=subset(mu_3, group %in% c("N", "H")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Host", "None"),values=c("#31a354","#3182bd"))
plot3a <- plot3a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot4a<-ggplot(subset(longevity, group %in% c("N", "H")), aes(x=max_longevity, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +xlab("Maximum Adult Age(log)") + scale_fill_manual( labels = c("Host", "None"),values = c("#31a354","#3182bd"))
plot4a <- plot4a + geom_vline(data=subset(mu_4, group %in% c("N", "H")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Host", "None"),values=c("#31a354","#3182bd"))
plot4a <- plot4a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#N vs I
plot1b<-ggplot(subset(mass, group %in% c("N", "I")), aes(x=mass, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual( labels = c("Invasive", "None"),values = c("#8856a7","#3182bd")) +xlab("Body mass(log)")
plot1b <- plot1b + geom_vline(data=subset(mu, group %in% c("N", "I") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Invasive", "None"),values=c("#8856a7","#3182bd"))
plot1b <- plot1b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot2b<-ggplot(subset(gestation, group %in% c("N", "I")), aes(x=gestation, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +xlab("Days of Gestation Length(log)") + scale_fill_manual( labels = c("Invasive", "None"),values = c("#8856a7","#3182bd"))
plot2b <- plot2b + geom_vline(data=subset(mu_2, group %in% c("N", "I")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Invasive", "None"),values=c("#8856a7","#3182bd"))
plot2b <- plot2b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot3b<-ggplot(subset(litter_size, group %in% c("N", "I")), aes(x=litter_size, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +xlab("Number of Offspring Born per Litter per Female(log)") + scale_fill_manual( labels = c("Invasive", "None"),values = c("#8856a7","#3182bd"))
plot3b <- plot3b + geom_vline(data=subset(mu_3, group %in% c("N", "I")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Invasive", "None"),values=c("#8856a7","#3182bd"))
plot3b <- plot3b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot4b<-ggplot(subset(longevity, group %in% c("N", "I")), aes(x=max_longevity, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +xlab("Maximum Adult Age(log)")  + scale_fill_manual( labels = c("Invasive", "None"),values = c("#8856a7","#3182bd"))
plot4b <- plot4b + geom_vline(data=subset(mu_4, group %in% c("N", "I")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Invasive", "None"),values=c("#8856a7","#3182bd"))
plot4b <- plot4b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#H vs I vs HI
plot1c<-ggplot(subset(mass, group %in% c("H", "I","HI")), aes(x=mass, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7")) +xlab("Body mass(log)")
plot1c <- plot1c + geom_vline(data=subset(mu, group %in% c("H", "I","HI") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot1c <- plot1c + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot2c<-ggplot(subset(gestation, group %in% c("H", "I","HI")), aes(x=gestation, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual( labels = c("Host", "Host and Invasive","Invasive"),values = c("#31a354","#313030","#8856a7"))  +xlab("Days of Gestation Length(log)")
plot2c <- plot2c + geom_vline(data=subset(mu_2, group %in% c("H", "I","HI") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot2c <- plot2c + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot3c<-ggplot(subset(litter_size, group %in% c("H", "I","HI")), aes(x=litter_size, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +xlab("Number of Offspring Born per Litter per Female(log)") + scale_fill_manual( labels = c("Host", "Host and Invasive","Invasive"),values = c("#31a354","#313030","#8856a7"))
plot3c <- plot3c + geom_vline(data=subset(mu_3, group %in% c("H", "I","HI")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot3c <- plot3c + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot4c<-ggplot(subset(longevity, group %in% c("H", "I","HI")), aes(x=max_longevity, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +xlab("Maximum Adult Age(log)") + scale_fill_manual( labels = c("Host", "Host and Invasive","Invasive"),values = c("#31a354","#313030","#8856a7"))
plot4c <- plot4c + geom_vline(data=subset(mu_4, group %in% c("H", "I","HI")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8)  + scale_color_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot4c <- plot4c + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


pdf(file = "~/results/plots/mammal_trait_dist.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches
grid.arrange(plot1a, plot1b, plot1c, plot2a, plot2b,plot2c ,plot3a, plot3b, plot3c, plot4a, plot4b, plot4c, ncol=3, nrow =4)
dev.off()

#STATITICAL TEST - H0: the distribution of scores for the two groups are equal, HA: the distribution of scores for the two groups are not equal

# MASS
mass_1<- mass %>% filter(group == "N"| group =="H" ) #Host vs None
wilcox.test(mass~ group,data = mass_1, paired=FALSE) # distribution not equal (p<0.01)

mass_2<- mass %>% filter(group == "N"| group =="I" ) #Host vs None
wilcox.test(mass~ group,data = mass_2,exact = FALSE) # distribution not equal (p<0.01)

mass_3<-mass %>% filter(group == "I"| group =="HI"| group =="H"  ) 
kruskal.test(mass ~ group, data = mass_3) #Significantly different between these three groups (p<0.01)
pairwise.wilcox.test(mass_3$mass, mass_3$group,p.adjust.method = "BH")
#pairwise compairson shows that HI, I and H distributions all are significantly different from each other (p<0.01)

# GESTATION
gestation_1<- gestation %>% filter(group == "N"| group =="H" )
wilcox.test(gestation~ group,data = gestation_1,exact = FALSE) # distribution not equal (p<0.01)

gestation_2<- gestation %>% filter(group == "N"| group =="I" ) #Host vs None
wilcox.test(gestation~ group,data = gestation_2,exact = FALSE) # distribution not equal (p<0.01)

gestation_3<-gestation %>% filter(group == "I"| group =="HI"| group =="H"  ) 
kruskal.test(gestation ~ group, data = gestation_3) #Significantly different between these three groups (p<0.01)
pairwise.wilcox.test(gestation_3$gestation, gestation_3$group,p.adjust.method = "BH")
#pairwise comparison shows that only H and I , HI and I distributions are significantly different between each other (p<0.01), 
#Not H vs HI

#Litter
litter_1<- litter_size %>% filter(group == "N"| group =="H" )
wilcox.test(litter_size~ group,data = litter_1,exact = FALSE) # distribution not equal (p<0.01)

litter_2<- litter_size %>% filter(group == "N"| group =="I" ) #Host vs None
wilcox.test(litter_size ~ group,data = litter_2,exact = FALSE) # distribution not equal (p<0.01)

litter_3<-litter_size %>% filter(group == "I"| group =="HI"| group =="H"  ) 
kruskal.test(litter_size ~ group, data = litter_3) #Significantly different between these three groups (p<0.01)
pairwise.wilcox.test(gestation_3$gestation, gestation_3$group,p.adjust.method = "BH")
#pairwise comparison shows that only H and I , HI and I distributions are significantly different between each other (p<0.01), 
#Not H vs HI

#longevity
longevity_1<- longevity %>% filter(group == "N"| group =="H" )
wilcox.test(max_longevity~ group,data = longevity_1,exact = FALSE) # distribution not equal (p<0.01)

longevity_2<- longevity %>% filter(group == "N"| group =="I" ) #Host vs None
wilcox.test(max_longevity ~ group,data = longevity_2,exact = FALSE) # distribution not equal (p<0.01)

longevity_3<-longevity %>% filter(group == "I"| group =="HI"| group =="H"  ) 
kruskal.test(max_longevity ~ group, data = longevity_3) #Significantly different between these three groups (p<0.01)
pairwise.wilcox.test(longevity_3$max_longevity, longevity_3$group,p.adjust.method = "BH")
#pairwise compairson shows that HI, I and H distributions all are significantly different from each other (p<0.01)




#########################

#Birds
bird_traits<-fread("../results/edited_bird_traits.csv") #Data, 6791 species

#Birds, trait 1: HWI 
birds_HWI<- select(bird_traits,1,2,23:26)
Nothing <- birds_HWI %>% select(HWI)%>% filter (birds_HWI$none == 'None') #5678
Host <- birds_HWI %>% select(HWI)%>% filter(birds_HWI$host == 'H') #269
Host_Invasive <- birds_HWI %>% select(HWI)%>%filter(birds_HWI$host_invasive == 'HI') #160
Invasive <- birds_HWI %>% select(HWI)%>%filter(birds_HWI$invasive == 'I') #1004

sample_HWI_N<-c() 
sample_HWI_H<-c() 
sample_HWI_I<-c() 
sample_HWI_HI<-c() 

for (a in 1:500){
  print(a)
  HWI_N<-sample(Nothing$HWI,80,replace=F)
  sample_HWI_N<-c(sample_HWI_N,HWI_N) 
  
  HWI_H<-sample(Host$HWI,80,replace=F)
  sample_HWI_H<-c(sample_HWI_H,HWI_H) 
  
  HWI_I<-sample(Invasive$HWI,80,replace=F)
  sample_HWI_I<-c(sample_HWI_I,HWI_I) 
  
  HWI_HI<-sample(Host_Invasive$HWI,80,replace=F)
  sample_HWI_HI<-c(sample_HWI_HI,HWI_HI) 
}

sample_HWI_N<-as.data.frame(sample_HWI_N)
colnames(sample_HWI_N)<-'HWI'
sample_HWI_N$group<-rep('N',40000)
sample_HWI_H<-as.data.frame(sample_HWI_H)
colnames(sample_HWI_H)<-'HWI'
sample_HWI_H$group<-rep('H',40000)
HWI<-rbind(sample_HWI_N,sample_HWI_H)
sample_HWI_I<-as.data.frame(sample_HWI_I)
colnames(sample_HWI_I)<-'HWI'
sample_HWI_I$group<-rep('I',40000)
HWI<-rbind(HWI,sample_HWI_I)
sample_HWI_HI<-as.data.frame(sample_HWI_HI)
colnames(sample_HWI_HI)<-'HWI'
sample_HWI_HI$group<-rep('HI',40000)
HWI<-rbind(HWI,sample_HWI_HI)

mu <- ddply(HWI, "group", summarise, grp.med=median(HWI))

#Birds, trait 2: BODY LENGTH 
birds_length<- select(bird_traits,1,3,23:26)
birds_length<-birds_length %>% drop_na(`Body_mass(log)`) #drop na in all rows 
N_2 <- birds_length %>% select(`Body_mass(log)`)%>% filter(birds_length$none == 'None') #5678
H_2 <- birds_length %>% select(`Body_mass(log)`)%>% filter(birds_length$host == 'H') #109
HI_2 <- birds_length %>% select(`Body_mass(log)`)%>% filter(birds_length$host_invasive == 'HI') #465
I_2 <- birds_length %>% select(`Body_mass(log)`)%>% filter(birds_length$invasive == 'I') #539

sample_length_N<-c() 
sample_length_H<-c() 
sample_length_I<-c() 
sample_length_HI<-c() 

for (a in 1:500){
  print(a)
  length_N<-sample(N_2$`Body_mass(log)`,80,replace=F)
  sample_length_N<-c(sample_length_N,length_N) 
  
  length_H<-sample(H_2$`Body_mass(log)`,80,replace=F)
  sample_length_H<-c(sample_length_H,length_H) 
  
  length_I<-sample(I_2$`Body_mass(log)`,80,replace=F)
  sample_length_I<-c(sample_length_I,length_I) 
  
  length_HI<-sample(HI_2$`Body_mass(log)`,80,replace=F)
  sample_length_HI<-c(sample_length_HI,length_HI) 
}

sample_length_N<-as.data.frame(sample_length_N)
sample_length_N$group<-rep('N',40000)
sample_length_HI<-as.data.frame(sample_length_HI)
sample_length_HI$group<-rep('HI',40000)
sample_length_I<-as.data.frame(sample_length_I)
sample_length_I$group<-rep('I',40000)
sample_length_H<-as.data.frame(sample_length_H)
sample_length_H$group<-rep('H',40000)
colnames(mass)[2]<-"group"
colnames(sample_length_N)<-'length(log)'
colnames(sample_length_HI)<-'length(log)'
colnames(sample_length_I)<-'length(log)'
colnames(sample_length_H)<-'length(log)'
mass<-rbind(sample_length_N,sample_length_H)
mass<-rbind(mass,sample_length_I)
mass<-rbind(mass,sample_length_HI)

mu_2 <- ddply(mass, "group", summarise, grp.med=median(`length(log)`))


#Birds, trait 3: Litter or Clutch Size
litter_clutch<- select(bird_traits,1,18,23:26)
litter_clutch<-litter_clutch %>% drop_na(litter_or_clutch_size_n) #drop na in all rows 
N_3 <- litter_clutch %>% select(litter_or_clutch_size_n)%>% filter(litter_clutch$none == 'None') #5678
H_3 <- litter_clutch %>% select(litter_or_clutch_size_n)%>% filter(litter_clutch$host == 'H') #109
HI_3 <- litter_clutch %>% select(litter_or_clutch_size_n)%>% filter(litter_clutch$host_invasive == 'HI') #465
I_3 <- litter_clutch %>% select(litter_or_clutch_size_n)%>% filter(litter_clutch$invasive == 'I') #539

sample_litter_N<-c() 
sample_litter_H<-c() 
sample_litter_I<-c() 
sample_litter_HI<-c() 

for (a in 1:500){
  print(a)
  litter_N<-sample(N_3$litter_or_clutch_size_n,80,replace=F)
  sample_litter_N<-c(sample_litter_N,litter_N) 
  
  litter_H<-sample(H_3$litter_or_clutch_size_n,80,replace=F)
  sample_litter_H<-c(sample_litter_H,litter_H) 
  
  litter_I<-sample(I_3$litter_or_clutch_size_n,80,replace=F)
  sample_litter_I<-c(sample_litter_I,litter_I) 
  
  litter_HI<-sample(HI_3$litter_or_clutch_size_n,80,replace=F)
  sample_litter_HI<-c(sample_litter_HI,litter_HI) 
}

sample_litter_N<-as.data.frame(sample_litter_N)
sample_litter_N$group<-rep('N',40000)
sample_litter_HI<-as.data.frame(sample_litter_HI)
sample_litter_HI$group<-rep('HI',40000)
sample_litter_I<-as.data.frame(sample_litter_I)
sample_litter_I$group<-rep('I',40000)
sample_litter_H<-as.data.frame(sample_litter_H)
sample_litter_H$group<-rep('H',40000)
colnames(sample_litter_N)<-'Clutch Size Number'
colnames(sample_litter_HI)<-'Clutch Size Number'
colnames(sample_litter_I)<-'Clutch Size Number'
colnames(sample_litter_H)<-'Clutch Size Number'
litter_clutch<-rbind(sample_litter_N,sample_litter_H)
litter_clutch<-rbind(litter_clutch,sample_litter_I)
litter_clutch<-rbind(litter_clutch,sample_litter_HI)
colnames(litter_clutch)[2]<-"group"
litter_clutch$`Clutch Size Number`<-log(litter_clutch$`Clutch Size Number`)

mu_3 <- ddply(litter_clutch, "group", summarise, grp.med=median(`Clutch Size Number`))

#Birds, trait 4: egg mass
egg<- select(bird_traits,1,19,23:26)
egg<-egg %>% drop_na(egg_mass_g) #drop na in all rows , # 3403
N_4 <- egg %>% select(egg_mass_g)%>% filter(egg$none == 'None') #5678
H_4 <- egg %>% select(egg_mass_g)%>% filter(egg$host == 'H') #109
HI_4 <- egg %>% select(egg_mass_g)%>% filter(egg$host_invasive == 'HI') #465
I_4 <- egg %>% select(egg_mass_g)%>% filter(egg$invasive == 'I') #539

sample_egg_N<-c() 
sample_egg_H<-c() 
sample_egg_I<-c() 
sample_egg_HI<-c() 

for (a in 1:500){
  print(a)
  egg_N<-sample(N_4$egg_mass_g,80,replace=F)
  sample_egg_N<-c(sample_egg_N,egg_N) 
  
  egg_H<-sample(H_4$egg_mass_g,80,replace=F)
  sample_egg_H<-c(sample_egg_H,egg_H) 
  
  egg_I<-sample(I_4$egg_mass_g,80,replace=F)
  sample_egg_I<-c(sample_egg_I,egg_I) 
  
  egg_HI<-sample(HI_4$egg_mass_g,80,replace=F)
  sample_egg_HI<-c(sample_egg_HI,egg_HI) 
}

sample_egg_N<-as.data.frame(sample_egg_N)
sample_egg_N$group<-rep('N',40000)
sample_egg_HI<-as.data.frame(sample_egg_HI)
sample_egg_HI$group<-rep('HI',40000)
sample_egg_I<-as.data.frame(sample_egg_I)
sample_egg_I$group<-rep('I',40000)
sample_egg_H<-as.data.frame(sample_egg_H)
sample_egg_H$group<-rep('H',40000)
colnames(sample_egg_N)<-'Egg mass(g)'
colnames(sample_egg_HI)<-'Egg mass(g)'
colnames(sample_egg_I)<-'Egg mass(g)'
colnames(sample_egg_H)<-'Egg mass(g)'
egg<-rbind(sample_egg_N,sample_egg_H)
egg<-rbind(egg,sample_egg_I)
egg<-rbind(egg,sample_egg_HI)
colnames(egg)[2]<-"group"
egg$`Egg mass(g)`<-log(egg$`Egg mass(g)`)

mu_4 <- ddply(egg, "group", summarise, grp.med=median(`Egg mass(g)`))

#Plotting
#N vs H
plot1a<-ggplot(subset(HWI, group %in% c("N", "H")), aes(x=HWI, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + ylim(0.00,0.05) + scale_fill_manual( labels = c("Host", "None"),values = c("#31a354","#3182bd"))
plot1a <- plot1a + geom_vline(data=subset(mu, group %in% c("N", "H") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Host", "None"),values=c("#31a354","#3182bd"))
plot1a <- plot1a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot2a<-ggplot(subset(mass, group %in% c("N", "H")), aes(x=`length(log)`, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +xlab("Body mass(log)") + scale_fill_manual( labels = c("Host", "None"),values = c("#31a354","#3182bd"))
plot2a <- plot2a + geom_vline(data=subset(mu_2, group %in% c("N", "H")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Host", "None"),values=c("#31a354","#3182bd"))
plot2a <- plot2a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot3a<-ggplot(subset(litter_clutch, group %in% c("N", "H")), aes(x=`Clutch Size Number`, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual( labels = c("Host", "None"),values = c("#31a354","#3182bd"))
plot3a <- plot3a + geom_vline(data=subset(mu_3,group %in% c("N", "H")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8)  + scale_color_manual(labels = c("Host", "None"),values=c("#31a354","#3182bd"))
plot3a <- plot3a + xlab("Clutch Size(log)") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot4a<-ggplot(subset(egg, group %in% c("N", "H")), aes(x=`Egg mass(g)`, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual( labels = c("Host", "None"),values = c("#31a354","#3182bd"))
plot4a <- plot4a + geom_vline(data=subset(mu_4, group %in% c("N", "H")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8)  + scale_color_manual(labels = c("Host", "None"),values=c("#31a354","#3182bd"))
plot4a <- plot4a + xlab("Egg mass(log)") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#N vs I
plot1b<-ggplot(subset(HWI, group %in% c("N", "I")), aes(x=HWI, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + ylim(0.00,0.05) + scale_fill_manual( labels = c("Invasive", "None"),values = c("#8856a7","#3182bd"))
plot1b <- plot1b + geom_vline(data=subset(mu, group %in% c("N", "I") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Invasive", "None"),values=c("#8856a7","#3182bd"))
plot1b <- plot1b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot2b<-ggplot(subset(mass, group %in% c("N", "I")), aes(x=`length(log)`, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + xlab("Body mass(log)") + scale_fill_manual( labels = c("Invasive", "None"),values = c("#8856a7","#3182bd"))
plot2b <- plot2b + geom_vline(data=subset(mu_2, group %in% c("N", "I")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Invasive", "None"),values=c("#8856a7","#3182bd"))
plot2b <- plot2b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot3b<-ggplot(subset(litter_clutch, group %in% c("N", "I")), aes(x=`Clutch Size Number`, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual( labels = c("Invasive", "None"),values = c("#8856a7","#3182bd"))
plot3b <- plot3b + geom_vline(data=subset(mu_3,group %in% c("N", "I")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Invasive", "None"),values=c("#8856a7","#3182bd"))
plot3b <- plot3b + xlab("Clutch Size(log)") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot4b<-ggplot(subset(egg, group %in% c("N", "I")), aes(x=`Egg mass(g)`, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual( labels = c("Invasive", "None"),values = c("#8856a7","#3182bd"))
plot4b <- plot4b + geom_vline(data=subset(mu_4, group %in% c("N", "I")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Invasive", "None"),values=c("#8856a7","#3182bd"))
plot4b <- plot4b + xlab("Egg mass(log)") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#H vs I vs HI
plot1c<-ggplot(subset(HWI, group %in% c("H", "I","HI")), aes(x=HWI, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + ylim(0.00,0.05) +  scale_fill_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot1c <- plot1c + geom_vline(data=subset(mu, group %in% c("H", "I","HI") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot1c <- plot1c + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot2c<-ggplot(subset(mass, group %in% c("H", "I","HI")), aes(x=`length(log)`, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + xlab("Body mass(log)") +  scale_fill_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot2c <- plot2c + geom_vline(data=subset(mu_2, group %in% c("H", "I","HI")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot2c <- plot2c + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


plot3c<-ggplot(subset(litter_clutch, group %in% c("H", "I","HI")), aes(x=`Clutch Size Number`, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +  scale_fill_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot3c <- plot3c + geom_vline(data=subset(mu_3,group %in% c("H", "I","HI")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot3c <- plot3c + xlab("Clutch Size(log)") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot4c<-ggplot(subset(egg, group %in% c("H", "I","HI")), aes(x=`Egg mass(g)`, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) +  scale_fill_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot4c <- plot4c + geom_vline(data=subset(mu_4, group %in% c("H", "I","HI")), aes(xintercept=grp.med, color=group), linetype="dashed",size=0.8) + scale_color_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot4c <- plot4c + xlab("Egg mass(log)") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


pdf(file = "~/results/plots/bird_trait_dist.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches
grid.arrange(plot1a, plot1b, plot1c, plot2a, plot2b,plot2c ,plot3a, plot3b, plot3c, plot4a, plot4b, plot4c, ncol=3, nrow =4)
dev.off()


#STATITICAL TEST
# HWI
HWI_1<- HWI %>% filter(group == "N"| group =="H" ) #Host vs None
out_1<-tidy(wilcox.test(HWI~ group,data = HWI_1, paired=FALSE)) # distribution not equal (p<0.01)
out_1["group"]<-"None x Host"

HWI_2<- HWI %>% filter(group == "N"| group =="I" ) #Host vs None
out_2<-tidy(wilcox.test(HWI~ group,data = HWI_2,exact = FALSE)) # distribution not equal (p<0.01)
out_2["group"]<-"None x Invasive"

pvalue<-rbind(out_1,out_2)
pvalue["variable"]<-c("Hand Wing Index","Hand Wing Index")

HWI_3<-HWI %>% filter(group == "I"| group =="HI"| group =="H"  ) 
out_3<-tidy(kruskal.test(HWI ~ group, data = HWI_3)) #Significantly different between these three groups (p<0.01)
out_3b<-tidy(pairwise.wilcox.test(HWI_3$HWI, HWI_3$group,p.adjust.method = "BH"))
#pairwise compairson shows that HI, I and H distributions all are significantly different from each other (p<0.01)
#H vs HI (p=0.00016)

# MASS
mass_1<- mass %>% filter(group == "N"| group =="H" )
out_4<-tidy(wilcox.test(`length(log)`~ group,data = mass_1,exact = FALSE)) # distribution not equal (p<0.01)
out_4["group"]<-"None x Host"
out_4["variable"]<-"length(log)"

mass_2<- mass %>% filter(group == "N"| group =="I" ) #Host vs None
out_5<-tidy(wilcox.test(`length(log)`~ group,data = mass_2,exact = FALSE)) # distribution not equal (p<0.01)
out_5["group"]<-"None x Invasive"
out_5["variable"]<-"length(log)"

pvalue<-rbind(pvalue,out_4,out_5)

mass_3<-mass %>% filter(group == "I"| group =="HI"| group =="H"  ) 
out_6<-tidy(kruskal.test(`length(log)` ~ group, data = mass_3)) #Significantly different between these three groups (p<0.01)
out_6b<-tidy(pairwise.wilcox.test(mass_3$`length(log)`, mass_3$group,p.adjust.method = "BH"))
#pairwise compairson shows that HI, I and H distributions all are significantly different from each other (p<0.01)

#CLUTCH SIZE
litter_1<- litter_clutch %>% filter(group == "N"| group =="H" )
out_7<-tidy(wilcox.test(`Clutch Size Number`~ group,data = litter_1,exact = FALSE)) # distribution not equal (p<0.01)
out_7["group"]<-"None x Host"
out_7["variable"]<-"Clutch Size Number"

litter_2<- litter_clutch %>% filter(group == "N"| group =="I" ) #Host vs None
out_8<-tidy(wilcox.test(`Clutch Size Number` ~ group,data = litter_2,exact = FALSE)) # distribution not equal (p<0.01)
out_8["group"]<-"None x Invasive"
out_8["variable"]<-"Clutch Size Number"

pvalue<-rbind(pvalue,out_7,out_8)

litter_3<-litter_clutch %>% filter(group == "I"| group =="HI"| group =="H"  ) 
out_9<-tidy(kruskal.test(`Clutch Size Number` ~ group, data = litter_3)) #Significantly different between these three groups (p<0.01)
out_9b<-tidy(pairwise.wilcox.test(litter_3$`Clutch Size Number`, litter_3$group,p.adjust.method = "BH"))
#pairwise comparison shows that only H and I , HI and I distributions are significantly different between each other (p<0.01), 
#Not H vs HI

#EGG MASS
egg_1<- egg %>% filter(group == "N"| group =="H" )
out_10<-tidy(wilcox.test(`Egg mass(g)`~ group,data = egg_1,exact = FALSE)) # distribution not equal (p<0.01)
out_10["group"]<-"None x Host"
out_10["variable"]<-"Egg mass(g)"

egg_2<- egg %>% filter(group == "N"| group =="I" ) #Host vs None
out_11<-tidy(wilcox.test(`Egg mass(g)` ~ group,data = egg_2,exact = FALSE)) # distribution not equal (p<0.01)
out_11["group"]<-"None x Invasive"
out_11["variable"]<-"Egg mass(g)"

pvalue<-rbind(pvalue,out_10,out_11)

egg_3<-egg %>% filter(group == "I"| group =="HI"| group =="H"  ) 
out_12<-tidy(kruskal.test(`Egg mass(g)` ~ group, data = egg_3)) #Significantly different between these three groups (p<0.01)
out_12b<-tidy(pairwise.wilcox.test(egg_3$`Egg mass(g)`, egg_3$group,p.adjust.method = "BH"))
#pairwise comparison shows that only H and I , HI and I distributions are significantly different between each other (p<0.01), 
#Not H vs HI (p=0.28)







#########################

#Fish
fish_traits<-fread("../results/edited_fish_traits.csv") #Data, 6791 species

#Fish, trait 1: Length
length<-select(fish_traits,1,15,35:38) #4630
Nothing <- fish %>% select(Length)%>% filter (fish$none == 'None') 
Nothing<-Nothing %>% drop_na(Length) #drop na in all rows , 16,579
Host <- fish %>% select(Length)%>% filter(fish$host == 'H') 
Host<-Host %>% drop_na(Length) #drop na in all rows , 270
Host_Invasive <- fish %>% select(Length)%>%filter(fish$host_invasive == 'HI') #108
Host_Invasive<-Host_Invasive %>% drop_na(Length) #drop na in all rows , 108
Invasive <- fish %>% select(Length)%>%filter(fish$invasive == 'I') #340
Invasive<-Invasive %>% drop_na(Length) #drop na in all rows , 1361

sample_length_N<-c() 
sample_length_H<-c() 
sample_length_I<-c() 
sample_length_HI<-c() 

for (a in 1:500){
  print(a)
  mass_N<-sample(Nothing$Length,80,replace=F)
  sample_length_N<-c(sample_length_N,mass_N) 
  
  mass_H<-sample(Host$Length,80,replace=F)
  sample_length_H<-c(sample_length_H,mass_H) 
  
  mass_I<-sample(Invasive$Length,80,replace=F)
  sample_length_I<-c(sample_length_I,mass_I) 
  
  mass_HI<-sample(Host_Invasive$Length,80,replace=F)
  sample_length_HI<-c(sample_length_HI,mass_HI) 
}

sample_length_N<-as.data.frame(sample_length_N)
colnames(sample_length_N)<-'length'
sample_length_N$group<-rep('N',40000)
sample_length_H<-as.data.frame(sample_length_H)
colnames(sample_length_H)<-'length'
sample_length_H$group<-rep('H',40000)
length<-rbind(sample_length_N,sample_length_H)
sample_length_I<-as.data.frame(sample_length_I)
colnames(sample_length_I)<-'length'
sample_length_I$group<-rep('I',40000)
length<-rbind(length,sample_length_I)
sample_length_HI<-as.data.frame(sample_length_HI)
colnames(sample_length_HI)<-'length'
sample_length_HI$group<-rep('HI',40000)
length<-rbind(length,sample_length_HI)
length$length<-log(length$length)

mu <- ddply(length, "group", summarise,  grp.med=median(length))

#Plotting
#N vs I
plot1a<-ggplot(subset(length, group %in% c("N", "I")), aes(x=length, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual( labels = c("Invasive", "None"),values = c("#8856a7","#3182bd")) +xlab("Body length(log)")
plot1a <- plot1a + geom_vline(data=subset(mu, group %in% c("N", "I") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Invasive", "None"),values=c("#8856a7","#3182bd"))
plot1a <- plot1a + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#N vs H
plot1b<-ggplot(subset(length, group %in% c("N", "H")), aes(x=length, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual( labels = c("Host", "None"),values = c("#31a354","#3182bd")) +xlab("Body length(log)")
plot1b <- plot1b + geom_vline(data=subset(mu, group %in% c("N", "H") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Host", "None"),values=c("#31a354","#3182bd"))
plot1b <- plot1b + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#I vs H vs HI
plot1c<-ggplot(subset(length, group %in% c("H", "I","HI")), aes(x=length, colour = group)) + geom_density(lwd = 0.8, linetype = 1,adjust = 2) + scale_fill_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7")) +xlab("Body length(log)")
plot1c <- plot1c + geom_vline(data=subset(mu, group %in% c("H", "I","HI") ), aes(xintercept=grp.med, color=group), linetype="dashed", size=0.8) + scale_color_manual(labels = c("Host", "Host and Invasive","Invasive"),values=c("#31a354","#313030","#8856a7"))
plot1c <- plot1c + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

pdf(file = "~/results/plots/fish_trait_dist.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 6) # The height of the plot in inches
grid.arrange(plot1a, plot1b, plot1c, ncol=1, nrow =3)
dev.off()

#STATITICAL TEST
# LENGTH
mass_1<- length %>% filter(group == "N"| group =="H" ) #Host vs None
wilcox.test(length~ group,data = mass_1, paired=FALSE) # distribution not equal (p<0.01)

mass_2<- length %>% filter(group == "N"| group =="I" ) #Host vs None
wilcox.test(length~ group,data = mass_2,exact = FALSE) # distribution not equal (p<0.01)

mass_3<-length %>% filter(group == "I"| group =="HI"| group =="H"  ) 
kruskal.test(length ~ group, data = mass_3) #Significantly different between these three groups (p<0.01)
pairwise.wilcox.test(mass_3$length, mass_3$group,p.adjust.method = "BH")
#pairwise compairson shows that HI, I and H distributions all are significantly different from each other (p<0.01)









