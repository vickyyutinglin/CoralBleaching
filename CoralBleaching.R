### library
library(ggplot2)
library(tidyr)
library(vegan)
library(dplyr)
#library(colorspace)    
#library(RColorBrewer)    
library(fmsb)
library(ncdf4)
library(scales)
library(sp)
library(maps)
library(maptools)
library(ggthemes)
library(rgdal)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(grid)
### directory & data importation
setwd('C:/Users/Vicky/Dropbox/FRE - Vicky@DP/Publication/On the way to publish/12. XLQ mesophotic bleacing event/Data')
benthic_data <- read.csv('benthic_data.csv', sep = ',', header = T)

### major categories 
maj <- aggregate(benthic_data[,-c(1,2,3,10)], list(benthic_data$Major_cate), sum)
colnames(maj) <- colnames(benthic_data[,-c(2,3,10)])
maj.per <- (maj[,-1]/colSums(maj[,-1]))*100
rownames(maj.per) <- maj$Major_cate
maj.per



# Hard coral cover did not change bwtween 2022 & 2023
#maj.per.2022HC <- as.numeric(maj.per[4,c(1,2,3)])
#maj.per.2023HC <- as.numeric(maj.per[4,c(4,5,6)])
#shapiro.test(maj.per.2022HC)
#shapiro.test(maj.per.2023HC)
#var.test(maj.per.2022HC, maj.per.2023HC)
#t.test(maj.per.2022HC, maj.per.2023HC, var.equal = TRUE)

# Turf algae cover changed bwtween 2022 & 2023
#maj.per.2022TF <- as.numeric(maj.per[8,c(1,2,3)])
#maj.per.2023TF <- as.numeric(maj.per[8,c(4,5,6)])
#shapiro.test(maj.per.2022TF)
#shapiro.test(maj.per.2023TF)
#var.test(maj.per.2022TF, maj.per.2023TF)
#t.test(maj.per.2022TF, maj.per.2023TF, var.equal = TRUE)

# stacked + percent
maj_long <- gather(maj, transect, cover, X2022_T1, X2022_T2, X2022_T3, X2023_T1, X2023_T2, X2023_T3, factor_key=TRUE)
maj_long
maj.col <- c('#66c2a5','#b3b3b3','#e78ac3','#8da0cb','#fc8d62','#e5c494','#ffd92f','#a6d854')
ggplot(maj_long, aes(fill=Major_cate, y=cover, x=transect)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = maj.col)

# stacked + percent + bleaching
maj.ble <- benthic_data[,-c(2,10)]
maj.ble$maj.ble <- with(maj.ble, paste(Major_cate, Bleached_status, sep = "_"))
maj.ble$Major_cate <- NULL
maj.ble$Bleached_status <- NULL
maj.ble <- aggregate(maj.ble[,-7], list(as.character(maj.ble$maj.ble)), sum)
colnames(maj.ble) <- c('Major_Category_Bleached','X2022_T1', 'X2022_T2', 'X2022_T3', 'X2023_T1', 'X2023_T2', 'X2023_T3')

maj.ble.long <- gather(maj.ble, Transect, Absolute_cover, X2022_T1, X2022_T2, X2022_T3, X2023_T1, X2023_T2, X2023_T3, factor_key=TRUE)

maj.ble.seq <- c("Algae_",                                  
                 "Bare_substrate_",   
                 "CCA_Dead_CCA",  
                 "CCA_",                                    
                 "Hard_corals_Bleached",                    
                 "Hard_corals_Partly_Bleached",   
                 "Hard_corals_Health",
                 "Other_live_",                             
                 "Soft_corals_Bleached",                    
                 "Soft_corals_Partly_Bleached" ,   
                 "Soft_corals_Health",
                 "Sponges_Dead_Terpios"  ,      
                 "Sponges_" ,
                 "Turf_cyanobacteria_Dead_Endolithic_Algae",
                 "Turf_cyanobacteria_Dead_Turf" ,
                 "Turf_cyanobacteria_" )
Tran.seq <- factor(maj.ble.long$Transect , levels = c('X2023_T3', 'X2023_T2','X2023_T1','X2022_T3','X2022_T2','X2022_T1'))
Major_Category <- factor(maj.ble.long$Major_Category_Bleached, levels = maj.ble.seq)


maj.ble.col <- c('#66c2a5','#b3b3b3',"#E78AC399",'#e78ac3',"#8DA0CB66","#8DA0CBB3",'#8da0cb',
                 '#fc8d62',"#E5C49499","#E5C494CC",'#e5c494',"#FFD92FB3",'#ffd92f',"#A6D85466","#A6D854B3",'#a6d854')


adjustcolor('#8da0cb', alpha.f = 0.4) 

ggplot(maj.ble.long, aes(fill = Major_Category, y = Absolute_cover, x = Tran.seq)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = maj.ble.col) + 
  coord_flip() + 
  theme_classic()

maj.ble.2022 <- rowSums(maj.ble[,2:4])
maj.ble.2023 <- rowSums(maj.ble[,5:7])
Major_Category_Bleached <- maj.ble$Major_Category_Bleached
maj.ble.yr <- data.frame(Major_Category_Bleached, maj.ble.2022, maj.ble.2023)
colnames(maj.ble.yr) <- c('Major_Category_Bleached','X2022','X2023')

maj.ble.2022 <- 100*(maj.ble.yr[,2]/colSums(maj.ble.yr[,2:3]))
maj.ble.2023 <- 100*(maj.ble.yr[,3]/colSums(maj.ble.yr[,2:3]))
maj.ble.2223 <- data.frame(maj.ble.yr$Major_Category_Bleached, maj.ble.2022, maj.ble.2023)
sum(maj.ble.2223[5:7,2])
sum(maj.ble.2223[5:7,3])

maj.ble.yr.long <- gather(maj.ble.yr, Year, Absolute_cover, X2022, X2023, factor_key=TRUE)
Major_Category <- factor(maj.ble.yr.long$Major_Category_Bleached, levels = maj.ble.seq)
Year. <- factor(maj.ble.yr.long$Year , levels = c('X2023','X2022'))

ggplot(maj.ble.yr.long, aes(fill = Major_Category, y = Absolute_cover, x = Year.)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = maj.ble.col) + 
  coord_flip() + 
  theme_classic()


# bar plot comparing 2022 and 2023
maj.per.yr <- as.data.frame(t(maj.per))
maj.per.yr$year <- c(rep('2022',3),rep('2023',3))

maj.per.yr.mean1 <- maj.per.yr %>%
  group_by(year) %>%
  summarise(Algae = mean(Algae),
            Bare.substrate = mean(Bare_substrate),
            CCA = mean(CCA),
            Hard.corals = mean(Hard_corals),
            Other.life = mean(Other_live),
            Soft.corals = mean(Soft_corals),
            Sponges = mean(Sponges),
            Turf = mean(Turf_cyanobacteria))
maj.per.yr.mean1[2,-1]-maj.per.yr.mean1[1,-1]
maj.per.yr.mean <- gather(maj.per.yr.mean1, maj, value = 'cover', 2:9)

maj.per.yr.sd <- maj.per.yr %>%
  group_by(year) %>%
  summarise(Algae = sd(Algae),
            Bare.substrate = sd(Bare_substrate),
            CCA = sd(CCA),
            Hard.corals = sd(Hard_corals),
            Other.life = sd(Other_live),
            Soft.corals = sd(Soft_corals),
            Sponges = sd(Sponges),
            Turf = sd(Turf_cyanobacteria))
maj.per.yr.sd <- gather(maj.per.yr.sd, maj, value = 'cover', 2:9)


maj.per.yr.bar <- cbind(maj.per.yr.mean, maj.per.yr.sd$cover)
colnames(maj.per.yr.bar) <- c('Year', 'Major', 'Cover', 'SD')
maj.per.yr.bar

ggplot(maj.per.yr.bar, aes(x = Year, y = Cover, fill = Major)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Cover-SD, ymax = Cover+SD), position =  "dodge", width = 0.2) +
  facet_wrap(~Major, scales = "free")+
  scale_fill_manual(values = maj.col)



### labels
# label percent cover
benthic.data.label <- benthic_data[,-c(1,3,10)]
benthic.data.label <- aggregate(benthic.data.label[,-1], list(benthic.data.label$Label), sum)
Label <- benthic.data.label$Group.1
benthic.data.label$Group.1 <- NULL
benthic.data.label <- t(benthic.data.label)
colnames(benthic.data.label) <- Label

# PERMANOVA does not work: too few samples
#Year <- as.factor(c('2022','2022','2022','2023','2023','2023'))
#benthic.data.label.tr <- sqrt(benthic.data.label)
#benthic.data.label.bray <-vegdist(benthic.data.label.tr, method='bray')
#Permanove.benthic.label <-adonis2(benthic.data.label.bray ~ Year, data = as.data.frame(benthic.data.label.tr), permutations = 99, method="bray", strata = Year)
#Permanove.benthic.label

benthic.data.label.per <- as.data.frame((benthic.data.label/rowSums(benthic.data.label)[1])*100)
benthic.data.label.per.nmds <- benthic.data.label.per %>% 
  select_if(~any(. >=5)) # select the species with >5% to show in nmds


# nmds
set.seed(1)
nmds.benthic <- metaMDS(benthic.data.label, distance = "bray")   
nmds.benthic$stress
stressplot(nmds.benthic)

# extract the scores and convex hull of nmds
site.scores <- as.data.frame(scores(nmds.benthic)$sites)
site.scores$Year <- c('2022','2022','2022','2023','2023','2023')
  
species.scores <- as.data.frame(scores(nmds.benthic, "species"))
species.scores.t <- matrix(as.numeric(t(species.scores)), ncol= nrow(species.scores), nrow = ncol(species.scores))
colnames(species.scores.t) <- rownames(species.scores)
species.scores.t <- as.data.frame(species.scores.t)
species.scores.5 <- species.scores.t %>% select(contains(colnames(benthic.data.label.per.nmds)))
species.scores.5 <- as.data.frame(matrix(as.numeric(t(species.scores.5)), ncol= nrow(species.scores.5), nrow = ncol(species.scores.5)))
colnames(species.scores.5) <- c('NMDS1','NMDS2')
rownames(species.scores.5) <- colnames(benthic.data.label.per.nmds)  # select the species with >5% to show in nmds


hull <- site.scores %>%
  group_by(Year) %>%
  slice(chull(NMDS1,NMDS2))

# plot
ggplot() +
  geom_point(data = site.scores, aes(x = NMDS1, y = NMDS2, fill = Year),size=4,pch=21) +
  geom_polygon(data = hull, aes(x = NMDS1, y = NMDS2,fill = Year, alpha = 0.5)) +
  geom_text(data = species.scores.5, mapping = aes(x = NMDS1, y = NMDS2, label = rownames(species.scores.5)),alpha = 0.6, size = 4) +
  theme_minimal() +
  theme(legend.position = "right",text = element_text(size = 24)) +
  scale_x_continuous(name="nMDS 1") +
  scale_y_continuous(name="nMDS 2") 


benthic.data.label$year <- c('2022','2022','2022','2023','2023','2023')   
benthic.data.label.dist <- benthic.data.label
adonis2(benthic.data.label ~ benthic.data.label$year, data = benthic.data.label, permutations = 999, method = "bray", strata = year)

### coral assemblage
HC <- benthic_data %>% filter(Major_cate=='Hard_corals')
SC <- benthic_data %>% filter(Major_cate=='Soft_corals')
Coral <- benthic_data %>% filter(Major_cate=='Soft_corals'|Major_cate=='Hard_corals')
Coral$Major_cate <- NULL
Coral$Bleached_status <- NULL
Coral$Label_raw <- NULL

Coral.per <- aggregate(Coral[,-1], unique(list(Coral$Label)), sum)
rownames(Coral.per) <- Coral.per$Group.1
Coral.per[,1]<-NULL
Coral.per <- apply(Coral.per,2,function(x) {100*(x/sum(x))})

mean(Coral.per[1,1:3])
sd(Coral.per[1,1:3])
mean(Coral.per[1,4:6])
sd(Coral.per[1,4:6])

Ana <- Coral.per[2:3,]
Ana <- colSums(Ana)

mean(Ana[1:3])
sd(Ana[1:3])
mean(Ana[4:6])
sd(Ana[4:6])



Coral.yr.cover <- as.data.frame(t(Coral.per))
Coral.yr.cover$year <- c(rep('2022',3),rep('2023',3))

Coral.yr.cover.mean1 <- Coral.yr.cover %>%
  group_by(year) %>%
  summarise(Acropora_tenella_branching = mean(Acropora_tenella_branching),
            Anacropora_forbesi_branching = mean(Anacropora_forbesi_branching),
            Anacropora_matthaii_pillai_branching = mean(Anacropora_matthaii_pillai_branching),
            Astreopora_spp_massive = mean(Astreopora_spp_massive),
            Cyphastrea_spp_massive = mean(Cyphastrea_spp_massive),
            Favites_spp_ecrusting = mean(Favites_spp_ecrusting),
            Goniopora_spp_massive = mean(Goniopora_spp_massive),
            Litophyton_spp_bushy = mean(Litophyton_spp_bushy),
            Lobophyllia_hemprichii_massive = mean(Lobophyllia_hemprichii_massive),
            Lobophyllia_spp_encrusting = mean(Lobophyllia_spp_encrusting),
            Montipora_spp_encrusting = mean(Montipora_spp_encrusting),
            Montipora_spp_massive = mean(Montipora_spp_massive),
            Poccilopora_spp_bushy = mean(Poccilopora_spp_bushy),
            Sarcophyton_spp_massive = mean(Sarcophyton_spp_massive),
            stony_coral_encrusting = mean(stony_coral_encrusting),
            Stylophora_pistillata_branching =mean(Stylophora_pistillata_branching))

as.data.frame(Coral.yr.cover.mean1)

Coral.yr.cover.sd1 <- Coral.yr.cover %>%
  group_by(year) %>%
  summarise(Acropora_tenella_branching = sd(Acropora_tenella_branching),
            Anacropora_forbesi_branching = sd(Anacropora_forbesi_branching),
            Anacropora_matthaii_pillai_branching = sd(Anacropora_matthaii_pillai_branching),
            Astreopora_spp_massive = sd(Astreopora_spp_massive),
            Cyphastrea_spp_massive = sd(Cyphastrea_spp_massive),
            Favites_spp_ecrusting = sd(Favites_spp_ecrusting),
            Goniopora_spp_massive = sd(Goniopora_spp_massive),
            Litophyton_spp_bushy = sd(Litophyton_spp_bushy),
            Lobophyllia_hemprichii_massive = sd(Lobophyllia_hemprichii_massive),
            Lobophyllia_spp_encrusting = sd(Lobophyllia_spp_encrusting),
            Montipora_spp_encrusting = sd(Montipora_spp_encrusting),
            Montipora_spp_massive = sd(Montipora_spp_massive),
            Poccilopora_spp_bushy = sd(Poccilopora_spp_bushy),
            Sarcophyton_spp_massive = sd(Sarcophyton_spp_massive),
            stony_coral_encrusting = sd(stony_coral_encrusting),
            Stylophora_pistillata_branching = sd(Stylophora_pistillata_branching))

as.data.frame(Coral.yr.cover.sd1)



# stacked + percent
Coral_long <- gather(Coral, Transect, Relative_cover, X2022_T1, X2022_T2, X2022_T3, X2023_T1, X2023_T2, X2023_T3, factor_key=TRUE)
Coral_long.seq <- c("Acropora_tenella_branching",          
                    "Anacropora_forbesi_branching",        
                    "Anacropora_matthaii&pillai_branching",
                    "Stylophora_pistillata_branching",
                    "Poccilopora_spp_bushy",
                    "Astreopora_spp_massive",              
                    "Cyphastrea_spp_massive",              
                    "Goniopora_spp_massive",               
                    "Lobophyllia_hemprichii_massive",  
                    "Montipora_spp_massive",
                    "Favites_spp_ecrusting",
                    "Lobophyllia_spp_encrusting",          
                    "Montipora_spp_encrusting",  
                    "stony_coral_encrusting",
                    "Litophyton_spp_bushy",
                    "Sarcophyton_spp_massive")    
Coral.seq <- factor(Coral_long$Label, levels = Coral_long.seq)
Tran.seq <- factor(Coral_long$Transect , levels = c('X2023_T3', 'X2023_T2','X2023_T1','X2022_T3','X2022_T2','X2022_T1'))
coral.col <- c('#3288bd',"#3288BDB3","#3288BD66","#3288BD1A",'#99d594',
               '#fee08b',"#FEE08BCC","#FEE08B99","#FEE08B66","#FEE08B33", 
               '#fc8d59',"#FC8D59CC","#FC8D5999","#FC8D5966",'#d53e4f','#9e0142')

adjustcolor('#3288bd', alpha.f = 0.1) 
ggplot(Coral_long, aes(fill = Coral.seq, y = Relative_cover, x = Tran.seq)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = coral.col) +
  coord_flip() + 
  theme_classic()



Coral.2022 <- rowSums(Coral[,2:4])
Coral.2023 <- rowSums(Coral[,5:7])
SpeciesXMorpho <- Coral$Label
Coral.yr <- data.frame(SpeciesXMorpho, Coral.2022, Coral.2023)
colnames(Coral.yr) <- c('SpeciesXMorpho','X2022','X2023')

Coral.yr <- aggregate(Coral.yr[,-1], unique(list(Coral$Label)), sum)
colnames(Coral.yr) <- c('SpeciesXMorpho','X2022','X2023')

coral.2223 <- 100*(Coral.yr[,2:3]/colSums(Coral.yr[,2:3]))
coral.2223 <- data.frame(Coral.yr$SpeciesXMorpho, coral.2223)
coral.2223

Coral.yr.long <- gather(Coral.yr, Year, Relative_cover, X2022, X2023, factor_key=TRUE)
SpeciesXMorpho <- factor(Coral.yr$SpeciesXMorpho, levels = Coral_long.seq)
Year. <- factor(Coral.yr.long$Year , levels = c('X2023','X2022'))
Coral.seq.yr <- factor(Coral.yr.long$SpeciesXMorpho, levels = Coral_long.seq)

ggplot(Coral.yr.long, aes(fill = Coral.seq.yr, y = Relative_cover, x = Year.)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = coral.col) + 
  coord_flip() + 
  theme_classic()



### bleaching conditions
# radar chart indicates the change in coral bleaching condition 
bleaching <- aggregate(benthic_data[,-c(1,2,3,10)], list(benthic_data$Bleached_status), sum)
bleaching <- t(bleaching)
bleaching <- bleaching[,-1]
colnames(bleaching) <- bleaching[1,]

pre.bleaching <- as.data.frame(matrix(as.numeric(bleaching[-1,]), ncol = 7, nrow = 6)[c(1:3),])
colnames(pre.bleaching) <- colnames(bleaching)
pre.bleaching.per <- 100*(pre.bleaching/rowSums(pre.bleaching))

pre.bleaching.mean <- pre.bleaching.per %>%
  summarise(Bleached = mean(Bleached),
            Dead_CCA = mean(Dead_CCA),
            Dead_Endolithic_Algae = mean(Dead_Endolithic_Algae),
            Dead_Terpios = mean(Dead_Terpios),
            Dead_Turf = mean(Dead_Turf),
            Health = mean(Health),
            Partly_Bleached = mean(Partly_Bleached))

pre.bleaching.sd <- pre.bleaching.per %>%
  summarise(Bleached = sd(Bleached),
            Dead_CCA = sd(Dead_CCA),
            Dead_Endolithic_Algae = sd(Dead_Endolithic_Algae),
            Dead_Terpios = sd(Dead_Terpios),
            Dead_Turf = sd(Dead_Turf),
            Health = sd(Health),
            Partly_Bleached = sd(Partly_Bleached))

var <- colnames(pre.bleaching.per)
pre.mean <- t(pre.bleaching.mean)
pre.upper <- t(pre.bleaching.mean) + t(pre.bleaching.sd) 
pre.lower <- t(pre.bleaching.mean) - t(pre.bleaching.sd)
pre.bleaching.rad <- data.frame(var, pre.mean, pre.upper, pre.lower)




post.bleaching <- as.data.frame(matrix(as.numeric(bleaching[-1,]), ncol = 7, nrow = 6)[c(4:6),])
colnames(post.bleaching) <- colnames(bleaching)
post.bleaching.per <- 100*(post.bleaching/rowSums(post.bleaching))

post.bleaching.mean <- post.bleaching.per %>%
  summarise(Bleached = mean(Bleached),
            Dead_CCA = mean(Dead_CCA),
            Dead_Endolithic_Algae = mean(Dead_Endolithic_Algae),
            Dead_Terpios = mean(Dead_Terpios),
            Dead_Turf = mean(Dead_Turf),
            Health = mean(Health),
            Partly_Bleached = mean(Partly_Bleached))

post.bleaching.sd <- post.bleaching.per %>%
  summarise(Bleached = sd(Bleached),
            Dead_CCA = sd(Dead_CCA),
            Dead_Endolithic_Algae = sd(Dead_Endolithic_Algae),
            Dead_Terpios = sd(Dead_Terpios),
            Dead_Turf = sd(Dead_Turf),
            Health = sd(Health),
            Partly_Bleached = sd(Partly_Bleached))


post.mean <- t(post.bleaching.mean)
post.upper <- t(post.bleaching.mean) + t(post.bleaching.sd) 
post.lower <- t(post.bleaching.mean) - t(post.bleaching.sd)
bleaching.rad <- data.frame(var, pre.mean, pre.upper, pre.lower,
                                 post.mean, post.upper, post.lower)

bleaching.mean.sd <- data.frame( t(pre.bleaching.mean), t(pre.bleaching.sd),
                                 t(post.bleaching.mean), t(post.bleaching.sd))


ggplot(bleaching.rad, aes(x = var, y = pre.mean, group = 1)) +
  geom_polygon(fill = NA, colour = '#fbb4ae') +
  geom_polygon(aes(y = pre.upper), fill = adjustcolor('#fbb4ae', alpha.f = 0.5)) +
  geom_polygon(aes(y = pre.lower), fill = 'white') +
 
  geom_polygon(aes(x = var, y = post.mean, group = 1), fill = NA, colour = '#8dd3c7') +
  geom_polygon(aes(y = post.upper), fill = adjustcolor( '#8dd3c7', alpha.f = 0.5)) +
  geom_polygon(aes(y = post.lower), fill = 'white') +
  
  theme_light() +
  theme() + 
  coord_polar() +
  labs(x = "", y = "")


# radar plot
pre.bleaching <- matrix(as.numeric(bleaching[-1,]), ncol = 7, nrow = 6)[c(1:3),]
colnames(pre.bleaching) <- colnames(bleaching)
pre.bleaching.per <- 100*(colSums(pre.bleaching)/sum(pre.bleaching))

bleaching.per.radar <- as.data.frame(rbind(rep(60,7), rep(0,7), pre.bleaching.per, post.bleaching.per))


post.bleaching <- matrix(as.numeric(bleaching[-1,]), ncol = 7, nrow = 6)[c(4:6),]
colnames(post.bleaching) <- colnames(bleaching)
post.bleaching.per <- 100*(colSums(post.bleaching)/sum(post.bleaching))

bleaching.per.radar <- as.data.frame(rbind(rep(60,7), rep(0,7), pre.bleaching.per, post.bleaching.per))

# Color vector
colors_border=c('#8dd3c7','#fbb4ae')
colors_in=c("#8DD3C7B3", "#FBB4AEB3")
adjustcolor('#fbb4ae', alpha.f = 0.7)

# plot with default options:
radarchart( bleaching.per.radar  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="darkgrey", cglty=1, axislabcol="grey", caxislabels=seq(0,60,15), cglwd=0.8,
            #custom labels
            vlcex=1.5, title=paste("Coral bleaching status"), cex.main = 3
)
legend(x=1.2, y=1.2, legend = c('2022 summer','2023 summer'), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.6, pt.cex=4)

# radar chart indicates the change in bleaching condition of Acropora tenella
AT.coral <- benthic_data[c(1:7),c(4:9)]
AT.coral <- t(AT.coral)
colnames(AT.coral) <- benthic_data[c(1:7),3]

AT.pre.bleaching <- matrix(as.numeric(AT.coral), ncol = 7, nrow = 6)[c(1:3),]
colnames(AT.pre.bleaching) <- colnames(AT.coral)
AT.pre.bleaching.per <- 100*(colSums(AT.pre.bleaching)/sum(AT.pre.bleaching))

AT.post.bleaching <- matrix(as.numeric(AT.coral), ncol = 7, nrow = 6)[c(4:6),]
colnames(AT.post.bleaching) <- colnames(AT.coral)
AT.post.bleaching.per <- 100*(colSums(AT.post.bleaching)/sum(AT.post.bleaching))

AT.bleaching.per.radar <- as.data.frame(rbind(rep(60,7), rep(0,7), AT.pre.bleaching.per, AT.post.bleaching.per))


# plot with default options:
radarchart( AT.bleaching.per.radar  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,60,15), cglwd=0.8,
            #custom labels
            vlcex=1.5, title=paste("Acropora tenella bleaching status"), cex.main = 3
)
legend(x=1.2, y=1.2, legend = c('2022 summer','2023 summer'), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.6, pt.cex=4)




AT.pre.bleaching <- matrix(as.numeric(AT.coral), ncol = 7, nrow = 6)[c(1:3),]
colnames(AT.pre.bleaching) <- colnames(AT.coral)
AT.pre.bleaching.per <- as.data.frame(100*(AT.pre.bleaching/rowSums(AT.pre.bleaching)))

AT.pre.bleaching.per.mean <- AT.pre.bleaching.per %>%
  summarise(Bleached = mean(Bleached),
            Dead_CCA = mean(Dead_CCA),
            Dead_Endolithic_Algae = mean(Dead_Endolithic_Algae),
            Dead_Terpios = mean(Dead_Terpios),
            Dead_Turf = mean(Dead_Turf),
            Health = mean(Health),
            Partly_Bleached = mean(Partly_Bleached))

AT.pre.bleaching.per.sd <- AT.pre.bleaching.per %>%
  summarise(Bleached = sd(Bleached),
            Dead_CCA = sd(Dead_CCA),
            Dead_Endolithic_Algae = sd(Dead_Endolithic_Algae),
            Dead_Terpios = sd(Dead_Terpios),
            Dead_Turf = sd(Dead_Turf),
            Health = sd(Health),
            Partly_Bleached = sd(Partly_Bleached))
AT.pre.mean <- t(AT.pre.bleaching.per.mean)
AT.pre.upper <- t(AT.pre.bleaching.per.mean) + t(AT.pre.bleaching.per.sd) 
AT.pre.lower <- t(AT.pre.bleaching.per.mean) - t(AT.pre.bleaching.per.sd)

AT.post.bleaching <- matrix(as.numeric(AT.coral), ncol = 7, nrow = 6)[c(4:6),]
colnames(AT.post.bleaching) <- colnames(AT.coral)
AT.post.bleaching.per <- as.data.frame(100*(AT.post.bleaching/rowSums(AT.post.bleaching)))
AT.post.bleaching.mean <- AT.post.bleaching.per %>%
  summarise(Bleached = mean(Bleached),
            Dead_CCA = mean(Dead_CCA),
            Dead_Endolithic_Algae = mean(Dead_Endolithic_Algae),
            Dead_Terpios = mean(Dead_Terpios),
            Dead_Turf = mean(Dead_Turf),
            Health = mean(Health),
            Partly_Bleached = mean(Partly_Bleached))

AT.post.bleaching.sd <- AT.post.bleaching.per %>%
  summarise(Bleached = sd(Bleached),
            Dead_CCA = sd(Dead_CCA),
            Dead_Endolithic_Algae = sd(Dead_Endolithic_Algae),
            Dead_Terpios = sd(Dead_Terpios),
            Dead_Turf = sd(Dead_Turf),
            Health = sd(Health),
            Partly_Bleached = sd(Partly_Bleached))

AT.post.bleaching.per %>%
  summarise(Daed.mean = mean(Dead_CCA + Dead_Endolithic_Algae + Dead_Terpios + Dead_Turf),
            Daed.sd = sd(Dead_CCA + Dead_Endolithic_Algae + Dead_Terpios + Dead_Turf))

AT.post.mean <- t(AT.post.bleaching.mean)
AT.post.upper <- t(AT.post.bleaching.mean) + t(AT.post.bleaching.sd) 
AT.post.lower <- t(AT.post.bleaching.mean) - t(AT.post.bleaching.sd)
AT.bleaching.rad <- data.frame(var,AT.pre.mean, AT.pre.upper, AT.pre.lower,
                               AT.post.mean, AT.post.upper, AT.post.lower)


AT.pre.bleaching.per %>%
  summarise(Bleached.mean = mean(Bleached + Partly_Bleached),
            Bleached.sd = sd(Bleached + Partly_Bleached))


ggplot(AT.bleaching.rad, aes(x = var, y = AT.post.mean, group = 1)) +
  geom_polygon(fill = NA, colour = '#fbb4ae') +
  geom_polygon(aes(y = AT.pre.upper), fill = adjustcolor('#fbb4ae', alpha.f = 0.5)) +
  geom_polygon(aes(y = AT.pre.lower), fill = 'white') +
  
  geom_polygon(aes(x = var, y = AT.post.mean, group = 1), fill = NA, colour = '#8dd3c7') +
  geom_polygon(aes(y = AT.post.upper), fill = adjustcolor('#8dd3c7', alpha.f = 0.5)) +
  geom_polygon(aes(y = AT.post.lower), fill = 'white') +
  
  theme_light() +
  theme() + 
  coord_polar() +
  labs(x = "", y = "")



AT.bleaching.per.radar <- as.data.frame(rbind(rep(60,7), rep(0,7), AT.pre.bleaching.per, AT.post.bleaching.per))





AN.coral <- benthic_data[c(8:14),c(4:9)]
AN.coral <- t(AN.coral)
colnames(AN.coral) <- benthic_data[c(8:14),3]
Dead_Turf <- AN.coral[,4] + AN.coral[,7]
AN.coral <- AN.coral[,-4] 
AN.coral <- as.data.frame(AN.coral[,-6] )
AN.coral$Dead_Turf <- Dead_Turf

#AN.pre.bleaching <- matrix(AN.coral, ncol = 6, nrow = 6)[c(1:3),]
#colnames(AN.pre.bleaching) <- colnames(AN.coral)
AN.pre.bleaching <- AN.coral[c(1:3),]
AN.pre.bleaching.per <- 100*(colSums(AN.pre.bleaching)/sum(AN.pre.bleaching))

#AN.post.bleaching <- matrix(as.numeric(AN.coral), ncol = 6, nrow = 6)[c(4:6),]
#colnames(AN.post.bleaching) <- colnames(AN.coral)
AN.post.bleaching <- as.data.frame(AN.coral[c(4:6),])
AN.post.bleaching.per <- 100*(colSums(AN.post.bleaching)/sum(AN.post.bleaching))
typeof(AN.post.bleaching.per)



AN.post.bleaching.mean <- AN.post.bleaching.per %>%
  summarise(Bleached = mean(Bleached),
            Dead_CCA = mean(Dead_CCA),
            Dead_Endolithic_Algae = mean(Dead_Endolithic_Algae),
            #Dead_Terpios = mean(Dead_Terpios),
            Dead_Turf = mean(Dead_Turf),
            Health = mean(Health),
            Partly_Bleached = mean(Partly_Bleached))

AN.post.bleaching.sd <- AN.post.bleaching.per %>%
  summarise(Bleached = sd(Bleached),
            Dead_CCA = sd(Dead_CCA),
            Dead_Endolithic_Algae = sd(Dead_Endolithic_Algae),
            Dead_Terpios = sd(Dead_Terpios),
            Dead_Turf = sd(Dead_Turf),
            Health = sd(Health),
            Partly_Bleached = sd(Partly_Bleached))

AN.post.bleaching.per %>%
  summarise(Daed.mean = mean(Dead_CCA + Dead_Endolithic_Algae + Dead_Terpios + Dead_Turf),
            Daed.sd = sd(Dead_CCA + Dead_Endolithic_Algae + Dead_Terpios + Dead_Turf))

### map


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf(color = "black", fill = 'antiquewhite')+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(100,150), ylim = c(10,40), expand = FALSE, 
           crs = "+proj=longlat +ellps=WGS84") +
  #annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = 'aliceblue'))  +
  geom_rect(xmin = 119.8,  ymin = 21.8,  xmax = 120.3,  ymax = 22.3,
            fill = NA,  colour = "black",  size = 1)

TW <- readOGR('TWN_adm0.shp') # import map data
TW.df.sf <- st_as_sf(TW, coords = c("long", "lat"),crs = "+proj=longlat +ellps=WGS84")

ggplot(data = TW.df.sf) +
  geom_sf(color = "black", fill = 'antiquewhite')+
  geom_point(aes(x = 120.35706, y = 22.33635), size = 3, colour = 'red')+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(120.32, 120.42), ylim = c(22.3, 22.37), expand = FALSE, 
         crs = "+proj=longlat +ellps=WGS84") +
  annotation_scale(location = "br", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = 'aliceblue')) 



library(rgdal)
library(raster)

library(marmap)
papoue <- getNOAA.bathy(lon1 = 120, lon2 = 121,
                        lat1 = 22, lat2 = 23, resolution = 0.1)

# Creating color palettes
blues <- c("lightsteelblue4", "lightsteelblue3",
           "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))

plot(papoue, image = TRUE, land = TRUE, lwd = 0.03,
     bpal = list(c(0, max(papoue), greys),
                 c(min(papoue), 0, blues)),
     xlim = c(120.3,120.4), ylim = c(22.3, 22.4))
# Add coastline
plot(papoue, n = 1, lwd = 0.4, add = TRUE)

summary(papoue)

# transect - west
trsect <- get.transect(papoue, 120.33,22.336, 120.36, 22.336, distance = TRUE)
head(trsect)
plotProfile(trsect)

# transect - east
trsect2 <- get.transect(papoue, 120.36,22.336, 120.2, 22.336, distance = TRUE)
head(trsect2)
plotProfile(trsect2)



# Load NW Atlantic xyz data and convert to class bathy

wireframe(unclass(papoue), shade = TRUE, aspect = c(1/2, 0.1))

plot(papoue, xlim = c(120,121),
     deep = c(-5000, 0), shallow = c(0, 0), step = c(1000, 0),
     col = c("lightgrey", "black"), lwd = c(0.8, 1),
     lty = c(1, 1), draw = c(FALSE, FALSE))
belt <- get.box(papoue, x1 = 120.28, x2 = 120.4, y1 = 22.336, y2 = 22.336,
                width = 0.1, col = "red")

wireframe(belt, shade = TRUE, zoom = 1.1,
          aspect = c(1/4, 0.1),
          screen = list(z = -60, x = -55),
          par.settings = list(axis.line = list(col = "transparent")),
          par.box = c(col = rgb(0, 0, 0, 0.1)))




##### SST

SST <- nc_open('dhw_5km_XLQ.nc')

#### extract data ####
sst<-'CRW_SST'
lon<-ncvar_get(SST,'longitude')
nlon<-dim(lon)
lat<- ncvar_get(SST,'latitude')
nlat<-dim(lat)
tm <- ncvar_get(SST,'time')

DHW <- 'CRW_DHW'
BAA <- 'CRW_BAA'
#### Check timeunit and alter date format ####
nt<-dim(tm)
nm<-12
ny<-nt/nm

date <-as.POSIXct(tm,origin='1970-01-01',tz='')
year <- as.numeric(unlist(strsplit(as.character(date),'-'))[seq(1,nt*3,by=3)])
month <- as.numeric(unlist(strsplit(as.character(date),'-'))[seq(2,nt*3,by=3)])

sst_array <- ncvar_get(SST,sst)
dim(sst_array)
DHW_array <- ncvar_get(SST,DHW)
dim(DHW_array)
BAA_array <- ncvar_get(SST,BAA)
dim(BAA_array)

nc_close(SST)

grid <- expand.grid(lon=lon, lat=lat) # create a grid system of this nc file
cutpts <- seq(14,28,by=0.5) # set the color bar
col <- colorRampPalette(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red')) # set the color panel
levelplot(sst_array ~ lon * lat,data=grid, at=cutpts, cuts= 5, 
          col.regions = col,
          xlab = "Longitude", ylab = "Latitude")

plot(sst_array[,,1])


# loop to make the arrangement of the longitude correct
sst_daily <- as.data.frame(sst_array[,,1])
sst_ltm_over <- list() 
for (i in 1:41){
  sst_ltm_over[[i]] <- sst_daily[,i]
}
sst_ltm_over <- data.frame(matrix(unlist(sst_ltm_over),ncol=1)) # transfer the matrix to df so that it can be used for spatialgriddf 
colnames(sst_ltm_over) <- 'sst_ltm_over'


### overlap sampling site and data
poi <- data.frame(lon = 120.35706, lat = 22.33635, # extract the coordinates of sampling locations
                  stringsAsFactors=F) 
coordinates(poi) <- c('lon', 'lat') # assign the lon and lat
proj4string(poi) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # assign the CRS
class(poi)

grid_exp_df <- expand.grid(lon=lon, lat=lat)
grid_exp_df[,'seq'] <- seq_len(nrow(grid_exp_df))
sst_pt <- SpatialPointsDataFrame(coords=grid_exp_df[, c("lon", "lat")], data=grid_exp_df[, "seq", drop=F])
sst_topo <- points2grid(sst_pt, tolerance = 0.000183117)
sst_df <- data.frame(seq(1:nrow(sst_ltm_over)),sst_ltm_over, grid_exp_df$seq)
sst_grid <- SpatialGridDataFrame(sst_topo,data = data.frame(sst_ltm_over, grid_exp_df$seq), 
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
sst_grid_plot <- SpatialGridDataFrame(sst_topo,data = data.frame(sst_ltm_over, grid_exp_df$seq), 
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

over_intp_non <- over(poi,sst_grid) # the overlapping between the sampling locations and grid
plot(sst_grid_plot , ylim = c(21,24), xlim = c(120,123), 
     breaks = seq(14,28,by=1), at = seq(14,28,by=1),
     col = c('purple','dark blue', 'blue', 'dark cyan', 'cyan','dark green', 'green', 'gold','yellow', 'dark orange', 'orange','pink','dark red','red')) # plot the wave grid system
plot(poi, add = T, pch = 16, col = 'red') # plot the sampling locations



Alert <- c()

DHW.df$DHW.poi>4&DHW.df$DHW.poi<8
which(DHW.df$DHW.poi>4&DHW.df$DHW.poi<=8) # Alert 1
DHW.poi[which(DHW.df$DHW.poi>4)]
which(DHW.df$DHW.poi>8) # Alert2
DHW.df$Alert 

## the point is on column = 8; row = 15
sst.poi <- sst_array[8,15,]
DHW.poi <- DHW_array[8,15,]
BAA.poi <- BAA_array[8,15,]
sst.df <- data.frame(date, sst.poi)
DHW.df <- data.frame(date, DHW.poi)
BAA.df <-  data.frame(date, BAA.poi)
date <- as.Date(date, format = "%y-%m-%d")
df <- data.frame(date, sst.poi, DHW.poi, BAA.poi)

## 2022 & 2023 ave SST
mean(sst.poi[1:365])
sd(sst.poi[1:365])
mean(sst.poi[366:614])
sd(sst.poi[366:614])

## 2022 & 2023 summer (Jul + Aug) ave SST
mean(sst.poi[181:242])
sd(sst.poi[181:242])
mean(sst.poi[546:607])
sd(sst.poi[546:607])

## DHW peak in 2022 and 2023
max(DHW.poi[1:365])
max(DHW.poi[366:614])

# plot
ggplot(data = df)+
  xlab('Date')+
  geom_line( aes(x = date, y = sst.poi), color = "black", size = 2)+
  geom_hline(yintercept = 8, color = "red", linetype="dotted", size = 1)+
  geom_hline(yintercept = 16, color = "red", linetype="dotted", size = 1)+
  geom_hline(yintercept = 0, color = "black", size = 1)+ 
  geom_area(data = filter(df, BAA.poi == 0& date > '2022-09-25' & date < '2023-01-01'), aes(x = date, y = DHW.poi*2), fill = "gray", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 0& date > '2023-09-01'), aes(x = date, y = DHW.poi*2), fill = "gray", alpha = 0.8) +
  
  geom_area(data = filter(df, BAA.poi == 1& date < '2022-12-30'& date > '2022-08-01'), aes(x = date, y = DHW.poi*2), fill = "yellow", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 2& date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "orange", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 3& date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "red", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 4& date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "darkred", alpha = 0.8) +
  
  geom_area(data = filter(df, BAA.poi == 1& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "yellow", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 2& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "orange", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 3& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "red", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 4& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "darkred", alpha = 0.8) +
  
  scale_y_continuous(name = "Temperature (°C)",
                     sec.axis = sec_axis(~./2, name="DHW (°C-weeks)"))+
  scale_x_date(breaks = date_breaks("1 month"), date_labels="%b")+
  #coord_cartesian( ylim = c(15, 30))+
  theme_bw()

ggplot(data = df)+
  xlab('Date')+
  geom_line( aes(x = date, y = sst.poi), color = "black", size = 2)+
  geom_hline(yintercept = 14, color = "red", linetype="dotted", size = 1)+
  geom_hline(yintercept = 22, color = "red", linetype="dotted", size = 1)+
  #geom_hline(yintercept = 0, color = "black", size = 1)+ 
  geom_area(data = filter(df, BAA.poi == 0& date > '2022-09-25' & date < '2023-01-01'), aes(x = date, y = DHW.poi*2+6), fill = "gray", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 0& date > '2023-09-01'), aes(x = date, y = DHW.poi*2+6), fill = "gray", alpha = 0.8) +
  
  geom_area(data = filter(df, BAA.poi == 1& date < '2022-12-30'& date > '2022-08-01'), aes(x = date, y = DHW.poi*2+6), fill = "yellow", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 2& date < '2022-12-30'), aes(x = date, y = DHW.poi*2+6), fill = "orange", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 3& date < '2022-12-30'), aes(x = date, y = DHW.poi*2+6), fill = "red", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 4& date < '2022-12-30'), aes(x = date, y = DHW.poi*2+6), fill = "darkred", alpha = 0.8) +
  
  geom_area(data = filter(df, BAA.poi == 1& date > '2022-12-30'), aes(x = date, y = DHW.poi*2+6), fill = "yellow", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 2& date > '2022-12-30'), aes(x = date, y = DHW.poi*2+6), fill = "orange", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 3& date > '2022-12-30'), aes(x = date, y = DHW.poi*2+6), fill = "red", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 4& date > '2022-12-30'), aes(x = date, y = DHW.poi*2+6), fill = "darkred", alpha = 0.8) +

  scale_y_continuous(name = "Temperature (°C)",
                     sec.axis = sec_axis(~(.-6)/2, name="DHW (°C-weeks)"))+
  scale_x_date(breaks = date_breaks("1 month"), date_labels="%b")+
  coord_cartesian( ylim = c(14, 32))+
  theme_bw()

ggplot(data = df)+
  xlab('Date')+

  geom_hline(yintercept = 8, color = "red", linetype="dotted", size = 1)+
  geom_hline(yintercept = 16, color = "red", linetype="dotted", size = 1)+
  #geom_hline(yintercept = 0, color = "black", size = 1)+ 
  geom_area(data = filter(df, BAA.poi == 0& date > '2022-09-25' & date < '2023-01-01'), aes(x = date, y = DHW.poi*2), fill = "gray", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 0& date > '2023-09-01'), aes(x = date, y = DHW.poi*2), fill = "gray", alpha = 0.8) +
  
  geom_area(data = filter(df, BAA.poi == 1& date < '2022-12-30'& date > '2022-08-01'), aes(x = date, y = DHW.poi*2), fill = "yellow", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 2& date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "orange", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 3& date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "red", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 4& date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "darkred", alpha = 0.8) +
  
  geom_area(data = filter(df, BAA.poi == 1& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "yellow", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 2& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "orange", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 3& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "red", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 4& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "darkred", alpha = 0.8) +
  
  scale_y_continuous(name = "Temperature (°C)", breaks=c(0, 5, 10, 15, 20, 25, 30), labels=c(6, 11, 16 ,21, 26, 31, 36), limits=c(0,25),
                     sec.axis = sec_axis(~./2, name="DHW (°C-weeks)"), expand = c(0,0))+
  scale_x_date(breaks = date_breaks("1 month"), date_labels="%b")+
  geom_line( aes(x = date, y = sst.poi-6), color = "black", size = 2)+
  theme_bw()







## SST long-term
#SST <- nc_open('dhw_5km_XLQ_1985-2023.nc')


## SST long-term
#SST <- nc_open('C:\\Users\\Vicky\\Downloads\\dhw_5km_20230901-1031.nc')
SST <- nc_open('dhw_5km_XLQ_20222023.nc')

#### extract data ####
sst<-'CRW_SST'
lon<-ncvar_get(SST,'longitude')
nlon<-dim(lon)
lat<- ncvar_get(SST,'latitude')
nlat<-dim(lat)
tm <- ncvar_get(SST,'time')

DHW <- 'CRW_DHW'
BAA <- 'CRW_BAA'
#### Check timeunit and alter date format ####
date <-as.POSIXct(tm,origin='1970-01-01',tz='')
Date <- as.Date(date, format = "%y-%m-%d") # keep only the date (y-m-d) information

#year <- as.numeric(unlist(strsplit(as.character(date),'-'))[seq(1,nt*3,by=3)])
#month <- as.numeric(unlist(strsplit(as.character(date),'-'))[seq(2,nt*3,by=3)])

sst_array <- ncvar_get(SST,sst)
dim(sst_array)
sst_array_matrix <- as.matrix(sst_array)

DHW_array <- ncvar_get(SST,DHW)
dim(DHW_array)
BAA_array <- ncvar_get(SST,BAA)
dim(BAA_array)

nc_close(SST)


###


sst_mean <- mean(sst_array_matrix)
sst_abn <- sst_array_matrix - sst_mean


sst_plot <- data.frame(Date, sst_abn)

ggplot(sst_plot) + 
  geom_bar(aes(x = Date, y = sst_abn), stat = 'identity')

