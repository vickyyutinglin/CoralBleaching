### library
library(ggplot2)
library(tidyr)
library(vegan)
library(dplyr)
#library(colorspace)    
#library(RColorBrewer)    
library(fmsb)

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
maj.per.2022HC <- as.numeric(maj.per[4,c(1,2,3)])
maj.per.2023HC <- as.numeric(maj.per[4,c(4,5,6)])
shapiro.test(maj.per.2022HC)
shapiro.test(maj.per.2023HC)
var.test(maj.per.2022HC, maj.per.2023HC)
t.test(maj.per.2022HC, maj.per.2023HC, var.equal = TRUE)

# Turf algae cover changed bwtween 2022 & 2023
maj.per.2022TF <- as.numeric(maj.per[8,c(1,2,3)])
maj.per.2023TF <- as.numeric(maj.per[8,c(4,5,6)])
shapiro.test(maj.per.2022TF)
shapiro.test(maj.per.2023TF)
var.test(maj.per.2022TF, maj.per.2023TF)
t.test(maj.per.2022TF, maj.per.2023TF, var.equal = TRUE)

# stacked + percent
maj_long <- gather(maj, transect, cover, X2022_T1, X2022_T2, X2022_T3, X2023_T1, X2023_T2, X2023_T3, factor_key=TRUE)
maj_long
ggplot(maj_long, aes(fill=Major_cate, y=cover, x=transect)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = c("green", "grey", "pink",'blue','black','lightblue','yellow','lightgreen'))


### labels
# nmds
benthic_data_label <- benthic_data[,-c(1,3,10)]
benthic_data_label <- aggregate(benthic_data_label[,-1], list(benthic_data_label$Label), sum)
Label <- benthic_data_label$Group.1
benthic_data_label$Group.1 <- NULL
benthic_data_label <- t(benthic_data_label)
colnames(benthic_data_label) <- Label

nmds.benthic <- metaMDS(benthic_data_label, distance = "bray")   
nmds.benthic$stress
stressplot(nmds.benthic)

# extract the scores of nmds
site.scores <- as.data.frame(scores(nmds.benthic)$sites)
site.scores$Year <- c('2022','2022','2022','2023','2023','2023')
  
species.scores <- as.data.frame(scores(nmds.benthic, "species"))
species.scores$species <- rownames(species.scores)

# plot
ggplot() +
  geom_point(data = site.scores, aes(x = NMDS1, y = NMDS2, fill = Year),size=4,pch=21) +
  geom_text(data = species.scores, mapping = aes(x = NMDS1, y = NMDS2, label = species),alpha = 0.6, size = 4) +
  theme_minimal() +
  theme(legend.position = "right",text = element_text(size = 24)) +
  scale_x_continuous(name="MDS 1") +
  scale_y_continuous(name="MDS 2")

### coral assemblage
HC <- benthic_data %>% filter(Major_cate=='Hard_corals')
SC <- benthic_data %>% filter(Major_cate=='Soft_corals')
Coral <- benthic_data %>% filter(Major_cate=='Soft_corals'|Major_cate=='Hard_corals')
Coral$Major_cate <- NULL
Coral$Bleached_status <- NULL
Coral$Label_raw <- NULL

Coral.per <- aggregate(Coral[,-1], list(Coral$Label), sum)
rownames(Coral.per) <- Coral.per$Group.1
Coral.per[,1]<-NULL
Coral.per <- apply(Coral.per,2,function(x) {100*(x/sum(x))})

# stacked + percent
Coral_long <- gather(Coral, transect, cover, X2022_T1, X2022_T2, X2022_T3, X2023_T1, X2023_T2, X2023_T3, factor_key=TRUE)
Coral_long
ggplot(Coral_long, aes(fill=Label, y=cover, x=transect)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = c("#1F78B4","#A6CEE3","#80B1D3",
                               "#B15928", "#FF7F00","#E31A1C", "#FDBF6F", 
                               "#6A3D9A","#BC80BD","#CAB2D6",
                               "#FFED6F", "#FB9A99","#33A02C","#5d0e66",
                               "#8DD3C7","#d1a66d"))


### bleaching conditions
# radar chart indicates the change in coral bleaching condition 
bleaching <- aggregate(benthic_data[,-c(1,2,3,10)], list(benthic_data$Bleached_status), sum)
bleaching <- t(bleaching)
bleaching <- bleaching[,-1]
colnames(bleaching) <- bleaching[1,]

pre.bleaching <- matrix(as.numeric(bleaching[-1,]), ncol = 7, nrow = 6)[c(1:3),]
colnames(pre.bleaching) <- colnames(bleaching)
pre.bleaching.per <- 100*(colSums(pre.bleaching)/sum(pre.bleaching))

post.bleaching <- matrix(as.numeric(bleaching[-1,]), ncol = 7, nrow = 6)[c(4:6),]
colnames(post.bleaching) <- colnames(bleaching)
post.bleaching.per <- 100*(colSums(post.bleaching)/sum(post.bleaching))

bleaching.per.radar <- as.data.frame(rbind(rep(60,7), rep(0,7), pre.bleaching.per, post.bleaching.per))

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( bleaching.per.radar  , axistype=1 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,60,15), cglwd=0.8,
            #custom labels
            vlcex=1.5
)

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
            vlcex=1.5
)





