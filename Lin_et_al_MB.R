### Library
library(ggplot2)
library(tidyr)
library(vegan)
library(dplyr)
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
library(marmap)

### Directory 
setwd('C:/Users/Vicky/Dropbox/FRE - Vicky@DP/Publication/On the way to publish/12. XLQ mesophotic bleacing event/Data')

### Map & depth profile of the sampling site - Vanice
# Figure 1a - world
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf(color = "black", fill = 'antiquewhite')+
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(100,150), ylim = c(10,40), expand = FALSE,  
           crs = "+proj=longlat +ellps=WGS84") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = 'aliceblue'))  +
  geom_rect(xmin = 119.8,  ymin = 21.8,  xmax = 120.3,  ymax = 22.3,
            fill = NA,  colour = "black",  size = 1)

# Figure 1b - Vanice
TW <- readOGR('TWN_adm0.shp') # import map data
TW.df.sf <- st_as_sf(TW, coords = c("long", "lat"), 
                     crs = "+proj=longlat +ellps=WGS84")

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
  geom_segment(aes(x=120.32000, xend=120.37000, y=22.33635, yend=22.33635), 
               colour="red", size = 1, linetype = 'dashed')+
  theme(panel.background = element_rect(fill = 'aliceblue')) 

# Figure 1c - depth profile
Van <- getNOAA.bathy(lon1 = 120, lon2 = 121, lat1 = 22, lat2 = 23, 
                     resolution = 0.1) # download the bathymetric data
trsect <- get.transect(Van, 120.31000, 22.33635, 120.36000, 22.33635, distance = TRUE) # add the transect to extract depth profile
head(trsect) # check the information of transects
plotProfile(trsect) # plot the depth profile


### Thermal environment
# Extract thermal data
SST <- nc_open('dhw_5km_XLQ_202223.nc') # open the nc file
sst <-'CRW_SST' # get SST variables
DHW <- 'CRW_DHW' # get DHW variables
BAA <- 'CRW_BAA' # get BAA variables

lon<-ncvar_get(SST,'longitude') # get longitudes
nlon<-dim(lon)
lat<- ncvar_get(SST,'latitude') # get latitudes
nlat<-dim(lat)
tm <- ncvar_get(SST,'time') # get time
date <- as.POSIXct(tm,origin='1970-01-01',tz='') # transform the date into y-m-d 00:00:00 CST format
date <- as.Date(date, format = "%y-%m-%d") # keep only the date (y-m-d) information

sst_array <- ncvar_get(SST,sst) # change SST data into array
dim(sst_array)
DHW_array <- ncvar_get(SST,DHW) # change DHW data into array
dim(DHW_array)
BAA_array <- ncvar_get(SST,BAA) # change BAA data into array
dim(BAA_array)
nc_close(SST) # close the nc file

# A loop to make the arrangement of the longitude correct
sst_daily <- as.data.frame(sst_array[,,1])  
sst_ltm_over <- list() 
for (i in 1:41){ 
  sst_ltm_over[[i]] <- sst_daily[,i]
}
sst_ltm_over <- data.frame(matrix(unlist(sst_ltm_over),ncol=1)) # transfer the matrix to df so that it can be used for spatialgriddf 
colnames(sst_ltm_over) <- 'sst_ltm_over' # add column name

# Look for the grid that contain the sampling site 
poi <- data.frame(lon = 120.35706, lat = 22.33635, # extract the coordinates of sampling site
                  stringsAsFactors=F) 
coordinates(poi) <- c('lon', 'lat') # assign the lon and lat
proj4string(poi) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # assign the CRS
grid_exp_df <- expand.grid(lon=lon, lat=lat) # expand the grid
grid_exp_df[,'seq'] <- seq_len(nrow(grid_exp_df)) # add the sequence to the grid
sst_pt <- SpatialPointsDataFrame(coords=grid_exp_df[, c("lon", "lat")], data=grid_exp_df[, "seq", drop=F]) # create SpatialPointsDataFrame with grid's sequence
sst_topo <- points2grid(sst_pt, tolerance = 0.000183117) # create the topography for SpatialGridDataFrame
sst_grid <- SpatialGridDataFrame(sst_topo,data = data.frame(sst_ltm_over, grid_exp_df$seq),  # create SpatialGridDataFrame that has SST data and the sequence of grids
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
over_intp_non <- over(poi,sst_grid) # overlap the sampling site point and SST grid to see where the sampling site locates

plot(sst_grid , ylim = c(21,24), xlim = c(120,123),  # plot the SST data
     breaks = seq(14,28,by=1), at = seq(14,28,by=1),
     col = c('purple','dark blue', 'blue', 'dark cyan', 'cyan','dark green', 'green', 'gold','yellow', 'dark orange', 'orange','pink','dark red','red')) # plot the wave grid system
plot(poi, add = T, pch = 16, col = 'red') # plot the sampling locations


# sampling site is on the 582th grid (column = 8; row = 15)
sst.poi <- sst_array[8,15,] # extract SST data of the sampling site
DHW.poi <- DHW_array[8,15,] # extract DHW data of the sampling site
BAA.poi <- BAA_array[8,15,] # extract BAA data of the sampling site

sst.df <- data.frame(date, sst.poi) # create a dataframe for SST of the sampling site
DHW.df <- data.frame(date, DHW.poi) # create a dataframe for DHW of the sampling site
BAA.df <- data.frame(date, BAA.poi) # create a dataframe for BAA of the sampling site
df <- data.frame(date, sst.poi, DHW.poi, BAA.poi) # combine 3 dataframes

## 2022 & 2023 mean SST
mean(sst.poi[1:365])
sd(sst.poi[1:365])
mean(sst.poi[366:614])
sd(sst.poi[366:614])

## 2022 & 2023 summer (Jul + Aug) mean SST
mean(sst.poi[181:242])
sd(sst.poi[181:242])
mean(sst.poi[546:607])
sd(sst.poi[546:607])

## DHW peak in 2022 and 2023
max(DHW.poi[1:365])
max(DHW.poi[366:614])

# Figure 2
ggplot(data = df)+
  xlab('Date')+
  geom_line(aes(x = date, y = sst.poi-6), color = "black", size = 1) +
  geom_hline(yintercept = 8, color = "red", linetype="dotted", size = 1)+
  geom_hline(yintercept = 16, color = "red", linetype="dotted", size = 1)+
  geom_area(data = filter(df, BAA.poi == 0& date > '2022-09-25' & date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "gray", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 0& date > '2023-10-05'& date < '2023-10-30'), aes(x = date, y = DHW.poi*2), fill = "gray", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 1& date < '2022-12-30'& date > '2022-08-01'), aes(x = date, y = DHW.poi*2), fill = "yellow", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 2& date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "orange", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 3& date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "red", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 4& date < '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "darkred", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 1& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "yellow", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 2& date > '2023-07-07' & date < '2023-07-24'), aes(x = date, y = DHW.poi*2), fill = "orange", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 2& date > '2023-08-25' & date < '2023-09-01'), aes(x = date, y = DHW.poi*2), fill = "orange", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 3& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "red", alpha = 0.8) +
  geom_area(data = filter(df, BAA.poi == 4& date > '2022-12-30'), aes(x = date, y = DHW.poi*2), fill = "darkred", alpha = 0.8) +
  scale_y_continuous(name = "Temperature (°C)", breaks=c(0, 5, 10, 15, 20, 25, 30), labels=c(6, 11, 16 ,21, 26, 31, 36), limits=c(0,25),
                     sec.axis = sec_axis(~./2, name="DHW (°C-weeks)"), expand = c(0,0))+
  scale_x_date(breaks = date_breaks("1 month"), limits = as.Date(c('2022-01-01','2023-10-30')), date_labels="%b")+
  theme_bw()

### Change in benthic composition & recovery of corals
# Import data
benthic_data <- read.csv('benthic_data.csv', sep = ',', header = T)

# Major benthic categories absolute covers
maj <- t(aggregate(benthic_data[,-c(1,2,3,10)], list(benthic_data$Major_cate), sum))
maj.lab <- maj[1,]
maj.cov <- as.data.frame(matrix( as.numeric(maj[2:7,]), nrow = 6, ncol = 8))
maj.year <- c('2022','2022','2022','2023','2023','2023')
colnames(maj.cov) <- maj.lab
maj.cov$Year <- maj.year
maj.yr <- maj.cov %>% 
  group_by(Year) %>%
  summarise(Algae = sum(Algae),
            Bare.substrate = sum(Bare_substrate),
            CCA = sum(CCA),
            Hard.corals = sum(Hard_corals),
            Other.life = sum(Other_live),
            Soft.corals = sum(Soft_corals),
            Sponges = sum(Sponges),
            Turf = sum(Turf_cyanobacteria))
maj.yr.2022 <- 100 * (as.numeric(maj.yr[1,2:9])/sum(as.numeric(maj.yr[1,2:9])))
maj.yr.2023 <- 100 * (as.numeric(maj.yr[2,2:9])/sum(as.numeric(maj.yr[2,2:9])))
maj.yr <- round(rbind(maj.yr.2022,maj.yr.2023),1) # cover matrix
colnames(maj.yr) <- maj.lab
row.names(maj.yr) <- c('2022','2023')

# Major benthic categories abdolute covers with corals' bleaching status
maj.ble <- benthic_data[,-c(2,10)]
maj.ble$maj.ble <- with(maj.ble, paste(Major_cate, Bleached_status, sep = "_"))
maj.ble$Major_cate <- NULL
maj.ble$Bleached_status <- NULL
maj.ble <- aggregate(maj.ble[,-7], list(as.character(maj.ble$maj.ble)), sum)
colnames(maj.ble) <- c('Major_Category_Bleached','X2022_T1', 'X2022_T2', 'X2022_T3', 'X2023_T1', 'X2023_T2', 'X2023_T3')

maj.ble.2022 <- rowSums(maj.ble[,2:4])
maj.ble.2023 <- rowSums(maj.ble[,5:7])
Major_Category_Bleached <- maj.ble$Major_Category_Bleached
maj.ble.yr <- data.frame(Major_Category_Bleached, maj.ble.2022, maj.ble.2023)
colnames(maj.ble.yr) <- c('Major_Category_Bleached','X2022','X2023')

maj.ble.2022 <- 100*(maj.ble.yr[,2]/colSums(maj.ble.yr[,2:3]))
maj.ble.2023 <- 100*(maj.ble.yr[,3]/colSums(maj.ble.yr[,2:3]))
maj.ble.2223 <- round(t(data.frame(maj.ble.2022, maj.ble.2023)),1)
colnames(maj.ble.2223) <- maj.ble.yr$Major_Category_Bleached
rownames(maj.ble.2223) <- c('2022','2023')

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

maj.ble.col <- c('#66c2a5','#b3b3b3',"#E78AC399",'#e78ac3',"#8DA0CB66","#8DA0CBB3",'#8da0cb',
                 '#fc8d62',"#E5C49499","#E5C494CC",'#e5c494',"#FFD92FB3",'#ffd92f',"#A6D85466","#A6D854B3",'#a6d854')

maj.ble.yr.long <- gather(maj.ble.yr, Year, Absolute_cover, X2022, X2023, factor_key=TRUE)
Major_Category <- factor(maj.ble.yr.long$Major_Category_Bleached, levels = maj.ble.seq)
Year. <- factor(maj.ble.yr.long$Year , levels = c('X2023','X2022'))

# Figure 4a
ggplot(maj.ble.yr.long, aes(fill = Major_Category, y = Absolute_cover, x = Year.)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = maj.ble.col) + 
  coord_flip() + 
  theme_classic()

# Comparing benthic composition between 2022 & 2023 (transect level)
maj.per.yr <- (maj.cov[,-9]/rowSums(maj.cov[,-9]))*100
maj.per.yr$Year <- c(rep('2022',3),rep('2023',3))

maj.per.yr.mean1 <- maj.per.yr %>%
  group_by(Year) %>%
  summarise(Algae = mean(Algae),
            Bare.substrate = mean(Bare_substrate),
            CCA = mean(CCA),
            Hard.corals = mean(Hard_corals),
            Other.life = mean(Other_live),
            Soft.corals = mean(Soft_corals),
            Sponges = mean(Sponges),
            Turf = mean(Turf_cyanobacteria))
maj.per.yr.mean <- gather(maj.per.yr.mean1, maj, value = 'cover', 2:9)

maj.per.yr.sd <- maj.per.yr %>%
  group_by(Year) %>%
  summarise(Algae = sd(Algae),
            Bare.substrate = sd(Bare_substrate),
            CCA = sd(CCA),
            Hard.corals = sd(Hard_corals),
            Other.life = sd(Other_live),
            Soft.corals = sd(Soft_corals),
            Sponges = sd(Sponges),
            Turf = sd(Turf_cyanobacteria))
maj.per.yr.sd <- gather(maj.per.yr.sd, maj, value = 'cover', 2:9)
maj.per.yr.bar <- cbind(maj.per.yr.mean[,1:2], round(maj.per.yr.mean$cover,1), round(maj.per.yr.sd$cover,1)) # benthic mean cover & SD
colnames(maj.per.yr.bar) <- c('Year', 'Major', 'Cover', 'SD')

# SuUpplementary Information 1
ggplot(maj.per.yr.bar, aes(x = Year, y = Cover, fill = Major)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Cover-SD, ymax = Cover+SD), position =  "dodge", width = 0.2) +
  facet_wrap(~Major, scales = "free")+
  scale_fill_manual(values = maj.col)

### Coral assemblage
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
col.per.yr.mean <- gather(Coral.yr.cover.mean1, coral, value = 'cover', 2:17)

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

coral.per.yr.sd <- gather(Coral.yr.cover.sd1, coral, value = 'cover', 2:17)
coral.per.yr.bar <- cbind(col.per.yr.mean[,1:2], round(col.per.yr.mean$cover,1), round(maj.per.yr.sd$cover,1)) # benthic mean cover & SD
colnames(coral.per.yr.bar) <- c('Year', 'Coral', 'Cover', 'SD')

# Figure 4b
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
Tran.seq <- factor(Coral_long$Transect , levels = c('X2023_T3', 'X2023_T2','X2023_T1','X2022_T3','X2022_T2','X2022_T1'))
coral.col <- c('#3288bd',"#3288BDB3","#3288BD66","#3288BD1A",'#99d594',
               '#fee08b',"#FEE08BCC","#FEE08B99","#FEE08B66","#FEE08B33", 
               '#fc8d59',"#FC8D59CC","#FC8D5999","#FC8D5966",'#d53e4f','#9e0142')

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


### Coral bleaching status
bleaching <- t(aggregate(benthic_data[,-c(1,2,3,10)], list(benthic_data$Bleached_status), sum))
bleaching <- bleaching[,-1]; colnames(bleaching) <- bleaching[1,]

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
pre.bleaching.mean1 <- gather(pre.bleaching.mean, coral, value = 'mean', 1:7)

pre.bleaching.sd <- pre.bleaching.per %>%
  summarise(Bleached = sd(Bleached),
            Dead_CCA = sd(Dead_CCA),
            Dead_Endolithic_Algae = sd(Dead_Endolithic_Algae),
            Dead_Terpios = sd(Dead_Terpios),
            Dead_Turf = sd(Dead_Turf),
            Health = sd(Health),
            Partly_Bleached = sd(Partly_Bleached))
pre.bleaching.sd1 <- gather(pre.bleaching.sd, coral, value = 'sd', 1:7)
post.bleaching.2022 <- data.frame(pre.bleaching.mean1,pre.bleaching.sd1$sd)
colnames(post.bleaching.2022) <- c('coral', 'mean', 'sd')
post.bleaching.2022$year <- rep('2022',7)

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
post.bleaching.mean1 <- gather(post.bleaching.mean, coral, value = 'mean', 1:7)

post.bleaching.sd <- post.bleaching.per %>%
  summarise(Bleached = sd(Bleached),
            Dead_CCA = sd(Dead_CCA),
            Dead_Endolithic_Algae = sd(Dead_Endolithic_Algae),
            Dead_Terpios = sd(Dead_Terpios),
            Dead_Turf = sd(Dead_Turf),
            Health = sd(Health),
            Partly_Bleached = sd(Partly_Bleached))
post.bleaching.sd1 <- gather(post.bleaching.sd, coral, value = 'sd', 1:7)

post.bleaching.2023 <- data.frame(post.bleaching.mean1,post.bleaching.sd1$sd)
colnames(post.bleaching.2023) <- c('coral', 'mean', 'sd')
post.bleaching.2023$year <- rep('2023',7)
bleaching.coral <- rbind(post.bleaching.2022,post.bleaching.2023)

# Figure 5a - Radar plot
var <- colnames(pre.bleaching.per)
pre.mean <- t(pre.bleaching.mean)
pre.upper <- t(pre.bleaching.mean) + t(pre.bleaching.sd) 
pre.lower <- t(pre.bleaching.mean) - t(pre.bleaching.sd)
pre.bleaching.rad <- data.frame(var, pre.mean, pre.upper, pre.lower)
post.mean <- t(post.bleaching.mean)
post.upper <- t(post.bleaching.mean) + t(post.bleaching.sd) 
post.lower <- t(post.bleaching.mean) - t(post.bleaching.sd)
bleaching.rad <- data.frame(var, pre.mean, pre.upper, pre.lower,
                            post.mean, post.upper, post.lower)

#bleaching.mean.sd <- data.frame( t(pre.bleaching.mean), t(pre.bleaching.sd),
#                                 t(post.bleaching.mean), t(post.bleaching.sd))

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

# Bleaching status of A. tenella
AT.pre.bleaching <- matrix(as.numeric(AT.coral), ncol = 7, nrow = 6)[c(1:3),]
colnames(AT.pre.bleaching) <- colnames(AT.coral)
AT.pre.bleaching.per <- as.data.frame(100*(AT.pre.bleaching/rowSums(AT.pre.bleaching)))
AT.pre.bleaching.per.mean <- colMeans(AT.pre.bleaching.per)
AT.pre.bleaching.per.sd <- sapply(AT.pre.bleaching.per, sd)

AT.post.bleaching <- matrix(as.numeric(AT.coral), ncol = 7, nrow = 6)[c(4:6),]
colnames(AT.post.bleaching) <- colnames(AT.coral)
AT.post.bleaching.per <- as.data.frame(100*(AT.post.bleaching/rowSums(AT.post.bleaching)))
AT.post.bleaching.per.mean <- colMeans(AT.post.bleaching.per)
AT.post.bleaching.per.sd <- sapply(AT.post.bleaching.per, sd)

AT.pre.bleaching.cb <- cbind(round(AT.pre.bleaching.per.mean,1), round(AT.pre.bleaching.per.sd,1))
AT.post.bleaching.cb <- cbind(round(AT.post.bleaching.per.mean,1), round(AT.post.bleaching.per.sd,1))
AT.bleaching <- data.frame(c(rep('2022',7),rep('2023',7)), rbind(AT.pre.bleaching.cb,AT.post.bleaching.cb))
colnames(AT.bleaching) <- c('year','mean','sd')

# Figure 5b - radar plot
AT.pre.upper <- as.numeric(AT.pre.bleaching.per.mean + AT.pre.bleaching.per.sd)
AT.pre.lower <- as.numeric(AT.pre.bleaching.per.mean - AT.pre.bleaching.per.sd)
AT.post.upper <- as.numeric(AT.post.bleaching.per.mean + AT.post.bleaching.per.sd)
AT.post.lower <- as.numeric(AT.post.bleaching.per.mean - AT.post.bleaching.per.sd)
AT.bleaching.rad <- data.frame(AT.pre.bleaching.per.mean,AT.pre.upper,AT.pre.lower,
                               AT.post.bleaching.per.mean, AT.post.upper, AT.post.lower)
AT.bleaching.rad <- AT.bleaching.rad[order(row.names(AT.bleaching.rad)), ]
AT.bleaching.rad <- data.frame(var, AT.bleaching.rad )

ggplot(AT.bleaching.rad, aes(x = var, y = AT.pre.bleaching.per.mean, group = 1)) +
  geom_polygon(fill = NA, colour = '#fbb4ae') +
  geom_polygon(aes(y = AT.pre.upper), fill = adjustcolor('#fbb4ae', alpha.f = 0.5)) +
  geom_polygon(aes(y = AT.pre.lower), fill = 'white') +
  geom_polygon(aes(x = var, y = AT.post.bleaching.per.mean, group = 1), fill = NA, colour = '#8dd3c7') +
  geom_polygon(aes(y = AT.post.upper), fill = adjustcolor('#8dd3c7', alpha.f = 0.5)) +
  geom_polygon(aes(y = AT.post.lower), fill = 'white') +
  theme_light() +
  theme() + 
  coord_polar() +
  labs(x = "", y = "")

