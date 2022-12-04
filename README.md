# community-analysis-full Script 

##### Libraries #####
library(tidyverse)
library(stringr)
library(dave)
library(vegan)
library(dendextend)
library(DarkDiv)
library(rnaturalearth)
library(tmap)
library(ggmap)
library(maps)
library(crs)
library(sp)
library(sf)
library(rgdal)
library(tree)
setwd("~/University Courses/Data analysis in community Ecology/project")
#######################################################
#step 1 : illustrate the data distribution on the map #
######################################################

#CAUTHION: FOR RUNNING THE CODE FOR GENRATING THE MAP YOU NEED THE LATEST VERSION OF "R". ##

#first step was to obtain a figure to show how data was collected was spread on the countries: 

## there was a problem in the cordinates so i rename the columns to get the right cordinate in the rename section
map <-read.csv("environmentaldata.csv")  %>%
  rename("lat"=Longitude,"lon"=Latitude) %>%
  select(Site.no., Country,Survey.year,lat,lon,Altitude) %>%
  drop_na()
#check coordinate reference system (crs)
st_crs(map)
#reproject to WGS84(EPSG:4326)
meta_sf <- st_as_sf(map, coords = c("lon", "lat"), crs = 4326)
#download worldmap
countries <- ne_download(scale = 50, type = 'countries', category = 'cultural', returnclass = "sf")
#select europe and country name(SOVEREIGNT) from data
europe <- countries %>%
  filter(CONTINENT=="Europe") %>%
  select(SOVEREIGNT)
#plot the map
ggplot()+
  geom_sf(data = europe,fill = "white", colour = "grey", size = 0.2)+
  geom_sf(data = meta_sf, aes(colour = Country))
europe_cropped <- st_crop(europe, xmin = -20, xmax = 45,
                          ymin = 30, ymax = 73)
ggplot() + geom_sf(data = europe_cropped) + theme_bw()
ggplot()+
  geom_sf(data = europe_cropped,fill = "white", colour = "grey", size = 0.2)+
  geom_sf(data = meta_sf, aes(colour = Country))+theme_bw()

####################################################
#step 2: reading the species and environmental data#
####################################################

#reading species data
species = read.csv("speciesdata.csv", header = T, sep = ",")
#checking the data for errors
str(species) #checking the variable types
species$Site.number #looking for errors visually
species$Country
species$Year
#checking the quadrat numbers (equal amount and sum of 765 = no errors)
for (i in (1:5)) {
  print(nrow(species[species$Quadrat == i, ]))
}
#checking the numbers of species in different sites, whether there are any anomalies
max(apply(species, 2, FUN = max, na.rm = T)[5:401])
min(apply(species, 2, FUN = max, na.rm = T)[5:401])
#Describing the data
nrow(species)/5
#153 sites, each with 5 quadrats
colnames(species)
species = species[1:395] #removing unknown species' data
#thymus praecox twice, no 361 and 370
isTRUE(species[, 361] == species[, 370]) #different data, not just doubled
species[, 361]
species[, 370]
#joining these columns
species[is.na(species)] = 0 #removing NAs
species[, 361] = species[, 361] + species[, 370]
species[, 361] #OK
species = subset(species, select = -Thymus.praecox.1) #removing now unnecessary column
ncol(species) - 4 #minus metadata columns
#390 species altogether

#Dividing species to vascular plants and bryophytes
colnames(species)
#moss species are: 36, 37, 47, 48, 49, 51, 56, 57, 58, 59, 62, 63,
#65, 66, 67, 68, 69, 102, 109, 116, 126, 127, 128, 129, 130, 147,
#158, 159, 172, 182, 187, 188, 189, 211, 217, 221, 237, 250, 255,
#256, 257, 258, 263, 269, 276, 277, 278, 279, 280, 281, 291, 293,
#298, 299, 300, 309, 310, 311, 340, 341, 342, 343, 344, 345, 346,
#and 360.
bryo = species[, c(1, 2, 3, 4, 36, 37, 47, 48, 49, 51, 56, 57, 58, 59, 62, 63,
                   65, 66, 67, 68, 69, 102, 109, 116, 126, 127, 128, 129, 130, 147,
                   158, 159, 172, 182, 187, 188, 189, 211, 217, 221, 237, 250, 255,
                   256, 257, 258, 263, 269, 276, 277, 278, 279, 280, 281, 291, 293,
                   298, 299, 300, 309, 310, 311, 340, 341, 342, 343, 344, 345, 346,
                   360)]
colnames(bryo) #OK
ncol(bryo) - 4
#66 species of mosses
vasc.plants = species [, -c(36, 37, 47, 48, 49, 51, 56, 57, 58, 59, 62, 63,
                            65, 66, 67, 68, 69, 102, 109, 116, 126, 127, 128, 129, 130, 147,
                            158, 159, 172, 182, 187, 188, 189, 211, 217, 221, 237, 250, 255,
                            256, 257, 258, 263, 269, 276, 277, 278, 279, 280, 281, 291, 293,
                            298, 299, 300, 309, 310, 311, 340, 341, 342, 343, 344, 345, 346,
                            360)]
colnames(vasc.plants) #OK
ncol(vasc.plants) - 4
#324 species of vascular plants

bryo <- as.data.frame(bryo) %>% drop_na()
vasc.plants <- data.frame(vasc.plants) %>% drop_na()

#### reading data and data correction #####
env <- read.csv("environmentaldata.csv", header = T)
gsub(" ", ".", colnames(env))

## subset of env data that we desire to do the analysis 
df <- env %>% select(Site.no.,
               Topsoil.pH,Topsoil.Al, Topsoil.NO3, 
               Topsoil.NH4, Topsoil.Olsen.P) %>% drop_na() 

####data correction environmental data####

df$Site.no. <- gsub("qFR701", "FR701", df$Site.no.)
row.names(df) <- df$Site.no.
df <- df %>% select(-Site.no.)
df <- df[order(row.names(df)), ]

##data correction bryo data: 
bryo <- bryo %>% select(-Quadrat, -Country, -Year)# drop unnecessary columns
bryo <- aggregate(.~Site.number, data = bryo, FUN = sum) # sum up the rows with the same name to have unique names

row.names(bryo) <- bryo$Site.number
bryo <- bryo %>% select(-Site.number)
 
##data correction Vascular plants data:
vasc.plants <- vasc.plants %>% select(-Quadrat, -Country, -Year) # drop unnecessary columns 
vasc.plants <- aggregate(.~Site.number, data = vasc.plants, FUN = sum)# sum up the rows with the same name to have unique names
row.names(vasc.plants) <- vasc.plants$Site.number 
vasc.plants <- vasc.plants %>% select(-Site.number)


#############################
#Step 3: Diversity analysis #
#############################

#### Shannon diversity index #####

#richness
richness=rowSums(vasc.plants>0)
richness1=rowSums(bryo>0)

#Shannon diversity
diversity.shannon = diversity(vasc.plants,"shannon")
diversity.shannon1 = diversity(bryo,"shannon")

#richness effect
eff.richness = exp(diversity.shannon)
eff.richness1 = exp(diversity.shannon1)


##############################################################################################################
#step 4: Shapiro test and correlation test on the environmental data for richness and dark diversity analysis# 
##############################################################################################################

#### Shapiro test for normality #####  
shapiro.test(richness) 
shapiro.test(df$Topsoil.pH) #not normal p-value = 0.01443
shapiro.test(df$Topsoil.NH4) # not normal p-value = 0.00000000002523
shapiro.test(df$Topsoil.Al) # not normal p-value = 0.0000000000000008622
shapiro.test(df$Topsoil.NO3) # not normal p-value = 0.00000000000000022
shapiro.test(df$Topsoil.Olsen.P) # not normal p-value = 0.0000000000005304 

#### correlation #####

##vasplants

cor.test(df$Topsoil.pH,richness, method = "spearman", exact = FALSE) 
cor.test(df$Topsoil.Al,richness, method = "spearman", exact = FALSE)
cor.test(df$Topsoil.NO3,richness, method = "spearman", exact = FALSE) #don't have a correlation
cor.test(df$Topsoil.NH4,richness, method = "spearman", exact = FALSE)
cor.test(df$Topsoil.Olsen.P,richness, method = "spearman", exact = FALSE) #don't have a correlation 

# bryo

cor.test(df$Topsoil.pH,richness1, method = "spearman", exact = FALSE)
cor.test(df$Topsoil.Al,richness1, method = "spearman", exact = FALSE)
cor.test(df$Topsoil.NO3,richness1, method = "spearman", exact = FALSE) # have correlation between them 
cor.test(df$Topsoil.NH4,richness1, method = "spearman", exact = FALSE) 
cor.test(df$Topsoil.Olsen.P,richness1, method = "spearman", exact = FALSE)

##### Dark Diversity ######

# Dark diversity of bryophites plants
dark.bryo <- rowSums(DarkDiv(bryo)$Dark>0.7,na.rm=T)
# Dark diversity of vascular plants
dark.vasc <- rowSums(DarkDiv(vasc.plants)$Dark>0.7,na.rm=T)
# Dark diversity related to environment
hist(dark.bryo)
shapiro.test(dark.bryo) # not-normal, p-value = 0.0004406
hist(dark.vasc)
shapiro.test(dark.vasc) # not-normal, p-value = 2.226e-05
# correlation between vascular plants
cor.test(df$Topsoil.pH,dark.vasc, method = "spearman", exact = FALSE)# significant correlation
plot(df$Topsoil.pH,dark.vasc)
cor.test(df$Topsoil.Al,dark.vasc, method = "spearman", exact = FALSE)# significant correlation
plot(df$Topsoil.Al,dark.vasc)
cor.test(df$Topsoil.NO3,dark.vasc, method = "spearman", exact = FALSE)
cor.test(df$Topsoil.NH4,dark.vasc, method = "spearman", exact = FALSE)# significant correlation
plot(df$Topsoil.NH4,dark.vasc)
cor.test(df$Topsoil.Olsen.P,dark.vasc, method = "spearman", exact = FALSE)
# bryo
cor.test(df$Topsoil.pH,dark.bryo, method = "spearman", exact = FALSE)
cor.test(df$Topsoil.Al,dark.bryo, method = "spearman", exact = FALSE)
cor.test(df$Topsoil.NO3,dark.bryo, method = "spearman", exact = FALSE)
cor.test(df$Topsoil.NH4,dark.bryo, method = "spearman", exact = FALSE)
cor.test(df$Topsoil.Olsen.P,dark.bryo, method = "spearman", exact = FALSE)

#############################################################
# step 5: Models of richness and dark vs. environmental data#
#############################################################

# Vascular plants
mod.rich.vasc <- lm(richness~df$Topsoil.pH+df$Topsoil.Al+df$Topsoil.NO3+df$Topsoil.NH4+df$Topsoil.Olsen.P)
summary(mod.rich.vasc)
hist(mod.rich.vasc$residuals)
shapiro.test(mod.rich.vasc$residuals)
par(mfrow =c(2,2))
plot(mod.rich.vasc)
# Bryophytes
mod.rich.bryo <- lm(richness1~df$Topsoil.pH+df$Topsoil.Al+df$Topsoil.NO3+df$Topsoil.NH4+df$Topsoil.Olsen.P)
summary(mod.rich.bryo)
hist(mod.rich.bryo$residuals)
shapiro.test(mod.rich.bryo$residuals)
plot(mod.rich.bryo)
###### Models of dark diversity vs. environmental data #####

# Vascular plants
mod.dark.vasc <- lm(dark.vasc~df$Topsoil.pH+df$Topsoil.Al+df$Topsoil.NO3+df$Topsoil.NH4+df$Topsoil.Olsen.P)
summary(mod.dark.vasc)# pH is the significant explanatory variable
hist(mod.dark.vasc$residuals)
shapiro.test(mod.dark.vasc$residuals)
par(mfrow =c(2,2))
plot(mod.dark.vasc)# the model is so good

# Bryophytes
mod.dark.bryo <- lm(dark.bryo~df$Topsoil.pH+df$Topsoil.Al+df$Topsoil.NO3+df$Topsoil.NH4+df$Topsoil.Olsen.P)
summary(mod.dark.bryo)# there are not effects of environmental variables
hist(mod.dark.bryo$residuals)
shapiro.test(mod.dark.bryo$residuals)
plot(mod.dark.bryo)# the model is so good

## Constrained ordination to see community composition and relation to environmental variables
Topsoil.pH<- df$Topsoil.pH
Topsoil.Al <- df$Topsoil.Al
Topsoil.NO3 <- df$Topsoil.NO3
Topsoil.NH4 <- df$Topsoil.NH4
Topsoil.Olsen.P <- df$Topsoil.Olsen.P
dev.off()

# Vascular plants
vasc.rda <- rda(vasc.plants~Topsoil.pH+Topsoil.Al+Topsoil.NO3+Topsoil.NH4+Topsoil.Olsen.P,scale=T)
plot(vasc.rda)
plot(vasc.rda,type="n")
points(vasc.rda,"sites",col="red",pch=16)
points(vasc.rda,"cn")
text(vasc.rda,"cn")
anova(vasc.rda)# Highly significant model
anova(vasc.rda,by="mar") # pH and NO3 influence the ocurrence and abundance of species
anova(vasc.rda,by="axis") # RDA1 and RDA2 are significant

# Bryophytes
bryo.rda <- rda(bryo~Topsoil.pH+Topsoil.Al+Topsoil.NO3+Topsoil.NH4+Topsoil.Olsen.P,scale=T)
plot(bryo.rda)
plot(bryo.rda,type="n")
points(bryo.rda,"sites",col="red",pch=16)
points(bryo.rda,"cn")
text(bryo.rda,"cn")
anova(bryo.rda)# Significant model
anova(bryo.rda,by="mar") # NO3 and NH4 influence the ocurrence and abundance of species
anova(bryo.rda,by="axis") # RDA1 is significant

####################################################
# Step 6 : Community Distance matrix and clustering#
###################################################

#calculating distance matrices 
vd <- vegdist(log1p(vasc.plants), "euclidean") # vascular plants
vd1 <- vegdist(log1p(bryo), "euclidean") # bryophycets
pairs(cbind(vd,vd1))

#hierarchical clustering, because we do not need to have a specific amount of clusters

#vascular
o.clu.s <- hclust(vd, method = "single")
o.clu.c <- hclust(vd, method = "complete")
o.clu.a <- hclust(vd, method = "average")
o.clu.v <- hclust(vd, method = "ward.D2")

par(mfrow = c(1, 1))

#choosing the best clustering method by most logical site country code setting
dend = as.dendrogram((o.clu.s), main = "single")
dend = set(dend, "labels_cex", 0.4)
plot(dend)
dend = as.dendrogram((o.clu.c), main = "complete")
dend = set(dend, "labels_cex", 0.4)
plot(dend)
dend = as.dendrogram((o.clu.a), main = "average")
dend = set(dend, "labels_cex", 0.4)
plot(dend)
dend = as.dendrogram((o.clu.v), main = "ward.D2")
dend = set(dend, "labels_cex", 0.4)
plot(dend) #the most logical one

#finding the optimal number of clusters
correls <- numeric() # making a numeric vector
for (i in 2:(nrow(vasc.plants) - 1)) {
  # loop for possible cluster numbers. We do not use 1 (all in the same cluster) and number of samples (all in different clusters)
  clusters <- cutree(o.clu.v, k = i) # defining clusters
  clusdist <- vegdist(table(1:153, clusters), "bray")  # dist calculates Bray distance
  # distance is 0 (in different cluster) or 1 (same cluster)
  correls[i] <- cor(vd, clusdist) # correlation as a measure
}
plot(correls, type = "h", xlab = "No of clusters", ylab = "Correlation")
max.corr <- which.max(correls) # which is max correlation?
points(max.corr, correls[max.corr], pch = 16, col = "red", cex = 2)
axis(side = 1, at = max.corr, labels = max.corr, col.axis = "red")
#optimal number of clusters is 9



#Bryo
#hierarchical clustering, because we do not need to have a specific amount of clusters
o.clu.s1 <- hclust(vd1, method = "single")
o.clu.c1 <- hclust(vd1, method = "complete")
o.clu.a1 <- hclust(vd1, method = "average")
o.clu.b <- hclust(vd1, method = "ward.D2")

par(mfrow = c(1, 1))

dend = as.dendrogram((o.clu.s1), main = "single")
dend = set(dend, "labels_cex", 0.4)
plot(dend)
dend = as.dendrogram((o.clu.c1), main = "complete")
dend = set(dend, "labels_cex", 0.4)
plot(dend)
dend = as.dendrogram((o.clu.a1), main = "average")
dend = set(dend, "labels_cex", 0.4)
plot(dend)
dend = as.dendrogram((o.clu.b), main = "ward.D2")
dend = set(dend, "labels_cex", 0.4)
plot(dend) #the most logical one

#finding the optimal number of clusters
correls <- numeric() # making a numeric vector
for (i in 2:(nrow(bryo) - 1)) {
  # loop for possible cluster numbers. We do not use 1 (all in the same cluster) and number of samples (all in different clusters)
  clusters <- cutree(o.clu.b, k = i) # defining clusters
  clusdist1 <- vegdist(table(1:153, clusters), "bray")  # dist calculates Bray distance
  # distance is 0 (in different cluster) or 1 (same cluster)
  correls[i] <- cor(vd1, clusdist1) # correlation as a measure
}
plot(correls, type = "h", xlab = "No of clusters", ylab = "Correlation")
max.corr <- which.max(correls) # which is max correlation?
points(max.corr, correls[max.corr], pch = 16, col = "red", cex = 2)
axis(side = 1, at = max.corr, labels = max.corr, col.axis = "red")
#optimal number of clusters is 4

##### cut tree vascular ####
o.grel <- cutree(o.clu.v, k = 9)
dend = as.dendrogram(o.clu.v)
dend = set(dend, "labels_cex", 0.4)
plot(dend)
rect.hclust(o.clu.v, 9, border = "red")

##### cut tree BRYOPHYTES #####
o.grel1 <- cutree(o.clu.b, k = 4)
dend1 = as.dendrogram(o.clu.b)
dend1 = set(dend1, "labels_cex", 0.4)
plot(dend1)
rect.hclust(o.clu.b, 4, border = "red")

######################################################
#Step 7 : NMDS ANALYSIS and diversity related to NMDS# 
######################################################

##### NMDS analysis ####
com.vas <- log1p(vasc.plants)
com.bryo <- log1p(bryo)
o.mds <- metaMDS(com.vas, distance = "euclidean", k = 3) #vascular
stressplot(o.mds, clusdist, main= "Vascular plants") #vascular
o.mds1 <- metaMDS(com.bryo, distance = "euclidean", k= 3) #bryophytes
stressplot(o.mds1,clusdist1, main ="Bryophytes")#bryophytes

#vasc plants
plot(o.mds$points, pch = o.grel)
ordihull(o.mds, o.grel, col = 1:9)
#bryo
plot(o.mds1$points, pch = o.grel1)
ordihull(o.mds1, o.grel1, col = 1:4)

##### NMDS and Diversity #####

#Vasc plants shannon
plot(o.mds$points, pch = o.grel, col= o.grel)
ordisurf(o.mds, diversity(com.vas), col= "grey", main ="Shannon diversity vascular plants", add = T)
ordihull(o.mds, o.grel, col = 1:9)
legend("bottomleft", legend = unique(o.grel), col = unique(o.grel),
       pch = unique(o.grel), lwd = 1)

#bryo shannon 
plot(o.mds1$points, pch = o.grel1, col= o.grel1)
ordisurf(o.mds1, diversity(com.bryo), col= "grey", main ="Shannon diversity bryophytes", add = T)
ordihull(o.mds1, o.grel1, col = 1:4)
legend("bottomleft", legend = unique(o.grel1), col = unique(o.grel1),
       pch = unique(o.grel1), lwd = 1)

#Vasc plants dark & NMDS

plot(o.mds$points, pch = o.grel, col= o.grel)
ordisurf(o.mds, dark.vasc, col= "grey", main ="Dark diversity vascular plants", add = T)
ordihull(o.mds, o.grel, col = 1:9)
legend("bottomleft", legend = unique(o.grel), col = unique(o.grel),
       pch = unique(o.grel), lwd = 1)


#bryo dark & NMDS

plot(o.mds1$points, pch = o.grel1, col= o.grel1)
ordisurf(o.mds1, dark.vasc, col= "grey", main ="Dark diversity bryophytes", add = T)
ordihull(o.mds1, o.grel1, col = 1:4)
legend("bottomleft", legend = unique(o.grel1), col = unique(o.grel1),
       pch = unique(o.grel1), lwd = 1)

###########################################
#Step 8:Environment related to ordination #
##########################################

#vasc plants 
o.ev <- envfit(o.mds,df)
plot(o.mds$points,pch=o.grel, main = "Vascular plants")
plot(o.ev,add=T)
ordisurf(o.mds,df$Topsoil.pH,add=T) #contour levels for pH 

#bryo
o.ev1 <- envfit(o.mds1,df)
plot(o.mds1$points,pch=o.grel1, main = "Bryophytes", xlim=c(-6,8), ylim=c(-5,5))
plot(o.ev1,add=T)
ordisurf(o.mds1,df$Topsoil.pH,add=T) #contour levels for pH 


###### Species Distribution modelling for vascular species #####

# Calcifugous grasslands are dominated by grasses Agrostis capillaris and Festuca rubra, Galium.saxatile Festuca.rubra
soil.var <- df[,c("Topsoil.pH","Topsoil.NH4","Topsoil.NO3")]
Agro.capi <- vasc.plants[,"Agrostis.capillaris"]>0
agrocapi.tree <- tree(Agro.capi~.,data=soil.var)
plot(agrocapi.tree)
text(agrocapi.tree)

# Obtaining coordinates
xy <- map %>% select(Site.no.,lat,lon)
row.names(xy) <- xy$Site.no.
xy <- xy %>% select(-Site.no.)
xy <- xy[order(row.names(xy)), ]
all(row.names(xy)==row.names(vasc.plants))# verifying order
points(xy[Agro.capi>0,],pch=16,cex=0.8,col="darkgreen") # Actual presence-absence
# Increasing 30 units of NH4
new.soil <- soil.var
new.soil[,2] <- new.soil[,2]+30
pr.tree <- predict(agrocapi.tree,new.soil)
plot(xy,cex=pr.tree*2.5+1)
points(xy[Agro.capi>0,],pch=16,cex=0.8,col="darkgreen") # Actual presence-absence
points(xy[Agro.capi>0 & pr.tree < 1.0,],pch=16,cex=1,col="red") # Reduction of the presence

