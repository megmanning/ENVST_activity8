#activity8
#working with raster datasets and remote sensing data

#install raster package 
install.packages("raster")
library(raster)
library(rgdal)
library(ggplot2)
library(sp)

#set up directory for oneida data folder
dirR <- "/Users/margaretmanning/GitHub/ENVST_activity8/a08/oneida"

#read in sentinel data 
rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))

plot(rdatB2/10000)

#stack red green blue to better see area 
rgbS <- stack(rdatB4, rdatB3, rdatB2)/10000
#view raster, a few pixels in blue have reflectance about 1 so set scale 
plotRGB(rgbS, scale=2)

#don't need the scale argument when adding in the contrast stretch
plotRGB(rgbS, stretch="lin")

#full resolutions (plotRGB lowers resolution)
#get the total number of pixels by multiplying the number of rows and columns
#in the raster
plotRGB(rgbS, stretch="lin",maxpixels=rgbS@nrows*rgbS@ncols)

#count number of pixels in raster 
cellStats(rgbS, 'sum')

#create a false color image 
plotRGB(rgbS, r=1, g=3, b=2, stretch="lin", maxpixels=rgbS@nrows*rgbS@ncols)

#calculate NDVI
#NIR-red/(NIR+RED)
NDVI <- (rdatB8 - rdatB4)/(rdatB8 + rdatB4)
#visualize NDVI across the Oneida lake area 
plot(NDVI)

#read in land cover points data 
#info print has been turned off 
algae <- readOGR(paste0(dirR, "/Oneida/algae.shp"), verbose = FALSE)
agri <- readOGR(paste0(dirR, "/Oneida/agriculture.shp"), verbose = FALSE)
forest <- readOGR(paste0(dirR, "/Oneida/forest.shp"), verbose = FALSE)
water <- readOGR(paste0(dirR, "/Oneida/water.shp"), verbose = FALSE)
wetlands <- readOGR(paste0(dirR, "/Oneida/wetlands.shp"), verbose = FALSE)

#plot points and true color 
plotRGB(rgbS, stretch="lin",maxpixels=2297430)
plot(algae, add=TRUE, col=rgb(0.5,0.5,0.5,0.5), pch=19)
plot(agri, add=TRUE, col=rgb(0.75,0.5,0.5,0.5), pch=19)
plot(forest, add=TRUE, col=rgb(0.75,0.75,0.25,0.5), pch=19)
plot(water, add=TRUE, col=rgb(0.33,0.75,0.75,0.5), pch=19)
plot(wetlands, add=TRUE, col=rgb(0.33,0.33,0.65,0.5), pch=19)
legend("bottomleft", c("algae","agri","forest","water","wetlands"),
       pch=19, col=c(rgb(0.5,0.5,0.5,0.5), rgb(0.75,0.5,0.5,0.5),rgb(0.75,0.75,0.25,0.5),rgb(0.33,0.75,0.75,0.5),rgb(0.33,0.33,0.65,0.5)),
       bty = "n", cex = 0.75)

#set up dataframe with all of the point coordinates
landExtract <- data.frame(landcID = as.factor(rep(c("algae","water","agri","forest","wetland"),each=120)),
                          x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],forest@coords[,1],wetlands@coords[,1]),
                          y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],forest@coords[,2],wetlands@coords[,2]))

#stack all bands
allbands <- stack(rdatB2, rdatB3, rdatB4, rdatB8)/10000
#add the raster reflectance values to the point coords and classes
#extract(raster, matrix of coords)
#raster::helps ensure that extract comes from the raster package
ExtractOut <- raster::extract(allbands,landExtract[,2:3])
#name the bands
colnames(ExtractOut) <- c("B02","B03","B04","B08")
#combine the original data with the coords with the raster 
rasterEx <- cbind(landExtract,ExtractOut)
#look at data 
head(rasterEx)

ggplot(data=rasterEx, aes(x=B02, y=B03, color=landcID))+
         geom_point(alpha=0.6)+
         theme_classic()

#make plots of each band of visible light versus NIR
ggplot(data=rasterEx, aes(x=B08, y=B02, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()
ggplot(data=rasterEx, aes(x=B08, y=B03, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()
ggplot(data=rasterEx, aes(x=B08, y=B04, color=landcID))+
  geom_point(alpha=0.6)+
  theme_classic()

#extract values for landcover points from NDVI raster
ExtractNDVI <- raster::extract(NDVI,landExtract[,2:3])
#combine NDVI values with geographic coords 
rasterNDVI <- cbind(landExtract,ExtractNDVI)
#rename NDVI column
colnames(rasterNDVI) <- c("landcID","X","Y","NDVI")
#make a combo violin and box plot comparing agri, forest, wetlands
ggplot(data = rasterNDVI[rasterNDVI$landcID==c("agri","forest","wetland"),], aes(x=landcID, y=NDVI, fill=landcID))+
  geom_boxplot(width=.1)+
  geom_violin(alpha=.2)+
  theme_classic()+
  labs(title = "NDVI Distribution by Landcover Class", x="Landcover Class")

