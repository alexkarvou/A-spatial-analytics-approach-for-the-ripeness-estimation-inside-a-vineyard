library(rgdal)
setwd("C:/Users/karvo/Desktop/vinocular/lytras winery/large")
spdf<-readGDAL('DJI_0001.tif')
spdf<-readGDAL('lytras_1.tif')
spdf<-readGDAL('DJI_0001.JPG')
spdf<-readGDAL('asdaf.tif')
df<-as.data.frame(spdf)
View(head(df))
#################
#install.packages('rnaturalearth')
library(tmap)
data(Europe)
tm_shape(Europe)+
  tm_borders()+
  tm_text('name',size='pop_est')
data(land)
data(World)
data(rivers)
data(metro)
tm_shape(Europe,is.master = TRUE)+
  tm_polygons()+
  tm_text('iso_a3',size='area')+
tm_shape(land)+
  tm_raster('elevation')+
tm_shape(rivers)+
  tm_lines(col="dodgerblue3",lwd='strokelwd')
  

############THESIS MAP
library(sp)
library(tmap)
data(Europe,land,rivers)
library(RColorBrewer)
library(gridExtra)
mypal<-brewer.pal(n=9,name='BuGn')
tm_shape(land)+
  tm_raster('trees', palette = mypal,legend.show = FALSE)+
tm_shape(Europe,is.master = TRUE)+
  tm_borders()+
tm_shape(rivers)+
  tm_lines(col='blue4',lwd=2)+
tm_shape(Europe)+
  tm_text('iso_a3',size='pop_est',legend.size.show = FALSE,scale=1.5)+
tm_layout('Trees and Rivers in Europe',legend.position = c('left','top'),
          legend.title.size = 3)
#####spatial 1
t1<-tm_shape(Europe)+
  tm_bubbles()+
  tm_grid()+
  tm_layout(title='Indicative location')

t2<-tm_shape(Europe)+
  tm_text('iso_a3',scale = 0.6)+
  tm_layout(title='Code of country')
t3<-tm_shape(Europe)+
  tm_text('pop_est',scale=0.6)+
  tm_layout(title='Estimated Population')
t4<-tm_shape(Europe)+
  tm_text('iso_a3',size='pop_est',legend.size.show = FALSE)+
  tm_layout(title='Estimated Population through code size',title.position = c('right','top'))
tmap_arrange(t1,t2,t3,t4)

t1<-tm_shape(Europe)+
  tm_dots()
t2<-tm_shape(Europe)+
  tm_text('iso_a3',scale=.48)
tmap_arrange(t1,t2)

tm_shape(Europe,is.master = TRUE)+
  tm_borders(col = 'white')+
tm_shape(rivers)+
  tm_lines(col='blue4',lwd=2)

t1<-tm_shape(Europe)+
  tm_borders()
t2<-tm_shape(Europe)+
  tm_fill()
tmap_arrange(t1,t2)

tm_shape(Europe,is.master = TRUE)+
  tm_borders(col='white')+
tm_shape(land)+
  tm_raster('trees', palette = mypal,legend.show = FALSE)

#######################
library(ggplot2)
library(RColorBrewer)
library(viridisLite)
library(gridExtra)
library(viridis)
library(ggmap)
library(rgdal)
library(gstat)
data(meuse)
coordinates(meuse)<-c('x','y')
proj4string(meuse) <- CRS("+init=epsg:28992")                   # set original projection
meuse <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
spplot(meuse,'zinc',do.log=T,colorkey=T)
bubble(meuse,'zinc',do.log=T,key.space = 'bottom')
map <- get_map(location=rowMeans(bbox(meuse)), zoom=13,maptype='satellite',color='bw')   # get Google map
ggmap(map,extent ='device')+
ggplot(data=as.data.frame(meuse),aes(x,y))+
  geom_point()+
  scale_color_gradientn(colours=vir)
ggplot(data=as.data.frame(meuse),aes(sqrt(dist),log(zinc)))+
  geom_point(size=3,alpha=.7,col='slateblue4')+
  theme_minimal()+
  ggtitle('Zinc concentration and distance to the river')
zn.lm<-lm(log(zinc)~sqrt(dist),meuse)  
meuse$fitted<-predict(zn.lm,meuse)#-mean(predict(zn.lm,meuse))
meuse$residuals<-zn.lm$residuals
#spplot(meuse,c('fitted','residuals'))
vir<-viridis(n=20)
g1<-ggplot(data=as.data.frame(meuse),aes(x,y,color=predicted))+
  geom_point(size=2)+
  theme_minimal()+
  scale_color_gradientn(colours = vir)+
  ggtitle('Predicted')
g1<-ggplot(data=as.data.frame(meuse),aes(sqrt(dist),log(zinc)))+
  geom_point(size=3,alpha=.7,col='slateblue4')+
  theme_minimal()+
  ggtitle('Zinc concentration and distance to the river')
g2<-ggplot(data=as.data.frame(meuse),aes(x,y,color=residuals))+
  geom_point(size=2)+
  theme_minimal()+
  scale_color_gradientn(colours = vir)+
  ggtitle('Residuals')
grid.arrange(g1,g2,ncol=2)
library(gstat)
hscat(log(zinc)~1,meuse,(0:9)*100)
par(mfrow=c(2,2))
plot(variogram(log(zinc)~1,meuse,cloud=TRUE))
plot(variogram(log(zinc)~1,meuse))
plot(variogram(log(zinc)~1,meuse,cloud=TRUE),digitize=TRUE)
v1<-variogram(log(zinc)~1,meuse,cloud=TRUE)
v2<-variogram(log(zinc)~1,meuse)
g1<-ggplot(v1,aes(dist,gamma))+
  geom_point(size=2,alpha=.7,color='slateblue4')+
  theme_minimal()+
  ggtitle('Variogram Cloud')
g2<-ggplot(v2,aes(dist,gamma,label=np))+
  #geom_point(size=5,alpha=.7,color='slateblue4')+
  geom_text()+
  theme_minimal()+
  ggtitle('Sample Variogram')
grid.arrange(g1,g2,ncol=1)