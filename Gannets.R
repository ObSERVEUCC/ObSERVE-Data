alive<-read.csv('//users//oriolgiraltparadell//documents//07 Publications//01 Dead Gannets study//R//alive_gannets.csv',sep=',')
dead<-read.csv('//users//oriolgiraltparadell//documents//07 Publications//01 Dead Gannets study//R//dead_gannets.csv',sep=',')
alive
dead
library(plotrix)

# Calculating SE and CVs
## for alive gannets
se<-std.error(alive$BirdsKm2)
me <-mean(alive$BirdsKm2, na.rm=TRUE)
CV<-se/me
CV

## for dead gannets
sed<-std.error(dead$BirdsKm2)
med <-mean(dead$BirdsKm2, na.rm=TRUE)

CVd<-sed/med
CVd


#Calculating lower and upper CIs
## Alive gannets
cal<-exp(1.96*sqrt(log(1+CV^2)))
cal
LCIa<-39427/cal
UCIa<-39427*cal

## Dead gannets
cald<-exp(1.96*sqrt(log(1+CVd^2)))
cald
LCId<-1540/cald
UCId<-1540*cald


x1<-rnorm(50,2,0.25)
b1<-boot(x1,function(u,i) mean(u[i]),R=1000)
boot.ci(b1,type=c("norm","basic","perc"))



## CACLCULATIONS FOR CUT AREA
CAalive<-read.csv('//users//oriolgiraltparadell//documents//07 Publications//01 Dead Gannets study//R//alive_gannets_cut_area.csv',sep=',')
CAdead<-read.csv('//users//oriolgiraltparadell//documents//07 Publications//01 Dead Gannets study//R//dead_gannets_cut_area.csv',sep=',')
CAalive
CAdead
library(plotrix)

# Adding summing 
Aestim<-sum(CAalive$BirdsCellCut)
Destim<-sum(CAdead$BirdsCellCut)

# Calculating SE and CVs
## for alive gannets
CAse<-std.error(CAalive$BirdsCellCut)
CAme <-mean(CAalive$BirdsCellCut, na.rm=TRUE)
CACV<-CAse/CAme
CACV

## for dead gannets
CAsed<-std.error(CAdead$BirdsCellCut)
CAmed <-mean(CAdead$BirdsCellCut, na.rm=TRUE)

CACVd<-CAsed/CAmed
CACVd


#Calculating lower and upper CIs
## Alive gannets
CAcal<-exp(1.96*sqrt(log(1+CACV^2)))
CAcal
CALCIa<-Aestim/CAcal
CAUCIa<-Aestim*CAcal

## Dead gannets
CAcald<-exp(1.96*sqrt(log(1+CACVd^2)))
CAcald
CALCId<-Destim/CAcald
CAUCId<-Destim*CAcald

# Loading packages
library(sf)
library(rgdal)
library(adehabitatHR)


tracks <- st_read(
  "//users//oriolgiraltparadell//documents//07 Publications//01 Dead Gannets study//R//tracking_data.shp") 

tracks.UTM <- st_transform(tracks, CRS("+init=epsg:32629")) # Reprojecting data to UTM




track<-read.csv('//users//oriolgiraltparadell//documents//07 Publications//01 Dead Gannets study//R//Tracking_data.csv',sep=',')
track<-read.csv('//users//oriolgiraltparadell//documents//07 Publications//01 Dead Gannets study//R//Tracking_data_LS.csv',sep=',')
track<-read.csv('//users//oriolgiraltparadell//documents//07 Publications//01 Dead Gannets study//R//Tracking_data_BR.csv',sep=',')
track
track$datePOS <-paste(track$DATE, track$TIME) # Merging column Date and Time to create a column with the combined information
track$datePOS<-as.POSIXct(track$datePOS) # Converting the column created in the previous step to POSIXct format

#Creating spatial points data
coords <- SpatialPoints(track[, c("X", "Y")])
cordsspa <- SpatialPointsDataFrame(coords,track)
proj4string(cordsspa) <- CRS("+init=epsg:32629")
head(cordsspa[,12])

getClass(cordsspa)


UDK<-kernelUD(cordsspa[,12], h = "href", grid = 500, extent = 1, same4all=TRUE) #Estimating the Kernel UD
image(UDK)
print(UDK)
UDK

class(UDK)
x <- adehabitatHR::estUDm2spixdf(UDK) 
image(x)
xcont <- adehabitatHR::getverticeshr(UDK, percent = 90) #Obtaining the 90% contour
xcont50 <- adehabitatHR::getverticeshr(UDK, percent = 50) #Obtaining the 50% contour
plot(xcont)
plot(xcont50)

#Exporting the contour as a shapefile
class(xcont)
xcontshp<-st_as_sf(xcont)
st_write(xcontshp,'//users//oriolgiraltparadell//documents//07 Dead Gannets study//R//Foraging_range_90.shp')
st_write(xcontshp,'//users//oriolgiraltparadell//documents//07 Dead Gannets study//R//Foraging_range_90_LS.shp')
st_write(xcontshp,'//users//oriolgiraltparadell//documents//07 Dead Gannets study//R//Foraging_range_90_BR.shp')

class(xcont50)
xcontshp50<-st_as_sf(xcont50)
st_write(xcontshp50,'//users//oriolgiraltparadell//documents//07 Dead Gannets study//R//Foraging_range_50.shp')
st_write(xcontshp50,'//users//oriolgiraltparadell//documents//07 Dead Gannets study//R//Foraging_range_50_LS.shp')
st_write(xcontshp50,'//users//oriolgiraltparadell//documents//07 Dead Gannets study//R//Foraging_range_50_BR.shp')



