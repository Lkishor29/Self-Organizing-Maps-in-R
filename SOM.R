#Implementing SOM and Plotting on a 2D Hexagonal Grid
#Dataset- Dow Jones Indes comprising of various stocks
#750 Values and 16 Attributes

require(kohonen)
require(RColorBrewer)
library(RCurl)
Absent <- read.csv('Absenteeism.csv')
#Need to Clean the dataset for kohonen library
colnames(Absent)
#First Plot with recatngular grid
#Pattern with 3 Attributes
#Green=Distance From Residence
#Orange=Age
#White=Day of the Week
Absent.1<-c("Distance.from.Residence.to.Work","Age","Day.of.the.week")
Absent.Som1<-som(scale(Absent[Absent.1]),grid=somgrid(6,4,"rectangular"))
plot(Absent.Som1)
colors <-function(n,alpha=1)
{
  rev(heat.colors(n,alpha))
}
plot(Absent.Som1,type="counts",palette.name=colors,heatkey=TRUE)

par(mfrow=c(1,2))
plot(Absent.Som1,type="mapping",pchs=20,main="Mapping SOM")
plot(Absent.Som1,main="Normal Som")

#Hexagonal SOMS
Absent.Som2<-som(scale(Absent[Absent.1]),grid=somgrid(6,6,"hexagonal"),toroidal = TRUE)
par(mfrow=c(1,2))
plot(Absent.Som2,type="mapping",pchs=20,main="Mapping SOM")
plot(Absent.Som2,main="Normal Som")
#Any Number of Attributes can be used
plot(Absent.Som2,type="dist.neighbours",palette.name = terrain.colors)