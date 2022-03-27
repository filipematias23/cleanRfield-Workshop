############################
### cleanRfield Pipeline ###
############################

######################
### 1. First steps ###
######################

### Necessary packages ###
library(raster)
library(rgdal)
library(cleanRfield)

### Opening Sample Field 1 ###
par(mfrow=c(1,2))
EX1<-readOGR("EX1.shp")
plot(EX1, main="Data Point")
EX1.Shape<-readOGR("EX1_boundary.shp")
plot(EX1.Shape, main="Field Boundary")
par(mfrow=c(1,1))

###############################################
### 2. Cropping or selecting targeted field ###
###############################################

# "Use cursor to select 4 points around of polygon (1) in the plots window."
x11()
EX1.C<-cropField(field = EX1, nPolygon = 1, nPoint = 4)
plot(EX1.C$shape,main="Drawing Shape")

# Using the shape drawn above to crop data:
EX1.C<-cropField(field = EX1, shape = EX1.C$shape)

##############################
### 3. Sampling point data ###
##############################

# Sampling 5%:
EX1.S<-sampleField(field = EX1, 
                   size = 0.05) 

# Sampling 10% under a small shape:
EX1.S<-sampleField(field = EX1,
                   shape = EX1.C$shape, 
                   size = 0.1) 

# Sampling 10% under a full shape:
# EX1.S<-sampleField(field = EX1,
#                    shape = EX1.Shape, 
#                    size = 0.1) 

#########################
### 4. Making rasters ###
#########################

# Check projection to observe '+units=':
projection(EX1)

# Unprojected Data (non or NA): use resolution around 0.00008 to create a raster for "Dry_Yield":
EX1.R<-rasterField(field = EX1,
                   trait = c("Dry_Yield"),
                   res = 0.00008)

# Making raster only for the small shape:
EX1.R<-rasterField(field = EX1,
                   shape = EX1.C$shape,
                   trait = c("Dry_Yield"),
                   res = 0.00008) 

# Multilayer raster for two or more traits:
EX1.R<-rasterField(field = EX1,
                   trait = c("Dry_Yield","Speed"),
                   res = 0.00008) 

# Different raster color visualizations:
library(RColorBrewer)
par(mfrow=c(2,3))
plot(EX1.R$Dry_Yield)
plot(EX1.R$Dry_Yield,col = heat.colors(10))
plot(EX1.R$Dry_Yield,col = topo.colors(10))
plot(EX1.R$Dry_Yield,col = brewer.pal(11, "RdYlGn"))
plot(EX1.R$Dry_Yield,col = brewer.pal(9, "BuGn"))
plot(EX1.R$Dry_Yield,col = brewer.pal(9, "Greens"))
par(mfrow=c(1,1))

####################################
### 5. Building shape boundaries ###
####################################

# Automatic 
EX1.P<-boundaryField(field = EX1.R$Dry_Yield)

# Manually
x11()
EX1.P<-boundaryField(field = EX1, draw = TRUE)

# Drawing 3 different fields (Manually):

# Upper field:
EX1.P1<-boundaryField(field = EX1, draw = T)
# Middle field:
EX1.P2<-boundaryField(field = EX1, draw = T)
# Lower field:
EX1.P3<-boundaryField(field = EX1, draw = T)

# Giving names to each field:
EX1.P1@data<-data.frame(Field=c(1))
EX1.P2@data<-data.frame(Field=c(2))
EX1.P3@data<-data.frame(Field=c(3))

# Combining field on the same shapefile:
EX1.P<-rbind(EX1.P1,EX1.P2,EX1.P3)
plot(EX1.P)

#########################################
### 6. Buffering the field boundaries ###
#########################################

# Check projection to observe '+units=':
projection(EX1.Shape)

# Unprojected Data (e.g., non or NA): buffer of -0.0001:
EX1.B<-bufferField(shape = EX1.Shape,
                   value = -0.0001)

# Buffer of -0.0002 (Unprojected Data) and -5 (Projected Data):
EX1.B<-bufferField(shape = EX1.Shape,
                   field = EX1,
                   value = -0.0002)

######################################
### 7. Filtering using data values ###
######################################

# Observing traits histograms:
par(mfrow=c(1,2))
hist(EX1$Dry_Yield)
hist(EX1$Speed)
par(mfrow=c(1,1))

# Filtering data for Dry_Yield>60:
EX1.F<-filterField(field = EX1,
                   trait = "Dry_Yield",
                   value = 60,
                   cropAbove = T)

# Filtering data for Dry_Yield>50 and Dry_Yield<70 (only for the data on the small shapefile):
EX1.F<-filterField(field = EX1,
                   shape = EX1.C$shape,
                   trait = c("Dry_Yield","Dry_Yield"),
                   value = c(50,70),
                   cropAbove = c(T,F))

# Filtering data for Dry_Yield>70 and Speed<5 (using the buffer shapefile):
EX1.F<-filterField(field = EX1,
                   shape = EX1.B$newShape,
                   trait = c("Dry_Yield","Speed"),
                   value = c(70,5),
                   cropAbove = c(T,F))

####################################################
### 8. Filtering using standard deviation values ###
####################################################

# Filtering data for Dry_Yield sd<0.2:
EX1.SD<-sdField(field = EX1,
                trait = c("Dry_Yield"),
                value = 0.2)

# Filtering data for Dry_Yield sd<0.5 and Dry_Yield sd<0.2:
EX1.SD<-sdField(field = EX1,
                trait = c("Dry_Yield","Dry_Yield"),
                value = c(0.5,0.2))

# Filtering data for Dry_Yield sd<0.5 and Speed sd<0.2 (only for the data on the small shapefile):
EX1.SD<-sdField(field = EX1,
                shape = EX1.C$shape,
                trait = c("Dry_Yield","Speed"),
                value = c(0.5,0.2))

# Filtering data for Dry_Yield sd<0.5 and Speed sd<0.2 (using the buffer shapefile):
EX1.SD<-sdField(field = EX1,
                shape = EX1.B$newShape,
                trait = c("Dry_Yield","Speed"),
                value = c(0.5,0.2))

################################################
### 9. Evaluating multiple field on parallel ###
################################################

################
### Parallel ###
################

# Required packages
library(parallel)
library(foreach)
library(doParallel)

# Files names (folder directory: "./field/" and "./boundary/")
field<-unique(do.call(rbind,strsplit(list.files("./field/"),split = "[.]"))[,1])
boundary<-unique(do.call(rbind,strsplit(list.files("./boundary/"),split = "[.]"))[,1])

# General filter information:
buffer=-50 # Boundary buffer of 50 feet
trait = c("Dry_Yield","Speed") # Filtered traits
filter.value = c(50,7) # cropping filter values 
cropAbove = c(T,T) # All values above the filter.value
sd.value = c(1,1) # All values between sd=1

# Number of cores
n.core<-3

# Starting parallel
cl <- makeCluster(n.core, output = "")
registerDoParallel(cl)
Filtered_Field <-foreach(i = 1:length(field), 
                         .packages = c("raster","cleanRfield","rgdal")) %dopar% {
                           
                           # Uploading data and boundary
                           F.ex<-readOGR(paste("./field/",field[i],".shp",sep=""))
                           B.ex<-readOGR(paste("./boundary/",boundary[i],".shp",sep=""))
                           
                           # Filtering the borders by buffering the boundary shape file:
                           B.ex<-bufferField(shape = B.ex,value = buffer)
                           
                           # Filtering data based on observed traits values:
                           F.ex<-filterField(field = F.ex,
                                             shape = B.ex,
                                             trait = trait,
                                             value = filter.value,
                                             cropAbove = cropAbove)
                           
                           # Filtering data based on standard deviation values:
                           F.ex<-sdField(field = F.ex,
                                         shape = B.ex,
                                         trait = trait,
                                         value = sd.value)
                           
                           # New filtered data and boundary files:
                           return(list(NewField=F.ex, NewBoundary=B.ex))
                         }

stopCluster(cl)
names(Filtered_Field)<-field

# Output
Filtered_Field

# New filtered - EX2_center
plot(Filtered_Field$EX2_center$NewBoundary, main="EX2_center")
plot(Filtered_Field$EX2_center$NewField, add=T, col="gold4",pch=20,cex=0.5)

# New filtered - EX2_north
plot(Filtered_Field$EX2_north$NewBoundary, main="EX2_north")
plot(Filtered_Field$EX2_north$NewField, add=T, col="gold4",pch=20,cex=2)

# New filtered - EX2_south
plot(Filtered_Field$EX2_south$NewBoundary, main="EX2_south")
plot(Filtered_Field$EX2_south$NewField, add=T, col="gold4",pch=20,cex=1)

# Combined new data:
NewField<-rbind(Filtered_Field$EX2_center$NewField,
                Filtered_Field$EX2_north$NewField,
                Filtered_Field$EX2_south$NewField)

# Giving names to each field:
Filtered_Field$EX2_center$NewBoundary$ID<-field[1]
Filtered_Field$EX2_north$NewBoundary$ID<-field[2]
Filtered_Field$EX2_south$NewBoundary$ID<-field[3]

# Combining field on the same shape file:
NewBoundary<-rbind(Filtered_Field$EX2_center$NewBoundary,
                   Filtered_Field$EX2_north$NewBoundary,
                   Filtered_Field$EX2_south$NewBoundary)

plot(NewBoundary, main="EX2_full")
plot(NewField, add=T, col="gold4",pch=20,cex=0.5)

#######################
### 10. Making Maps ###
#######################

# Make a very basic plot where brighter colors denote higher yield
spplot(EX1, "Dry_Yield") 

#Adjusting cuts changes the number of categories in the legend
spplot(EX1, "Dry_Yield", cuts=6) 

###########
### END ###
###########

