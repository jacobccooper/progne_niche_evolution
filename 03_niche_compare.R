# Creation of random models and iterations of niche comparions for Progne Niche Evolution Project
# https://github.com/jacobccooper/progne_niches

# Introduction

# Load necessary packages.

library(data.table)
library(dismo)
library(MASS)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

filepath="~/Dropbox/Manuscripts/Progne/"
GISpath="~/Dropbox/Manuscripts/Progne/Ms/"

land=readOGR("~/Dropbox/GIS/ne_10m_land/")

envirem_path="~/Dropbox/GIS/NewWorld_current_2.5arcmin_geotiff/"
envirem_files=list.files(envirem_path,pattern='*.tif')
envirem_files=paste0(envirem_path,envirem_files)



data=read_csv(paste0(filepath,"progne_env_extract.csv"))


# Load required functions.


#MAJA function
maja=function(p,m,s)((p-m)%*%s%*%t(p-m))^0.5

#Quantile function
##Double check function? divide by 1 is 1...
##changed to 4, for quantiles
NDquantil=function(nD,level){
  return(round(nD*level))
}

model_files=envirem_files[-which(envirem_files%like%"_tri"|
                                   envirem_files%like%"_continentality"|
                                   envirem_files%like%"growingDegDays"|
                                   envirem_files%like%"_maxTempColdest"|
                                   envirem_files%like%"_monthCount"|
                                   envirem_files%like%"PETColdest"|
                                   envirem_files%like%"PETseason"|
                                   envirem_files%like%"thermicity")]


# Niche comparisons

# In order to do comparisons, we have to create 100 null models as well; these models are "blanks" against which we can check each individual species. These distributions need to be pulled from shapefiles around their defined distributions, so as to mimic random distributions from within each species invasible area.

### Randomizer

# Perform random niche model iterations, and then create the comparisons. DO NOT save random models, since we do not have space on the computer to do that.


# stack model files
y=stack(model_files)

nc=ncell(y)
valsT=raster::extract(x=y,y=seq(from=1,to=nc,by=1))
valsT1=as.matrix(valsT)
rm(valsT)

# testing function with modesta

data=data
type="random" # sampling method
sp.text="Progne sinaloae"
shp.text="progne-sinaloae_accessible"
nd=0.9
raster.stack=y
# preloaded land shapefile
dist=10 # distance in KM for rarefy
# dist removed

p.subis=raster(paste0(filepath,"MVEs/raw_mve/Progne_subis.asc"))
p.cryptoleuca=raster(paste0(filepath,"MVEs/raw_mve/Progne_cryptoleuca.asc"))
p.dominicensis=raster(paste0(filepath,"MVEs/raw_mve/Progne_dominicensis.asc"))
p.sinaloae=raster(paste0(filepath,"MVEs/raw_mve/Progne_sinaloae.asc"))
p.chalybea=raster(paste0(filepath,"MVEs/raw_mve/Progne_chalybea.asc"))
p.elegans=raster(paste0(filepath,"MVEs/raw_mve/Progne_elegans.asc"))
p.murphyi=raster(paste0(filepath,"MVEs/raw_mve/Progne_murphyi.asc"))
p.modesta=raster(paste0(filepath,"MVEs/raw_mve/Progne_modesta.asc"))
p.tapera=raster(paste0(filepath,"MVEs/raw_mve/Progne_tapera.asc"))

sp.rast=p.sinaloae

randomizer=function(data,type,sp.text,shp.text,nd,raster.stack,land,sp.rast){
  # set to GISpath
  
  subdata=data%>%filter(Species==sp.text)
  
  x=readOGR(paste0(GISpath,shp.text,'.gpkg'))
  # fit to landmass
  land.sf=st_as_sf(land)
  x.sf=st_as_sf(x)
  x=st_intersection(land.sf,x.sf)
  x=as_Spatial(x)
  
  
  nc=ncell(raster.stack)
  
  dT1=matrix(0,ncol=1,nrow=nc)

  nx=nrow(subdata)
  
  comparisons=matrix(data=NA,nrow=101,ncol=9)
  colnames(comparisons)=c("Progne_subis","Progne_cryptoleuca","Progne_dominicensis",
                          "Progne_sinaloae","Progne_chalybea","Progne_elegans",
                          "Progne_murphyi","Progne_modesta","Progne_tapera")
  
  # perform initial comparisons
  comparisons[1,1]=nicheOverlap(sp.rast,p.subis,stat="D")
  comparisons[1,2]=nicheOverlap(sp.rast,p.cryptoleuca,stat="D")
  comparisons[1,3]=nicheOverlap(sp.rast,p.dominicensis,stat="D")
  comparisons[1,4]=nicheOverlap(sp.rast,p.sinaloae,stat="D")
  comparisons[1,5]=nicheOverlap(sp.rast,p.chalybea,stat="D")
  comparisons[1,6]=nicheOverlap(sp.rast,p.elegans,stat="D")
  comparisons[1,7]=nicheOverlap(sp.rast,p.murphyi,stat="D")
  comparisons[1,8]=nicheOverlap(sp.rast,p.modesta,stat="D")
  comparisons[1,9]=nicheOverlap(sp.rast,p.tapera,stat="D")
  
  sp1=gsub(" ","_",sp.text)
  
  for(i in 1:100){
    #yy=spsample(x=x,n=nx,type=type,cellsize=(dist*1000))
    yy=spsample(x=x,n=nx,type="random")
    #Alternate method, not as effective
    #yy=randomPoints(mask=x,n=nrow(data),
    #                p=data[,2:3],excludep=T,
    #                cellnumbers=F,tryf=5)
    
    yy2=as.data.frame(coordinates(yy))
    colnames(yy2)=c("Long","Lat")
    yy2$Population=sp.text
    yy2=yy2[,c('Population','Long','Lat')]
    
    vals=raster::extract(x=raster.stack,y=yy2[,c("Long","Lat")])
    vals=na.omit(vals)
    vals=unique(vals)
    #vals=vals[,-10]

    n1=NDquantil(nrow(vals),nd)

    #for(i in 1:ncol(vals)){print(IQR(vals[,i]))}

    mve1=cov.mve(vals,quantile.use=n1)

    mu1=matrix(mve1$center,nrow=1)
    s1=mve1$cov
    invs1=solve(s1)
    
    dT1=matrix(0,ncol=1,nrow=nc)
    
    mu2=as.matrix(mu1)
    invs2=as.matrix(invs1)
      
    for(j in 1:nrow(valsT1)){
      dT1[j,1]=maja(valsT1[j,],mu2,invs2)
    }
    
    #q=raster(nrow=nrow(y),ncol=ncol(y),
    #     ext=extent(y),resolution=res(y),vals=dT1)
    write.csv(yy2,file=paste0(filepath,'Ecological_Analysis/no-out/random/',
                              sp1,"_random-",i,'.csv'),quote=F,row.names=F)
    #writeRaster(q,filename=paste0(filepath,'Ecological_Analysis/no-out/random/',
    #sp.text,"_random-",i),format='ascii',overwrite=T)
    
    q1=raster(nrow=nrow(sp.rast),ncol=ncol(sp.rast),
              ext=extent(sp.rast),resolution=res(sp.rast),
              vals=dT1)
    
    # perform pairwise iterations
    skipcol=which(colnames(comparisons)%like%sp1)
    for(ii in 1:ncol(comparisons)){
      if(ii==skipcol){next}else{
        if(ii==1){test.rast=p.subis}
        if(ii==2){test.rast=p.cryptoleuca}
        if(ii==3){test.rast=p.dominicensis}
        if(ii==4){test.rast=p.sinaloae}
        if(ii==5){test.rast=p.chalybea}
        if(ii==6){test.rast=p.elegans}
        if(ii==7){test.rast=p.murphyi}
        if(ii==8){test.rast=p.modesta}
        if(ii==9){test.rast=p.tapera}
        comparisons[(i+1),ii]=nicheOverlap(q1,test.rast,stat="D")
      }
      rm(test.rast)
    }
  }
  write_csv(as.data.frame(comparisons),paste0(filepath,sp1,"_mve_comparisons.csv"))
}



randomizer(data=data,
           type='random',sp.text="Progne modesta",
           shp.text = "progne-modesta_accessible",
           nd=0.9,raster.stack = y,
           land=land,sp.rast=p.modesta)
randomizer(data=data,
           type='random',sp.text="Progne chalybea",
           shp.text = "progne-chalybea_accessible",
           nd=0.9,raster.stack = y,
           land=land,sp.rast=p.chalybea)
randomizer(data=data,
           type='random',sp.text="Progne cryptoleuca",
           shp.text = "progne-cryptoleuca_accessible",
           nd=0.9,raster.stack = y,
           land=land,sp.rast = p.cryptoleuca)
randomizer(data=data,
           type='random',sp.text="Progne dominicensis",
           shp.text = "progne-dominicensis_accessible",
           nd=0.9,raster.stack = y,
           land=land,sp.rast = p.dominicensis)
randomizer(data=data,
           type='random',sp.text="Progne elegans",
           shp.text = "progne-elegans_accessible",
           nd=0.9,raster.stack = y,
           land=land,sp.rast = p.elegans)
randomizer(data=data,
           type='random',sp.text="Progne murphyi",
           shp.text = "progne-murphyi_accessible",
           nd=0.9,raster.stack = y,
           land=land,sp.rast = p.murphyi)
randomizer(data=data,
           type='random',sp.text="Progne sinaloae",
           shp.text = "progne-sinaloae_accessible",
           nd=0.9,raster.stack = y,
           land=land,sp.rast=p.sinaloae)
randomizer(data=data,
           type='random',sp.text="Progne subis",
           shp.text = "progne-subis_accessible",
           nd=0.9,raster.stack = y,
           land=land,sp.rast=p.subis)
randomizer(data=data,
           type='random',sp.text="Progne tapera",
           shp.text = "progne-tapera_accessible",
           nd=0.9,raster.stack = y,
           land=land,sp.rast = p.tapera)




### Between subspecies

# We are comparing within the lineages of *Progne subis* to determine if there are any differences. This is limited to North America to ease processing.


# stack model files
y=stack(model_files)
limits=readOGR(paste0(filepath,"Ms/progne_subis_crop.gpkg"))

y=crop(y,extent(limits))
y=mask(y,limits)

nc=ncell(y)
subis.valsT=raster::extract(x=y,y=seq(from=1,to=nc,by=1))
subis.valsT1=as.matrix(subis.valsT)
rm(subis.valsT)

# testing function with subis arboricola

data=data
type="random" # sampling method
sp.text="pacifica"
shp.text="progne_subis_westcoast"
nd=0.9
raster.stack=y
# preloaded land shapefile
dist=10 # distance in KM for rarefy
# dist removed

p.s.pacifica=raster(paste0(filepath,"MVEs/raw_mve/Progne_subis_pacifica.asc"))%>%
  crop(extent(limits))%>%
  mask(limits)
p.s.enigmae=raster(paste0(filepath,"MVEs/raw_mve/Progne_subis_enigmae.asc"))%>%
  crop(extent(limits))%>%
  mask(limits)
p.s.arboricola=raster(paste0(filepath,"MVEs/raw_mve/Progne_subis_arboricola.asc"))%>%
  crop(extent(limits))%>%
  mask(limits)
p.s.hesperia=raster(paste0(filepath,"MVEs/raw_mve/Progne_subis_hesperia.asc"))%>%
  crop(extent(limits))%>%
  mask(limits)
p.s.subis=raster(paste0(filepath,"MVEs/raw_mve/Progne_subis_subis.asc"))%>%
  crop(extent(limits))%>%
  mask(limits)

sp.rast=p.s.pacifica

randomizer.subspecies=function(data,type,sp.text,shp.text,nd,raster.stack,land,sp.rast){
  # set to GISpath
  
  subdata=data%>%
    filter(Species=="Progne subis")%>%
    filter(Subspecies==sp.text)
  
  x2=readOGR(paste0(GISpath,shp.text,'.gpkg'))
  # fit to landmass
  land.sf=st_as_sf(land)
  x.sf=st_as_sf(x2)
  st_crs(x.sf)=st_crs(land.sf)
  x=st_intersection(land.sf,x.sf)
  x=as_Spatial(x)
  
  nc=ncell(raster.stack)
  
  dT1=matrix(0,ncol=1,nrow=nc)

  nx=nrow(subdata)
  
  comparisons=matrix(data=NA,nrow=101,ncol=5)
  colnames(comparisons)=c("P_s_subis","P_s_arboricola","P_s_hesperia",
                          "P_s_pacifica","P_s_enigmae")
  
  # perform initial comparisons
  comparisons[1,1]=nicheOverlap(sp.rast,p.s.subis,stat="D")
  comparisons[1,2]=nicheOverlap(sp.rast,p.s.arboricola,stat="D")
  comparisons[1,3]=nicheOverlap(sp.rast,p.s.hesperia,stat="D")
  comparisons[1,4]=nicheOverlap(sp.rast,p.s.pacifica,stat="D")
  comparisons[1,5]=nicheOverlap(sp.rast,p.s.enigmae,stat="D")
  
  sp1=gsub(" ","_",sp.text)
  
  for(i in 1:100){
    #yy=spsample(x=x,n=nx,type=type,cellsize=(dist*1000))
    yy=spsample(x=x,n=nx,type="random")
    #Alternate method, not as effective
    #yy=randomPoints(mask=x,n=nrow(data),
    #                p=data[,2:3],excludep=T,
    #                cellnumbers=F,tryf=5)
    
    yy2=as.data.frame(coordinates(yy))
    colnames(yy2)=c("Long","Lat")
    yy2$Population=sp.text
    yy2=yy2[,c('Population','Long','Lat')]
    
    vals=raster::extract(x=raster.stack,y=yy2[,c("Long","Lat")])
    vals=na.omit(vals)
    vals=unique(vals)
    #vals=vals[,-10]

    n1=NDquantil(nrow(vals),nd)

    #for(i in 1:ncol(vals)){print(IQR(vals[,i]))}

    mve1=cov.mve(vals,quantile.use=n1)

    mu1=matrix(mve1$center,nrow=1)
    s1=mve1$cov
    invs1=solve(s1)
    
    dT1=matrix(0,ncol=1,nrow=nc)
    
    mu2=as.matrix(mu1)
    invs2=as.matrix(invs1)
      
    for(j in 1:nrow(subis.valsT1)){
      dT1[j,1]=maja(subis.valsT1[j,],mu2,invs2)
    }
    
    q1=raster(nrow=nrow(sp.rast),ncol=ncol(sp.rast),
              ext=extent(sp.rast),resolution=res(sp.rast),
              vals=dT1)
    
    #q=raster(nrow=nrow(y),ncol=ncol(y),
    #     ext=extent(y),resolution=res(y),vals=dT1)
    write.csv(yy2,file=paste0(filepath,'Ecological_Analysis/no-out/random/',
                              sp1,"_random-",i,'.csv'),quote=F,row.names=F)
    #writeRaster(q,filename=paste0(filepath,'Ecological_Analysis/no-out/random/',
    #sp.text,"_random-",i),format='ascii',overwrite=T)
    
    # perform pairwise iterations
    skipcol=which(colnames(comparisons)%like%sp1)
    for(ii in 1:ncol(comparisons)){
      if(ii==skipcol){next}else{
        if(ii==1){test.rast=p.s.subis}
        if(ii==2){test.rast=p.s.arboricola}
        if(ii==3){test.rast=p.s.hesperia}
        if(ii==4){test.rast=p.s.pacifica}
        if(ii==5){test.rast=p.s.enigmae}
        comparisons[(i+1),ii]=nicheOverlap(q1,
                                           test.rast,stat="D")
      }
      rm(test.rast)
    }
  }
  write_csv(as.data.frame(comparisons),paste0(filepath,sp1,"_mve_comparisons.csv"))
}



randomizer.subspecies(data=data,type='random',
                      sp.text="subis",shp.text = "progne_subis_subis",
                      nd=0.9,raster.stack = y,
                      land=land,sp.rast = p.s.subis)
randomizer.subspecies(data=data,type='random',
                      sp.text="arboricola",shp.text = "progne_subis_arboricola",
                      nd=0.9,raster.stack = y,
                      land=land,sp.rast = p.s.arboricola)
randomizer.subspecies(data=data,type='random',
                      sp.text="pacifica",shp.text = "progne_subis_westcoast",
                      nd=0.9,raster.stack = y,
                      land=land,sp.rast = p.s.pacifica)
randomizer.subspecies(data=data,type='random',
                      sp.text="enigmae",shp.text = "progne_subis_unknown",
                      nd=0.9,raster.stack = y,
                      land=land,sp.rast = p.s.enigmae)
randomizer.subspecies(data=data,type='random',
                      sp.text="hesperia",shp.text = "progne_subis_hesperia",
                      nd=0.9,raster.stack = y,
                      land=land,sp.rast = p.s.hesperia)


# end