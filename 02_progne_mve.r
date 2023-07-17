# Minimum volume ellipsoid creation for Progne Niche Evolution Project
# https://github.com/jacobccooper/progne_niches

## ENM tests of differentiation

library(tidyverse)
library(rgdal)
library(raster)
library(MASS)
library(data.table)
library(fitdistrplus)

rm(list=ls())
filepath="~/Dropbox/Manuscripts/Progne/"

envirem_path="~/Dropbox/GIS/NewWorld_current_2.5arcmin_geotiff/"
envirem_files=list.files(envirem_path,pattern='*.tif')
envirem_files=paste0(envirem_path,envirem_files)


data=read_csv(paste0(filepath,"progne_env_extract.csv"))


# We can also perform ENM tests to see how populations differ.


#Create function for individual plot formation

ssp="Progne subis"
ssp.text="Progne_subis"

nd=0.9

# Function from Jorge Soberon

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



ssp.plot=function(ssp, # species
                  ssp.text, # species for file
                  data, # dataset
                  model_files, # environment
                  nd){ # ND quantil level (confidence 1)
  y=stack(model_files)
  
  # the following is for this computer
  # get layers from model_files list
  # get final element of path name
  end_env_files=strsplit(model_files[1],"[.]")[[1]][length(strsplit(model_files[1],"[.]")[[1]])]
  keep_list=gsub(paste(envirem_path),
                 "",
                 model_files)
  keep_list=gsub(paste0(".",end_env_files),
                 "",
                 keep_list)
  
  # get name of layers only
  
  vals=data%>%
    filter(Species==ssp)%>%
    # remove character fields
    dplyr::select(c(keep_list))%>%
    # remove NA values
    na.omit()%>%
    # get unique - presence only for each site
    # uniform representation across presences
    unique()
  
  coords.vals=data%>%
    filter(Species==ssp)%>%
    # remove character fields
    dplyr::select(Long,Lat)%>%
    # get unique - presence only for each site
    # uniform representation across presences
    unique()
  
  n1=NDquantil(nrow(vals),nd)

  mve1=cov.mve(vals,quantile.use=n1)

  nc=ncell(y)
  
  mu1=matrix(mve1$center,nrow=1)
  s1=mve1$cov
  invs1=solve(s1)

  dT1=matrix(0,ncol=1,nrow=nc)
  
  #Load values for current time period
  
  valsT=raster::extract(x=y,y=seq(from=1,to=nc,by=1))
  
  #Create current models
  
  valsT1=as.matrix(valsT)
  mu2=as.matrix(mu1)
  invs2=as.matrix(invs1)
  
  for(j in 1:nrow(valsT1)){
    dT1[j,1]=maja(valsT1[j,],mu2,invs2)
  }
    
  q=raster(nrow=nrow(y),ncol=ncol(y),
         ext=extent(y),resolution=res(y),vals=dT1)
  setwd(paste0(filepath,'MVEs/raw_mve/'))
  #sp1=strsplit(sp,'[.]')[[1]][1]
  sp1=ssp.text
  writeRaster(q,filename=sp1,
              format='ascii',overwrite=T)
  plot(q)
  
  ext=raster::extract(x=q,y=coords.vals)
  ext2=na.omit(ext)
  ext2=ext2[order(ext2)]
  
  # changing from other code
  # most points near center, drop off
  # Need equation that accounts for increasing distances
  # also need to exclude any super far outliers if lots of data
  
  # gamma distribution, shape = 1
  # 
  
  eq.x=fitdist(ext2,dist="gamma",method="mle")
  
  # get rate; shape should be set to 1
  rate.x=eq.x$estimate["rate"]
  shape.x=eq.x$estimate["shape"]
  
  thresh.val.75=qgamma(0.75,shape=shape.x,rate=rate.x)
  thresh.val.85=qgamma(0.85,shape=shape.x,rate=rate.x)
  thresh.val.90=qgamma(0.90,shape=shape.x,rate=rate.x)
  thresh.val.95=qgamma(0.95,shape=shape.x,rate=rate.x)
  thresh.val.99=qgamma(0.99,shape=shape.x,rate=rate.x)
  
  #ext2
  # hierarchical matrix
  m=c(NA,NA,NA,
      0,thresh.val.75,1,
      thresh.val.75,thresh.val.85,2,
      thresh.val.85,thresh.val.90,3,
      thresh.val.90,thresh.val.95,4,
      thresh.val.95,thresh.val.99,5,
      thresh.val.99,Inf,6)
  # binary, 90% matrix
  m_bin=c(NA,NA,NA,
          0,thresh.val.90,1,
          thresh.val.90,Inf,0)
 
  # free memory
  rm(valsT)
  rm(valsT1)
  rm(dT1)
  rm(vals)
  
  #Current threshold
  ##Used only here; heirarchical for other parts
  
  m=matrix(m,ncol=3,byrow=T)
  m_bin=matrix(m_bin,ncol=3,byrow=T)
  rc=reclassify(q,m_bin)
  
  # ensure there is data for writing later
  #y2=y[which(ext>ND),]

  #Create color threshold for past models
  #color change for every standard deviation
  
  #New threshold on current conditions, then hierarchical
  #Created here, executed further down
  
  species=ssp.text

  #if(nrow(y2)!=0){
    #setwd(paste0(filepath,"MVEs/threshold_mve/"))
    #write.csv(y2,file=paste0(species,'_out.csv'),quote=F,row.names=F)
  #}
  pathway=paste0(filepath,"MVEs/threshold_mve/",
                 species,".asc",sep="")
  writeRaster(rc,pathway,overwrite=T)
  
  xlim=c(min(coords.vals$Long)-4,
         max(coords.vals$Long)+4)
  ylim=c(min(coords.vals$Lat)-4,
         max(coords.vals$Lat)+4)
  
  plot(rc,xlim=xlim,ylim=ylim)
  points(coords.vals,pch=19,col="black")

  #threshold classify tier
  thresh=reclassify(q,m)
  pathway=paste0(filepath,"MVEs/threshold_mve/",
                 species,"-tier.asc",sep="")
  writeRaster(thresh,pathway,overwrite=T)
  plot(thresh,
       xlim=xlim,ylim=ylim)
  rm(thresh)
  
  #Create color bands of how far it is from center
  
  ### For now, no past climate models.
}


### Species specific runs

ssp.plot(ssp="Progne subis",
         ssp.text="Progne_subis",
         data=data,model_files = model_files,
         nd=0.90)

ssp.plot(ssp="Progne cryptoleuca",
         ssp.text="Progne_cryptoleuca",
         data=data,model_files = model_files,
         nd=.90)

ssp.plot(ssp="Progne dominicensis",
         ssp.text="Progne_dominicensis",
         data=data,model_files = model_files,
         nd=.9)

ssp.plot(ssp="Progne sinaloae",
         ssp.text="Progne_sinaloae",
         data=data,model_files = model_files,
         nd=.9)

ssp.plot(ssp="Progne chalybea",
         ssp.text="Progne_chalybea",
         data=data,model_files = model_files,
         nd=.9)

ssp.plot(ssp="Progne elegans",
         ssp.text="Progne_elegans",
         data=data,model_files = model_files,
         nd=.9)

ssp.plot(ssp="Progne murphyi",
         ssp.text="Progne_murphyi",
         data=data,model_files = model_files,
         nd=.9)

ssp.plot(ssp="Progne modesta",
         ssp.text="Progne_modesta",
         data=data,model_files = model_files,
         nd=.9)

ssp.plot(ssp="Progne tapera",
         ssp.text="Progne_tapera",
         data=data,model_files = model_files,
         nd=.9)


### *Progne subis* subspecies modeling


subis=data%>%
  filter(Species=="Progne subis")%>%
  dplyr::select(-Species)

colnames(subis)[1]="Species" # changing to make function work

ssp.plot(ssp="subis",
         ssp.text="Progne_subis_subis",
         data=subis,model_files = model_files,
         nd=.9)

ssp.plot(ssp="arboricola",
         ssp.text="Progne_subis_arboricola",
         data=subis,model_files = model_files,
         nd=.9)

ssp.plot(ssp="mexican",
         ssp.text="Progne_subis_mexican",
         data=subis,model_files = model_files,
         nd=.9)

ssp.plot(ssp="hesperia",
         ssp.text="Progne_subis_hesperia",
         data=subis,model_files = model_files,
         nd=.9)

ssp.plot(ssp="pacific",
         ssp.text="Progne_subis_pacific",
         data=subis,model_files = model_files,
         nd=.9)
