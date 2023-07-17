# Running the package NichEvol for Progne Niche Evolution Project
# https://github.com/jacobccooper/progne_niches

# declare your filepath
filepath="~/Dropbox/Manuscripts/Progne/"


library(geiger)
library(nichevol)
library(raster)
library(tidyverse)
library(data.table)


# Prep data layers


GISpath="~/Dropbox/GIS/NewWorld_current_2.5arcmin_geotiff/"
elevpath="~/Dropbox/GIS/elev_NewWorld_current_2.5arcmin_geotiff/"


# test with one
precip=raster(paste0(GISpath,"current_2-5arcmin_annualPET.tif"))

m_files=list.files(paste0(filepath,"Ms/"),pattern="*accessible.gpkg")

occ_list=read_csv(paste0(filepath,"progne_env_extract.csv"))

p.tree=read.tree(paste0(filepath,"Sequences/cytb/RAxML_bestTree.cytb_noboot"))



# fix tree
fnames=list.files(paste0(filepath,"Sequences/cytb/"),pattern="*-cytb.fasta")

p.tree$tip.label

for(i in 1:length(p.tree$tip.label)){
  lab=p.tree$tip.label[i]
  fname.x=fnames[which(fnames%like%lab)]
  sp=strsplit(fname.x,"-")[[1]][2]
  p.tree$tip.label[i]=paste0(lab,"-",sp)
}


plot.phylo(p.tree,cex=0.65)


# all Tachycineta group together
# per Moyle, tapera is the outgroup to all other martins
# root to tapera
p.tree2=drop.tip(p.tree,tip = which(p.tree$tip.label%like%"Tachycineta"))
p.tree2=root(p.tree2,outgroup = p.tree$tip.label[which(p.tree$tip.label%like%"tapera")])
plot.phylo(p.tree2,cex = 0.65)


# This reflects the other phylogenies - including the confusingly placed *Progne chalybea*! Given that *Progne chalybea* has uncertain placement, I am leaving tips in two parts of the tree; we can then compare how this looks across the project. From Clare Brown's dissertation, we know that *chalybea* is monophyletic, and that it is not sister to *elegans*.


keepers=c("AF074588.1-Progne_tapera","AY825948.1-Progne_chalybea","AY825947.1-Progne_sinaloae",
          "AY825946.1-Progne_dominicensis","AY825945.1-Progne_cryptoleuca","AY825949.1-Progne_elegans",
          "EU427745.1-Progne_subis_arboricola","AY825950.1-Progne_murphyi","MH307578.1-Progne_modesta")

p.tree3=drop.tip(p.tree2,c(which(p.tree2$tip.label%in%keepers==F)))
plot(p.tree3)  



# rename tips to SP only
for(i in 1:length(p.tree3$tip.label)){
  sp=strsplit(p.tree3$tip.label[i],"-")[[1]][2]
  if(sp%like%'subis'){
    sp=gsub("_arboricola","",sp)
  }
  sp=gsub("_"," ",sp)
  p.tree3$tip.label[i]=sp
}

# reformatted other files to be the same name elsewhere


# We need to split occurrences into separate files, and put them in a separate folder.


# split occ_list into files named the same as the tips

spp=unique(occ_list$Species)

for(i in 1:length(spp)){
  sp=spp[i]
  occ_list%>%
    filter(Species==sp)%>%
    write_csv(file=paste0(filepath,"occ_dir/",
                          gsub(" ","_",sp),".csv"))
}



GIS="~/Dropbox/GIS/NewWorld_current_2.5arcmin_geotiff/"


# There is an error in the code. I removed it and then it was able to run. Specifically, in line 86, I removed reference to the "species" corresponding to that iteration. Note also that the files in each have to be an exact match.


stats_evalues2(stats=c("mean","median","sd","range"),
              M_folder=paste0(filepath,"Ms/name_species_tree/"),M_format="gpkg",
              occ_folder=paste0(filepath,"occ_dir/"),
              longitude = "Long",latitude = "Lat",
              var_folder=GIS,var_format="GTiff", # must specify GTiff
              save=T,output_directory = paste0(filepath,"nichevol/"),
              overwrite = T)


# Testing the next section with annual PET.


# load both files for annualPET
progne.stats=
  read_csv(paste0(filepath,
                  "nichevol/current_2.5arcmin_annualPET_M_stats.csv"))
m.stats=
  read_csv(paste0(filepath,
                  "nichevol/current_2.5arcmin_annualPET_Occurrence_stats.csv"))

M_folder=paste0(filepath,"Ms/name_species_tree/")
M_format="gpkg"
occ_folder=paste0(filepath,"occ_dir/")
longitude = "Long"
latitude = "Lat"
percentage_out = 0
bin_size = 10
var_folder=GIS
var_format="GTiff"
multiplication_factor = 1
round = FALSE
save = FALSE
output_directory=paste0(filepath,"bins/")
overwrite = T


bintables_jc=function (M_folder, M_format, occ_folder, longitude, latitude, 
						var_folder, var_format, round = FALSE, round_names, 
						multiplication_factor = 1, percentage_out = 5, 
						bin_size = 10, save = FALSE, output_directory,
						overwrite = FALSE){
    if (missing(M_folder)) {
        stop("Argument 'M_folder' is missing.")
    }
    if (missing(M_format)) {
        stop("Argument 'M_format' is missing.")
    }
    if (missing(occ_folder)) {
        stop("Argument 'occ_folder' is missing.")
    }
    if (missing(longitude)) {
        stop("Argument 'longitude' is missing.")
    }
    if (missing(latitude)) {
        stop("Argument 'latitude' is missing.")
    }
    if (missing(var_folder)) {
        stop("Argument 'var_folder' is missing.")
    }
    if (missing(var_format)) {
        stop("Argument 'var_format' is missing.")
    }
    if (save == TRUE) {
        if (missing(output_directory)) {
            stop("Argument 'output_directory' is missing.")
        }else{
            if (overwrite == FALSE & dir.exists(output_directory)) {
                stop("'output_directory' already exists, to replace it use overwrite = TRUE.")
            }
            if (overwrite == TRUE & dir.exists(output_directory)) {
                unlink(x = output_directory, recursive = TRUE, 
                  force = TRUE)
            }
        }
    }
    message("\nPreparing data, please wait...\n")
    if (M_format %in% c("shp", "gpkg")) {
        if (M_format == "shp") {
            M_patt <- ".shp$"
            subs <- ".shp"
            mlist <- gsub(subs, "", list.files(path = M_folder, 
                pattern = M_patt))
            spnames <- mlist
        }else{
            M_patt <- ".gpkg$"
            subs <- ".gpkg"
            mlist <- list.files(path = M_folder, pattern = M_patt)
            spnames <- gsub(subs, "", mlist)
        }
    } else {
        M_patt <- paste0(rformat_type(var_format), "$")
        subs <- rformat_type(var_format)
        mlist <- list.files(path = M_folder, pattern = M_patt, 
            full.names = TRUE)
        spnames <- gsub(subs, "", list.files(path = M_folder, 
            pattern = M_patt))
    }
    v_patt <- paste0(rformat_type(var_format), "$")
    occlist <- list.files(path = occ_folder, pattern = ".csv$", 
        full.names = TRUE)
    variables <- raster::stack(list.files(path = var_folder, 
        pattern = v_patt, full.names = TRUE))
    if (round == TRUE) {
        rounds <- round(variables[[round_names]] * multiplication_factor)
        var_names <- names(variables)
        noround <- var_names[!var_names %in% round_names]
        variables <- raster::stack(variables[[noround]], rounds)
    }
    if (save == TRUE) {
        dir.create(output_directory)
    }
    message("Preparing range values and bin tables from environmental layers and species data:")
    nvars <- raster::nlayers(variables)
    bin_tabs <- lapply(1:nvars, function(i) {
        M_range <- list()
        sp_range <- list()
        message("\n   Preparing range values:")
        for (j in 1:length(occlist)) {
            if (M_format %in% c("shp", "gpkg")) {
                if (M_format == "shp") {
                  M <- rgdal::readOGR(dsn = M_folder, layer = mlist[j], 
                    verbose = FALSE)
                } else {
                  M <- rgdal::readOGR(paste0(M_folder, "/", mlist[j]), 
                    verbose = FALSE)
                }
            } else {
                M <- raster::raster(mlist[j])
            }
            occ <- read.csv(occlist[j])
            mvar <- raster::mask(raster::crop(variables[[i]], 
                M), M)
            mval <- na.omit(mvar[])
            medians <- median(mval)
            df_layer <- abs(mval - medians)
            names(df_layer) <- mval
            limit <- floor((100 - percentage_out) * length(df_layer)/100)
            df_layer <- sort(df_layer)[1:limit]
            M_range[[j]] <- range(as.numeric(names(df_layer)))
            occval <- na.omit(raster::extract(mvar, occ[, c(longitude, 
                latitude)]))
            sp_range[[j]] <- range(occval)
            message("\t", j, " of ", length(occlist), " species finished")
        }
        M_range <- do.call(rbind, M_range)
        sp_range <- do.call(rbind, sp_range)
        overall_range <- range(c(c(M_range), c(sp_range)))
        if (overall_range[2] > 999) {
            overall_range <- round(overall_range/10)
            M_range <- round(M_range/10)
            sp_range <- round(sp_range/10)
        }
        if (overall_range[2] > 9999) {
            overall_range <- round(overall_range/100)
            M_range <- round(M_range/100)
            sp_range <- round(sp_range/100)
        }
        o_minimum <- overall_range[1]
        o_minimumc <- ifelse(o_minimum == 0, 0, floor(o_minimum/bin_size) * 
            bin_size) - bin_size
        o_maximum <- overall_range[2]
        o_maximumc <- ifelse(o_maximum == 0, 0, ceiling(o_maximum/bin_size) * 
            bin_size) + bin_size
        overall_range <- c(o_minimumc, o_maximumc)
        message("\n   Preparing bin tables using ranges:")
        bin_table <- bin_env(overall_range, M_range, sp_range, 
            bin_size)
        rownames(bin_table) <- gsub("_", " ", spnames)
        if (save == TRUE) {
            write.csv(bin_table, paste0(output_directory, "/", 
                names(variables)[i], "_bin_table.csv"), row.names = TRUE)
        }
        message(i, " of ", nvars, " variables processed")
        return(bin_table)
    })
    names(bin_tabs) <- names(variables)
    return(bin_tabs)
}

bins=bintables_jc(M_folder=paste0(filepath,"Ms/name_species_tree/"),
                 M_format="gpkg",occ_folder=paste0(filepath,"occ_dir/"),
                 longitude = "Long",latitude = "Lat",
                 percentage_out = 0,bin_size = 10,
                 var_folder=GIS,var_format="GTiff") # must specify GTiff


# Link the tree to the data, and then iteratively perform the reconstructions.


# the formatted tree from above is p.tree3

# create the tree data project
# THIS USES HAND DRAWN
# root the tree
p.tree4=read.tree(paste0(filepath,"by_hand.tre"))
for(j in 1:length(p.tree4$tip.label)){
  p.tree4$tip.label[j]=
    gsub("_"," ",p.tree4$tip.label[j])
}
# create ultrametric tree
p.tree5=chronos(p.tree4)
# make edges nonzero
p.tree4$edge.length=p.tree4$edge.length+
  0.00000000000000000000000001

### loop through and create tree projects

for(i in 1:length(bins)){
  # link tree and data from list object
  var=strsplit(names(bins[i]),"_")[[1]][3]
  varproj=treedata(p.tree4,bins[[i]])
  # create tables for reconstructions
  ml_rec_table=bin_ml_rec(varproj)
  s_ml_rec_table=smooth_rec(ml_rec_table)
  
  plot.phylo(p.tree5,label.offset = 0.03,main=var)
  niche_labels(p.tree5,s_ml_rec_table,
               label_type = 'tip_node',height=1.25, width = 3)
  nichevol_labels(p.tree5, s_ml_rec_table, height = 1.25, width = 3)
  nichevol_legend(position = "bottomleft", cex = 1)
}

