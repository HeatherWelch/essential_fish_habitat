# 1c. 
# Average 1981-2022 reanalysis, average 1981-2022 GFDL projection, calculate anomaly (reanalysis - projection) (n=3 maps/rasters: reanalysis, projection, anomaly)
# Output names: species_1981-2022_[reanalysis/projectionGFDL/anomalyGFDL] 
# e.g. swordfish_1981-2022_projectionGFDL


library(terra)
library(tidyverse)
library(glue)
library(raster)
library(maps)
library(ncdf4)

## define dirs
outdir="/Users/EcoCast/Dropbox/EFH/outputs_2023-12-20"
climdir=glue("{outdir}/proj_reanyl");dir.create(climdir)
climdir_intermediate=glue("{outdir}/proj_reanyl/intermediate");dir.create(climdir_intermediate) ## 30 year averages take too long, doing monthly clim first and then averaging
spdir="/Volumes/Triple_Bottom_Line/Data/EcoROMS/species_predictions/swor"
projdir="/Volumes/Triple_Bottom_Line/Nerea_working/HMS_projections/Results/Plotmaps_noSSH2/GFDL_swor"

## define other objects
yrs=seq(1981,2022,by=1)
yrs_string=paste(yrs,collapse = "|")
species="Swordfish"

sp_list_reanalysis=list.files(spdir,pattern="mean.grd",full.names = T) %>% 
  grep(yrs_string,.,value=T) ## target years
sp_list_proj=list.files(projdir,pattern="mean.grd",full.names = T) %>% 
  grep(yrs_string,.,value=T) ## target years

## monthly climatologies ####
months=c("-01-",
         "-02-",
         "-03-",
         "-04-",
         "-05-",
         "-06-",
         "-07-",
         "-08-",
         "-09-",
         "-10-",
         "-11-",
         "-12-"
         )

for(i in 1:length(months)){
  print(months[i])
  
  ## reanalysis
  sp_list_reanalysis_m=sp_list_reanalysis %>% 
    grep(months[i],.,value=T)
  ras_stack_renalysis_m=rast(c(sp_list_reanalysis_m))
  ras_mean_renalysis_m=app(ras_stack_renalysis_m,fun=mean,na.rm=T,cores=6)
  writeRaster(ras_mean_renalysis_m,glue("{climdir_intermediate}/{species}{months[i]}reanalysis.grd"),overwrite=TRUE)
  
  ## projections
  sp_list_proj_m=sp_list_proj %>% 
    grep(months[i],.,value=T)
  ras_stack_proj_m=rast(c(sp_list_proj_m))
  ras_mean_proj_m=app(ras_stack_proj_m,fun=mean,na.rm=T,cores=6)
  writeRaster(ras_mean_proj_m,glue("{climdir_intermediate}/{species}{months[i]}projection.grd"),overwrite=TRUE)
  
}

## 30 year climatologies + anomalies ####
reanlysis_stack=list.files(climdir_intermediate,pattern="reanalysis.grd$",full.names = T) %>% 
  rast(c(.))
projection_stack=list.files(climdir_intermediate,pattern="projection.grd$",full.names = T) %>% 
  rast(c(.))

reanlysis_mean=app(reanlysis_stack,fun=mean,na.rm=T,cores=6)
projection_mean=app(projection_stack,fun=mean,na.rm=T,cores=6)
anomaly=reanlysis_mean-projection_mean

master=c(reanlysis_mean,projection_mean,anomaly)
names=c("reanalysis","projectionGFDL","anomalyGFDL")

for(i in 1:nlyr(master)){
  
  ## climatology to dataframe
  df=as.data.frame(master[[i]],xy=T)
  names(df)=c("lon","lat","pred")
  
  if(names[i]=="anomalyGFDL") { ## divergent color scale for anomalies
  ## write out png
  pred_map=ggplot()+
    geom_tile(data=df,aes(x = lon, y = lat, fill=pred))+
    scale_fill_gradient2("")+
    geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
    theme_classic()+xlab(NULL)+ylab(NULL)+
    coord_sf(xlim = c(-134, -115.5), ylim = c(30,46),expand=F)+
    ggtitle(glue("{species} 1981-2022 {names[i]}"))+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, 'cm'))
  } else {

  ## write out png
  pred_map=ggplot()+
    geom_tile(data=df,aes(x = lon, y = lat, fill=pred))+
    scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
    geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
    theme_classic()+xlab(NULL)+ylab(NULL)+
    coord_sf(xlim = c(-134, -115.5), ylim = c(30,46),expand=F)+
    ggtitle(glue("{species} 1981-2022 {names[i]}"))+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, 'cm'))
  }

  png(glue("{climdir}/{species}_1981-2022_{names[i]}.png"),width=11,height=11,units='cm',res=400,type = "cairo")
  par(ps=10)
  par(mar=c(0,0,0,0))
  par(cex=1)
  print({pred_map})
  # gg_hm
  dev.off()

  ## turn into netcdf
  # Set dimensions
  lons <- unique(df$lon)
  lats <- unique(df$lat)

  toShapeW <- pivot_wider(df, id_cols = c("lon"), values_from = "pred", names_from = "lat")
  toShapeW$lon <- NULL
  suppressWarnings(dimnames(toShapeW) <- list(lon = lons, lat = lats))
  outArray <- array(unlist(toShapeW), dim(toShapeW), dimnames = list(lon = lons, lat = lats)) # unlist is essential

  # Now save as new netcdf
  ncfname <-glue("{climdir}/{species}_1981-2022_{names[i]}.nc")
  # Then define dimensions and variables
  londim <- ncdim_def("lon", "degrees_west", as.double(lons))
  latdim <- ncdim_def("lat", "degrees_north", as.double(lats))
  fillvalue <- 1e32

  # Variable names
  dlname <- "predicted probability of species occurrence from brt" # or whatever
  my_def <- ncvar_def("pred", "/1", list(londim, latdim), fillvalue, dlname, prec = "single")
  # Now create the file and put variables into it
  ncoutSDM <- nc_create(ncfname, list(my_def), force_v4 = TRUE)
  ncvar_put(ncoutSDM, my_def, outArray)
  ncatt_put(ncoutSDM, "lon", "axis", "X")
  ncatt_put(ncoutSDM, "lat", "axis", "Y")

  ncoutSDM # Get a summary of the created file if you want, then close it
  nc_close(ncoutSDM)

}
  


