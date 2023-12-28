# 1b. 
# Average MHW conditions: Aug - Oct (inclusive) across 2014, 2015, 2019, 2020, 2022 (n=1 maps/rasters)
# Output names: species_2014-15-19-20-22_MHW_ASO 
# e.g. swordfish_2014-15-19-20-22_MHW_ASO

library(terra)
library(tidyverse)
library(glue)
library(raster)
library(maps)
library(ncdf4)

## define dirs
outdir="/Users/EcoCast/Dropbox/EFH/outputs_2023-12-20"
climdir=glue("{outdir}/MHW");dir.create(climdir)
spdir="/Volumes/Triple_Bottom_Line/Data/EcoROMS/species_predictions/swor"

## define other objects
species="Swordfish"
sp_list=list.files(spdir,pattern="mean.grd",full.names = T) %>% 
  grep("2014|2015|2019|2020|2022",.,value=T) %>%  ## MHW years
  grep("-08-|-09-|-10-",.,value=T) ## MHW months

ras_stack=rast(c(sp_list))
ras_mean=app(ras_stack,fun=mean,na.rm=T,cores=6)

  ## climatology to dataframe
  df=as.data.frame(ras_mean,xy=T)
  names(df)=c("lon","lat","pred")

  ## write out png
  pred_map=ggplot()+
    geom_tile(data=df,aes(x = lon, y = lat, fill=pred))+
    scale_fill_gradientn("",colours = pals::parula(100),na.value="white")+
    geom_polygon(data = fortify(maps::map("world",plot=FALSE,fill=TRUE)), aes(x=long, y = lat, group=group),color="black",fill="grey")+
    theme_classic()+xlab(NULL)+ylab(NULL)+
    coord_sf(xlim = c(-134, -115.5), ylim = c(30,46),expand=F)+
    ggtitle(glue("{species} MHW"))+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, 'cm'))

  png(glue("{climdir}/{species}_2014-15-19-20-22_MHW_ASO.png"),width=11,height=11,units='cm',res=400,type = "cairo")
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
  ncfname <-glue("{climdir}/{species}_2014-15-19-20-22_MHW_ASO.nc")
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

  


