# 1a. Climatological rolling seasonal averages - 1981-2022 (n=12 maps/rasters to match ONI rolling three month averages)
# Output names: species_1981-2022_climatology_[season] 
# e.g. swordfish_1981-2022_climatology_DJF

library(terra)
library(tidyverse)
library(glue)
library(raster)
library(maps)
library(ncdf4)

## define dirs
outdir="/Users/EcoCast/Dropbox/EFH/outputs_2023-12-20"
climdir=glue("{outdir}/Climatologies");dir.create(climdir)
spdir="/Volumes/Triple_Bottom_Line/Data/EcoROMS/species_predictions/swor"

## define other objects
species="Swordfish"
sp_list=list.files(spdir,pattern="mean.grd",full.names = T) %>% 
  grep("2023",.,value=T,invert = T)

# DJF -> "-12-|-01-|-02-" 
# JFM -> "-01-|-02-|-03-"  
# FMA -> "-02-|-03-|-04-" 
# MAM -> "-03-|-04-|-05-"  
# AMJ -> "-04-|-05-|-06-"  
# MJJ -> "-05-|-06-|-07-"  
# JJA -> "-06-|-07-|-08-"  
# JAS -> "-07-|-08-|-09-"  
# ASO -> "-08-|-09-|-10-"  
# SON -> "-09-|-10-|-11-"  
# OND -> "-10-|-11-|-12-"  
# NDJ -> "-11-|-12-|-01-"  

seasons=c("-12-|-01-|-02-",
          "-01-|-02-|-03-",
          "-02-|-03-|-04-", 
          "-03-|-04-|-05-", 
          "-04-|-05-|-06-", 
          "-05-|-06-|-07-", 
          "-06-|-07-|-08-",  
          "-07-|-08-|-09-", 
          "-08-|-09-|-10-",  
          "-09-|-10-|-11-", 
          "-10-|-11-|-12-", 
          "-11-|-12-|-01-" 
          )

season_names=c(
  "DJF",
  "JFM",
  "FMA",
  "MAM",  
  "AMJ",  
  "MJJ", 
  "JJA",
  "JAS",  
  "ASO",  
  "SON", 
  "OND", 
  "NDJ" 
)

for(i in 1:length(seasons)){
  print(glue("{season_names[i]} {seasons[i]}"))
  
  ## create climatology
  ras=sp_list %>% grep(seasons[i],.,value=T) 
  ras_stack=rast(c(ras))
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
    ggtitle(glue("{species} 1981-2022 {season_names[i]}"))+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, 'cm'))
  
  png(glue("{climdir}/{species}_1981-2022_climatology_{season_names[i]}.png"),width=11,height=11,units='cm',res=400,type = "cairo")
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
  ncfname <-glue("{climdir}/{species}_1981-2022_climatology_{season_names[i]}.nc")
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


