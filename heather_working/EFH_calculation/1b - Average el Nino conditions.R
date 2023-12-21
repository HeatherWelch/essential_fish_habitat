# 1b. 
# -Average el Nino conditions: from this (1981-2022), average by rolling seasons where index is > 0.5 C (n=12 maps/rasters because ONI index is rolling three month averages)
# Output names: species_1981-2022_elNino_[season] 
# e.g. swordfish_1981-2022_elNino_DJF


library(terra)
library(tidyverse)
library(glue)
library(raster)
library(maps)
library(ncdf4)

## define dirs
outdir="/Users/EcoCast/Dropbox/EFH/outputs_2023-12-20"
climdir=glue("{outdir}/El_nino");dir.create(climdir)
spdir="/Volumes/Triple_Bottom_Line/Data/EcoROMS/species_predictions/swor"

## define other objects
species="Swordfish"
sp_list=list.files(spdir,pattern="mean.grd",full.names = T) %>% 
  grep("2023",.,value=T,invert = T)
oni=read.csv("/Users/EcoCast/Dropbox/EFH/ONI/cleaned_oni.csv")
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
  "DJF", ## in oni when it says e.g. 1990 DFJ it means 1989 D and 1990 FJ
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
  "NDJ" ## in oni when it says e.g. 1990 NDJ it means 1990 ND and 1991 J
)

for(i in 1:length(seasons)){
  print(glue("{season_names[i]} {seasons[i]}"))
  
  ## find which years were el nino in each season
  oni_years=oni %>% filter(season==season_names[i]) %>% 
    filter(anomaly> 0.5) %>% 
    filter(Year>1981) %>% 
    pull(Year)
  
  if(season_names[i]=="DJF"){ ## in oni when it says e.g. 1990 DFJ it means 1989 D and 1990 FJ
    print("season spans multiple years (DJF)")
    prev_years=oni_years-1
    decembers=prev_years %>% map_chr(\(x) glue("{x}-12"))
    januaries=oni_years %>% map_chr(\(x) glue("{x}-01"))
    februaries=oni_years %>% map_chr(\(x) glue("{x}-02"))
    
    targetYears=c(decembers,januaries,februaries) %>% sort() %>% 
      paste0(.,collapse = "|")
    ras=sp_list %>% 
      grep(targetYears,.,value=T)
    
  } else if(season_names[i]=="NDJ"){ ## in oni when it says e.g. 1990 NDJ it means 1990 ND and 1991 J
    print("season spans multiple years (NDJ)")
    next_years=oni_years+1
    novembers=oni_years %>% map_chr(\(x) glue("{x}-11"))
    decembers=oni_years %>% map_chr(\(x) glue("{x}-12"))
    januaries=next_years %>% map_chr(\(x) glue("{x}-01"))
    
    targetYears=c(novembers,decembers,januaries) %>% sort() %>% 
      paste0(.,collapse = "|")
    ras=sp_list %>% 
      grep(targetYears,.,value=T)
    
  } else{
    print("season doesn't span years")
    targetYears=paste0(oni_years,collapse = "|")
    ras=sp_list %>% grep(seasons[i],.,value=T) %>%
      grep(targetYears,.,value=T)
  }

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
    ggtitle(glue("{species} 1981-2022 {season_names[i]} El Nino"))+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, 'cm'))

  png(glue("{climdir}/{species}_1981-2022_elNino_{season_names[i]}.png"),width=11,height=11,units='cm',res=400,type = "cairo")
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
  ncfname <-glue("{climdir}/{species}_1981-2022_elNino_{season_names[i]}.nc")
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


