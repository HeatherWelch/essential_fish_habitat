#######################################################################################################
######################################### Export dfs as netcdfs #######################################
#######################################################################################################

library(ncdf4)
library(abind)
library(tidyr)
library(ggplot2)

# Make a test df for June 2015. Ordered by lon, lat, time is easiest
df <- expand.grid("lon" = c(-134:-110), "lat" = c(30: 48), 
                  "dt" = seq.Date(from = as.Date("2015-06-01"), to = as.Date("2015-06-30"), by = "days"))
# Random value as predictive output
df$pred <- rnorm(nrow(df))

# Map
ggplot(df) + geom_tile(aes(x = lon, y = lat, fill = pred))

# Convert date to something that exports easier, say days since 1-1-1900
df$time <- difftime(df$dt, as.Date("1900-01-01"), units = "days")
# Set dimensions
lons <- unique(df$lon)
lats <- unique(df$lat)
times <- unique(df$time)

# Reshape long-format 2D df to get 3D arrays: I save separate ncs for each SDM, species, year, and month
suppressWarnings(rm(outArray))
for (k in 1:length(times)) {
  toShape <- subset(df, df$time == times[k])
  toShape <- toShape[c("lon", "lat", "pred")]
  toShapeW <- pivot_wider(toShape, id_cols = c("lon"), values_from = "pred", names_from = "lat")
  toShapeW$lon <- NULL
  suppressWarnings(dimnames(toShapeW) <- list(lon = lons, lat = lats))
  if(exists("outArray")) {
    outArray <- abind(outArray, toShapeW, along = 3)
   } else {
      outArray <- toShapeW
   }
 }
dimnames(outArray)[[3]] <- times

# Check dimensions
dim(outArray)

# Now save as new netcdf
ncfname <- "test.nc"
# Then define dimensions and variables
londim <- ncdim_def("lon", "degrees_west", as.double(lons)) 
latdim <- ncdim_def("lat", "degrees_north", as.double(lats))
timdim <- ncdim_def("time", "days since 1-1-1900", as.double(times))
fillvalue <- 1e32
# Variable names
dlname <- "predicted probability of species occurrence from GAM" # or whatever
my_def <- ncvar_def("pred", "/1", list(londim, latdim, timdim), fillvalue, dlname, prec = "single")
# Now create the file and put variables into it
ncoutSDM <- nc_create(ncfname, list(my_def), force_v4 = TRUE)
ncvar_put(ncoutSDM, my_def, outArray)
ncatt_put(ncoutSDM, "lon", "axis", "X")
ncatt_put(ncoutSDM, "lat", "axis", "Y")
ncatt_put(ncoutSDM, "time","axis", "T")
ncoutSDM # Get a summary of the created file if you want, then close it
nc_close(ncoutSDM)

# Re-open to check if you like
test <- nc_open("test.nc")
print(test)
lons2 <- ncvar_get(test, "lon")
lats2 <- ncvar_get(test, "lat")
tim <- ncvar_get(test, "time")
preds <- ncvar_get(test, "pred") 
dimnames(preds) <- list(lon = lons2, lat = lats2, time = tim)
df2 <- reshape2::melt(preds, value.name = "pred")
# Map, so can see is the same
ggplot(df2) + geom_tile(aes(x = lon, y = lat, fill = pred))
nc_close(test)