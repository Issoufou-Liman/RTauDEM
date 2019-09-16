#' Download digital elevation model (DEM) by shapefile
#'
#' Takes a shapefile and download SRTM DEM covering the shapefile
#' @param shp SpatialPolygonsDataFrame shapefile from the sp package
#' @inheritParams raster::getData
#' @inheritParams raster::mosaic
#' @inheritDotParams raster::mosaic
#' @return a rater layer or a list of raster layers (if the shapefile covers several tiles).
#' @example
#' \dontrun{
#' kenya <- getData('GADM', country='KEN', level=1)
#' kisumu <- kenya[kenya$NAME_1 == 'Kisumu', ]
#' kisumu_dem <- getDEM(kisumu)
#' kisumu_dem$fun <- 'mean'
#' kisumu_dem <- do.call(mosaic, kisumu_dem)
#' plot(kisumu_dem)
#' plot(kisumu, add=T)
#' }
#' @importFrom raster rasterToPoints crop getData extent ncell
#' @importFrom sp proj4string<-
#' @importFrom methods as
#' @export
getDEM <- function(shp, path='', fun, filename, ...){
  tile_spy <- extent(-180, 180, -60, 60)
  tile_spy <- as (tile_spy, 'SpatialPolygons')
  proj4string(tile_spy) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  tile_spy <- raster(tile_spy, resolution = 5)
  tile_spy[] <- 1:ncell(tile_spy)
  tile_spy <- crop(tile_spy, shp, snap='out')
  tile_spy <- rasterToPoints(tile_spy)
  if(path == ''){
    path = getwd()
  }
  if (nrow(tile_spy) == 1){
    lon <- as.numeric(tile_spy[1])
    lat <- as.numeric(tile_spy[2])
    out <- getData('SRTM', lon=lon, lat=lat, path = path)
  } else if(nrow(tile_spy) > 1) {
    out <- sapply(X = 1:nrow(tile_spy), function(i) {
      lon <- as.numeric(tile_spy[i, ][1])
      lat <- as.numeric(tile_spy[i, ][2])
      getData('SRTM', lon=lon, lat=lat, path = path)
    }, simplify = FALSE)
  } else {
    stop("invalid coordinates! Please check the shapefile projection and/or the coverage of SRTM data")
  }
  out$fun <- fun
  do.call('mosaic', out, filename, ...)
}
