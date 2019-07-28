#' compute the D8 flow directions using a digital elevation model (DEM).
#'
#' Take a DEM (typically the depression filled file returned by Pitremove ()),
#' and return a raster files containing the D8 slope and the D8 flow directions.
#' @param dem character path to the raster file, A digital elevation model.
#' @param out_dir character, the path to the directory where the output file is meant to be saved.
#' @param slope_prefix character (optional), the choosen base file name for slope output file.
#' @param fldir_prefix character (optional), the choosen base file name for flow directions output file.
#' @param slope_suffix character (optional), suffix to be added to the slope output file, useful for tracking purpose.
#' @param fldir_suffix character (optional), suffix to be added to the flow directions output file, useful for tracking purpose.
#' @return raster object
#' @details TauDEM version 5.3 has been developed to read any raster supported by GDAL
#' and write grid files in the GeoTiff (.tif) format only. TauDEM reads and writes ESRI
#' shape files and text files generally identified using the .txt extension.
#' @examples
#' # This may take time and requires a connection to internet to download a DEM of size 5.3 MB.
#' destfile <- "C:/Users/LIssoufou/Downloads/LoganDemo.zip"
#' exdir <- "C:/Users/LIssoufou/Downloads"
#' exdir <- paste0 (exdir, "/", "LoganDemo")
#' dem <- paste0 (exdir, "/","Logan", "/", "logan.tif")
#' download.file(url = "http://hydrology.usu.edu/taudem/taudem5/LoganDemo.zip", destfile = destfile)
#' unzip(zipfile = destfile, exdir = exdir)
#' z <- raster(dem)
#' plot(z)
#' out_dir <- paste0(exdir, "/", "R_examples")
#' dir.create(file.path(exdir, 'R_examples'))
#' prefix <- 'logan'
#' suffix <- 'fel'
#' fel <- Pitremove (dem = dem, out_dir = out_dir, prefix = prefix, suffix = suffix)
#' fel <- raster(paste0(out_dir,"/", prefix, suffix, ".tif"))
#' plot(fel)
#' dem <- paste0(out_dir,"/", prefix, suffix, ".tif")
#' D8Flowdir(dem=dem, out_dir = out_dir, slope_prefix = "slope_logan", fldir_prefix = "fldir_logan",
#' slope_suffix = "sd8", fldir_suffix="p")
#' @export
D8Flowdir <- function(dem, out_dir = getwd(), slope_prefix = NULL, fldir_prefix = NULL,
                      slope_suffix = "sd8", fldir_suffix="p"){
  wd0 <- unlist(strsplit(dem, "/"))
  wd <- wd0[-length(wd0)]
  wd <- paste0(wd, collapse = "/")
  setwd(wd)
  dem <- wd0[length(wd0)]
  # mpiexec -n 8 D8Flowdir -p loganp.tif -sd8 logansd8.tif -fel loganfel.tif
  suffix <- as.character(suffix); prefix <- as.character(prefix) # making sure to have characters
  if (is.null(prefix)){
    out_fldir <- paste0(dem, fldir_suffix, ".tif")
    out_slope <- paste0(dem, slope_suffix, ".tif")

  } else {
    out_fldir <- paste0(fldir_prefix, fldir_suffix, ".tif")
    out_slope <- paste0(slope_prefix, slope_suffix, ".tif")
  }
  out_fldir <- paste0(out_dir, "/", out_fldir)
  out_slope <- paste0(out_dir, "/", out_slope)
  cmd <- paste("mpiexec", "-n", "8", "D8Flowdir", "-p", out_fldir, "-sd8", out_slope, "-fel", dem, collapse = " ")
  system(cmd)
}
