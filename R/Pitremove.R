#' remove pits by filling depressions in a digital elevation model (DEM).
#'
#' Take a DEM, and a return a file comprising the DEM with pits removed.
#' @param dem character path to the raster file, A digital elevation model.
#' @param out_dir character, the path to the directory where the output file is meant to be saved.
#' @param prefix character (optional), the choosen base file name.
#' @param suffix character (optional), suffix to be added to the output file, useful for tracking
#' purpose. default is "fel" as by TauDEM file naming convention.
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
#' @export
Pitremove <- function(dem, out_dir = getwd(), prefix = NULL, suffix = "fel"){
  wd0 <- unlist(strsplit(dem, "/"))
  wd <- wd0[-length(wd0)]
  wd <- paste0(wd, collapse = "/")
  setwd(wd)
  dem <- wd0[length(wd0)]
  # system("mpiexec -n 8 pitremove -z logan.tif -fel loganfel.tif")
  suffix <- as.character(suffix); prefix <- as.character(prefix) # making sure to have characters
  if (is.null(prefix)){
    out <- paste0(dem, suffix, ".tif")
  } else {
    out <- paste0(prefix, suffix, ".tif")
  }
  out <- paste0(out_dir, "/", out)
  cmd <- paste("mpiexec", "-n", "8", "pitremove", "-z", dem, "-fel", out, collapse = " ")
  system(cmd)
}
