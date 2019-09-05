#' remove pits by filling depressions in a digital elevation model (DEM).
#'
#' Take a DEM, and a return a file comprising the DEM with pits removed.
#' @param fun the name of pit removal function depending on the version TauDEM in use.
#' @param demfile character, path to the raster file, A digital elevation model (Must be TIFF File).
#' @param felfile character, path to the output file.
#' @param number_of_processes, number of MPI processes to be used. The default is 8.This is the
#' number of stripes that the domain will be divided into and the number of MPI parallel processes
#' that will be spawned to evaluate each of the stripes.
#' @param four_way_only, boolean. If this option is selected Fill ensures that the grid is hydrologically
#' conditioned with cell to cell connectivity in only 4 directions (N, S, E or W neighbors).
#' Each grid cell is conditioned to drain to one of these adjacent, but not diagonal neighbors.
#' @param depmaskfile Depression mask indicator grid (depmask) to identify cells that are real pits
#' (or sinks or depressions) and should not be filled. Grid cells with a value of 1 (depmask=1) are not filled.
#' Grid cells with any other value are filled.
#' @return raster object
#' @details TauDEM version 5.3 has been developed to read any raster supported by GDAL
#' and write grid files in the GeoTiff (.tif) format only. TauDEM reads and writes ESRI
#' shape files and text files generally identified using the .txt extension.
#'
#' This funciton identifies all pits in the DEM and raises their elevation to the level of the
#' lowest pour point around their edge. Pits, also referred to as depressions or sinks, are low
#' elevation areas in digital elevation models (DEMs) that are completely surrounded by higher
#' terrain. They are generally taken to be artifacts that interfere with the routing of flow
#' across DEMs, so are removed by raising their elevationt, or filling them, to the point
#' where they drain off the edge of the domain. The pour point is the lowest point on
#' the boundary of the "watershed" draining to the pit.If there are specific pits that you do
#' not wish to remove, but do want to remove other pits, pits you want to retain can be marked
#' using the optional depression mask input file (dempask). Elevation values from the input DEM
#' corresponding to grid cells with depmask=1 will not be changed and if these mask cells are at
#' the low points of internally draining depressions, the depression will be retained. "No data"
#' values serve to define edges in the domain, and elevations are only raised to where flow is off
#' an edge or to a grid cell with "no data". An internal "no data" value can placed at the low point
#' of internally draining depressions to stop it being raised by pit filling as an alternative to using
#' a depression mask. This pit removal step is not essential if you have reason to believe that
#' all the pits in your DEM are real. This step can be circumvented by using the raw DEM as input
#' to other TauDEM functions in the place where a filled DEM is used, such as by copying the raw DEM
#' source data onto the file with suffix "fel" to simulate the output of "Pit Remove" without actually
#' removing the pits.
#' @examples
#' \dontrun{
#' # This may take time and requires a connection to internet to download a DEM of size 5.3 MB.
#' destfile <- "C:/Users/LIssoufou/Downloads/LoganDemo.zip"
#' exdir <- "C:/Users/LIssoufou/Downloads"
#' exdir <- paste0 (exdir, "/", "LoganDemo")
#' dem <- paste0 (exdir, "/","Logan", "/", "logan.tif")
#' download.file(url = "http://hydrology.usu.edu/taudem/taudem5/LoganDemo.zip", destfile = destfile)
#' unzip(zipfile = destfile, exdir = exdir)
#' library(rgdal)
#' library(raster)
#' z <- raster(dem)
#' plot(z)
#' out_dir <- paste0(exdir, "/", "R_examples")
#' dir.create(file.path(exdir, 'R_examples'))
#' out_dir <- paste0(out_dir, "/", "logan_pitremoved.tif")
#' fel <- Pitremove (demfile = dem, felfile = out_dir)
#' fel <- raster(paste0(out_dir,"/", prefix, suffix, ".tif"))
#' plot(fel)
#' }
#' @importFrom raster raster trim extension
#' @references
#' http://hydrology.usu.edu/taudem/taudem5/help53/PitRemove.html
#' http://hydrology.usu.edu/taudem/taudem5/help/PitRemove.html
#' http://hydrology.usu.edu/taudem/taudem5/TauDEM51CommandLineGuide.pdf
#' @export
Pitremove <- function(fun = "PitRemove", demfile, felfile = NULL, number_of_processes=8, four_way_only = FALSE, depmaskfile = NULL){
  # http://hydrology.usu.edu/taudem/taudem5/help53/PitRemove.html
  # mpiexec -n <number of processes> PitRemove -z <demfile> -fel < felfile> [ -4way ] [ -depmask depmaskfile]
  four_way_only <- ifelse (four_way_only, "-4way", "")

  in_dem <- paste("-z", demfile)
  # in_dem <- check_file(x=demfile, prefix = "-z")
  out_dem <- check_file(x=felfile, prefix = "-fel")
  depmaskfile <- check_file(x=depmaskfile, prefix = "-depmask")
  number_of_processes <- check_number_of_processus (number_of_processes)

  if(out_dem == ""){
    out_dem <- guess_out_file(demfile, prefix = "fel.tif")
    out_dem <- check_file(x=out_dem, prefix = "-fel")
  }
  cmd <- raster::trim(paste("mpiexec", number_of_processes, fun, in_dem, out_dem, four_way_only, depmaskfile))
  system(cmd)
  out <- trim(gsub(pattern = "-fel", replacement = "", out_dem))
  raster(out)
}
