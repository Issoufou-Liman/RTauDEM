#' compute the D8 flow directions using a digital elevation model (DEM).
#'
#' Take a DEM (typically the depression filled file returned by Pitremove),
#' and return a raster files containing the D8 slope and the D8 flow directions.
#' @inheritParams Pitremove
#' @param fun the name of flow direction function depending on the version TauDEM in use.
#' @param felfile character, path to the input raster file, A digital elevation model (Must be TIFF File and typical
#' submited to pitremove function aprior).
#' @param sd8file character, path to the output slope file to be computed along the way by D8Flowdir.
#' @param pfile character, path to the output flow direction file to be computed as end product.
#' @return raster layer
#' @details TauDEM version 5.3 has been developed to read any raster supported by GDAL
#' and write grid files in the GeoTiff (.tif) format only. TauDEM reads and writes ESRI
#' shape files and text files generally identified using the .txt extension.
#'
#' D8Flowdir creates 2 grids. The first contains the flow direction from each grid cell to one of its adjacent
#' or diagonal neighbors, calculated using the direction of steepest descent. The second contain the slope, as
#' evaluated in the direction of steepest descent, and is reported as drop/distance, i.e. tan of the angle.
#' Flow direction is reported as "no data" for any grid cell adjacent to the edge of the DEM domain, or adjacent
#' to a "no data" value in the DEM. In flat areas, flow directions are assigned away from higher ground and
#' towards lower ground using the method of Garbrecht and Martz (1997). The D8 flow direction algorithm may be
#' applied to a DEM that has not had its pits filled, but it will then result in "no data" values for flow
#' direction and slope at the lowest point of each pit.
#' @importFrom raster extension
#' @export
D8Flowdir <- function(fun = "D8Flowdir", felfile, sd8file = NULL, pfile = NULL, number_of_processes=8){
  # http://hydrology.usu.edu/taudem/taudem5/help53/D8FlowDirections.html
  # mpiexec -n <number of processes> D8FlowDir -fel < felfile > -p < pfile> -sd8 < sd8file>

  in_dem <- paste("-fel", felfile)
  out_dem <- check_file(x=pfile, prefix = "-p")
  out_slope <- check_file(x=sd8file, prefix = "-sd8")
  number_of_processes <- check_number_of_processus (number_of_processes)

  if(out_dem == ""){
    out_dem <- guess_out_file(felfile, prefix="p.tif")
    out_dem <- check_file(x=out_dem, prefix = "-p")
  }
  if(out_slope == ""){
    out_slope <- guess_out_file(felfile, prefix="sd8.tif")
    out_slope <- check_file(x=out_slope, prefix = "-sd8")
  }
  cmd <- raster::trim(paste("mpiexec", number_of_processes, fun, in_dem, out_dem, out_slope))
  system(cmd)
  out <- trim(gsub(pattern = "-p", replacement = "", out_dem))
  raster(out)
}
