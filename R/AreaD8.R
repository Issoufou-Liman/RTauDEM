#' compute the D8 flow directions using a digital elevation model (DEM).
#'
#' Take a DEM (typically the depression filled file returned by Pitremove),
#' and return a raster files containing the D8 slope and the D8 flow directions.
#' @inheritParams Pitremove
#' @param fun the name of flow direction function depending on the version TauDEM in use.
#' @param pfile character, path to the input raster file. This is typically the flow direction raster file.
#' submited to pitremove function aprior).
#' @param ad8file character, path to the output slope file to be computed along the way by D8Flowdir.
#' @return raster layer
#' @details TauDEM version 5.3 has been developed to read any raster supported by GDAL
#' and write grid files in the GeoTiff (.tif) format only. TauDEM reads and writes ESRI
#' shape files and text files generally identified using the .txt extension.
#'
#' Calculates a grid of contributing areas using the single direction D8 flow model. The contribution of each
#' grid cell is taken as one (or when the optional weight grid is used, the value from the weight grid).
#' The contributing area for each grid cell is taken as its own contribution plus the contribution from upslope
#' neighbors that drain in to it according to the D8 flow model. If the optional outlet point shapefile is used,
#' only the outlet cells and the cells upslope (by the D8 flow model) of them are in the domain to be evaluated.
#' By default, the tool checks for edge contamination. This is defined as the possibility that a contributing
#' area value may be underestimated due to grid cells outside of the domain not being counted. This occurs when
#' drainage is inwards from the boundaries or areas with no data values for elevation. The algorithm recognizes
#' this and reports "no data" for the contributing area. It is common to see streaks of "no data" values
#' extending inwards from boundaries along flow paths that enter the domain at a boundary. This is the desired
#' effect and indicates that contributing area for these grid cells is unknown due to it being dependent on
#' terrain outside of the domain of data available. Edge contamination checking may be turned off in cases
#' where you know this is not an issue or want to ignore these problems, if for example, the DEM has been
#' clipped along a watershed outline.
#' @importFrom raster extension
#' @export
AreaD8 <- function(fun = "AreaD8", pfile, ad8file=NULL, number_of_processes=8){
  # http://hydrology.usu.edu/taudem/taudem5/help53/D8ContributingArea.html
  # mpiexec -n <number of processes> AreaD8 -p < pfile > -ad8 <ad8file> [ -o <outletfile>] [ -wg < wgfile >] [ -nc ] [ -lyrname < layer name >] [ -lyrno < layer number >]
  in_dem <- paste("-p", pfile)
  out_dem <- check_file(x=ad8file, prefix = "-ad8")
  number_of_processes <- check_number_of_processus (number_of_processes)

  if(out_dem == ""){
    out_dem <- guess_out_file(pfile, prefix="ad8.tif")
    out_dem <- check_file(x=out_dem, prefix = "-ad8")
  }

  cmd <- raster::trim(paste("mpiexec", number_of_processes, fun, in_dem, out_dem))
  system(cmd)
  out <- trim(gsub(pattern = "-ad8", replacement = "", out_dem))
  raster(out)
}
