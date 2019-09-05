check_file <- function(x, prefix){
  if (!is.null(x) & !is.character(x)){
    stop(paste (deparse(substitute(x)), " argument must be character"))
  }
  if (is.null(x)){
    out_dem <- ""
  } else {
    if (x == ""){
      out_dem <- ""
    } else {
      if ((raster::extension(x) != ".tif") & (raster::extension(x) != "")){
        stop(paste(deparse(substitute(x)), "must have .tif extension"))
      } else if(raster::extension(x) == ""){
        x <- paste0(x, ".tif")
      }
      out_dem <- paste(prefix, x)
    }
  }
  return (out_dem)
}

check_number_of_processus <- function(number_of_processes){
  if (!is.numeric(number_of_processes)){
    stop("number_of_processes argument must be numeric")
  } else if (number_of_processes%%1 != 0){
    number_of_processes <- as.integer(number_of_processes)
    warning("number_of_processes is converted to interger")
  }
  number_of_processes <- paste("-n", number_of_processes)
  return (number_of_processes)
}

guess_out_file <- function(x, prefix){
  x <- unlist(strsplit(x, "/"))
  out_file <- gsub(pattern = ".tif", replacement = prefix, x[length(x)])
  x <- paste0(x[-length(x)], collapse = '/')
  paste0(x, "/", out_file)
}
