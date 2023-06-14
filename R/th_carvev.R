#' Carve channels on a MDT in a v-shaped form
#'
#' @param dem May be a SpatRaster object or a file path indicating the source of the MDT
#' @param output Specify the output of the carved MDT
#' @param keep_mdt Keep the original MDT in temp directory for later use
#'
#' @return A SpatRaster object of the v-shaped carved MDT
#' @export
#'
#' @examples
#' # carved <- th_carvev(srtm1arc, "~/dem_carvev.tif")
th_carvev <- function(dem, output, keep_mdt = TRUE) {
    if(keep_mdt) message("Storing original MDT for later use...")
    temp_working_dir <- getwd()
    if(grepl("\\.tif$|\\.tiff$", dem)) {
        dem_file <- dem
    } else {
        dem_file <- paste0(temp_working_dir, "/input_dem.tif")
        terra::writeRaster(dem, dem_file, overwrite = TRUE)
    }

    args <- ""
    args <- paste(args, "carvev")
    args <- paste(args, dem_file, output)

    command <- paste("th", args)

    print(command)

    system(command)

    terra::rast(paste0(temp_working_dir, "/", output))
}

?message
