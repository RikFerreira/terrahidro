#' Remove pits using PFS algorithm
#'
#' @param dem May be a SpatRaster object or a file path indicating the source of the DEM
#' @param output Specify the output of the pitless DEM
#' @param keep_input Keep the original DEM in temp directory for later use, if it is not a SpatRaster object
#' @param load_output Load output layer as a SpatRaster object
#'
#' @return A SpatRaster object of the pitless DEM
#' @export
#'
#' @examples
th_pfs <- function(dem = NULL, output = NULL, keep_input = TRUE, load_output = FALSE) {
   if(is.null(pkg.env$temp_working_dir)) cli::cli_abort("Please, run the setup function first!")

    dem_file <- check_input(dem, "DEM", keep_input)

    output_file <- paste0(pkg.env$temp_working_dir, "dtm_pfs.tif")

    run_th_command(
        "pfs",
        dem_file,
        output_file
    )

    if(!is.null(output)) terra::writeRaster(terra::rast(output_file), output, overwrite = TRUE)

    update_workflow("input_dem", dem_file)
    update_workflow("dem_pfs", output_file)

    if(load_output) {
        return(terra::rast(output))
    }
}
