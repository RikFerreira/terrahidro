#' Carve channels on a DEM in a v-shaped form
#'
#' @param dem May be a SpatRaster object or a file path indicating the source of the DEM
#' @param output Specify the output of the carved DEM
#' @param keep_input Keep the original DEM in temp directory for later use, if it is not a SpatRaster object
#' @param load_output Load output layer as a SpatRaster object
#'
#' @return A SpatRaster object of the v-shaped carved MDT
#' @export
#'
#' @examples
#' # carved <- th_carvev(srtm1arc, "~/dem_carvev.tif")
th_carvev <- function(dem = NULL, output = NULL, keep_input = TRUE, load_output = FALSE) {
    if(is.null(pkg.env$temp_working_dir)) cli::cli_abort("Please, run the setup function first!")

    dem_file <- check_input(dem, "DEM", keep_input)

    output_file <- paste0(pkg.env$temp_working_dir, "dtm_carvedv.tif")

    run_th_command(
        "carvev",
        dem_file,
        output_file
    )

    if(!is.null(output)) terra::writeRaster(terra::rast(output_file), output, overwrite = TRUE)

    update_workflow("input_dem", dem_file)
    update_workflow("dem_carvev", output_file)

    if(load_output) {
        return(terra::rast(output))
    }
}
