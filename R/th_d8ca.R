#' Remove pits from DEM
#'
#' @param d8 May be a SpatRaster object or a file path indicating the source of the DEM
#' @param output Specify the output of the carved DEM
#' @param keep_dem Keep the original DEM in temp directory for later use, if it is not a SpatRaster object
#' @param load_output Load output layer as a SpatRaster object
#'
#' @return A SpatRaster object of the v-shaped carved MDT
#' @export
#'
#' @examples
#' # carved <- th_carvev(srtm1arc, "~/dem_carvev.tif")
th_d8ca <- function(d8 = NULL, output = NULL, keep_dem = TRUE, load_output = FALSE) {
    if(is.null(pkg.env$temp_working_dir)) cli::cli_abort("Please, run the setup function first!")

    d8_file <- check_input(d8_file, "d8", keep_dem)

    output_file <- paste0(pkg.env$temp_working_dir, "contributing_area.tif")

    run_th_command(
        "d8ca",
        d8_file,
        output_file
    )

    if(!is.null(output)) terra::writeRaster(terra::rast(output_file), output, overwrite = TRUE)

    update_workflow("d8", d8_file)
    update_workflow("contributing_area", output_file)

    if(load_output) {
        return(terra::rast(output))
    }
}
