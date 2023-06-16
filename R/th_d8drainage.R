#' Extract drainage network from contributing area
#'
#' @param d8 May be a SpatRaster object or a file path indicating the source of the DEM
#' @param threshold Threshold value
#' @param output Specify the output of the carved DEM
#' @param keep_dem Keep the original DEM in temp directory for later use, if it is not a SpatRaster object
#' @param load_output Load output layer as a SpatRaster object
#'
#' @return A SpatRaster object of the v-shaped carved MDT
#' @export
#'
#' @examples
#' # carved <- th_carvev(srtm1arc, "~/dem_carvev.tif")
th_d8drainage <- function(contributing_area = NULL, threshold, output = NULL, keep_dem = TRUE, load_output = FALSE) {
    if(is.null(pkg.env$temp_working_dir)) cli::cli_abort("Please, run the setup function first!")

    if(!is.numeric(threshold)) cli::cli_abort("Invalid {.var {threshold}} value!")

    contributing_area_file <- check_input(contributing_area, "contributing_area", keep_dem)

    output_file <- paste0(pkg.env$temp_working_dir, "drainage.tif")

    run_th_command(
        "d8drainage",
        contributing_area_file,
        c(output_file, threshold)
    )

    if(!is.null(output)) terra::writeRaster(terra::rast(output_file), output, overwrite = TRUE)

    update_workflow("contributing_area", contributing_area_file)
    update_workflow("drainage", output_file)

    if(load_output) {
        return(terra::rast(output))
    }
}
