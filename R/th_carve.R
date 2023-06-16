#' Carve channels on a DEM in a v-shaped form
#'
#' @param dem May be a SpatRaster object or a file path indicating the source of the DEM
#' @param drainage May be a SpatRaster object or a file path indicating the source of the DEM
#' @param flat_areas May be a SpatRaster object or a file path indicating the source of the DEM
#' @param use_drainage May be a SpatRaster object or a file path indicating the source of the DEM
#' @param nb May be a SpatRaster object or a file path indicating the source of the DEM
#' @param output Specify the output of the carved DEM
#' @param keep_input Keep the original DEM in temp directory for later use, if it is not a SpatRaster object
#' @param load_output Load output layer as a SpatRaster object
#'
#' @return A SpatRaster object of the v-shaped carved MDT
#' @export
#'
#' @examples
#' # carved <- th_carvev(srtm1arc, "~/dem_carvev.tif")
th_carve <- function(
        dem = NULL,
        drainage = NULL,
        flat_areas = NULL,
        use_drainage = TRUE,
        nb = c("rook", "bishop", "queen"),
        output = NULL,
        keep_input = TRUE,
        load_output = FALSE
    ) {
    if(is.null(pkg.env$temp_working_dir)) cli::cli_abort("Please, run the setup function first!")

    dem_file <- check_input(dem, "DEM", keep_input)
    drainage_file <- check_input(drainage, "drainage", keep_input)
    flat_areas_file <- check_input(drainage, "flat_areas", keep_input)

    output_file <- paste0(pkg.env$temp_working_dir, "carve.tif")

    run_th_command(
        "carve",
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
