#' Carve channels on a DEM in a v-shaped form
#'
#' @param dem May be a SpatRaster object or a file path indicating the source of the DEM
#' @param output Specify the output of the carved DEM
#' @param keep_dem Keep the original DEM in temp directory for later use, if it is not a SpatRaster object
#' @param load_output Load output layer as a SpatRaster object
#'
#' @return A SpatRaster object of the v-shaped carved MDT
#' @export
#'
#' @examples
#' # carved <- th_carvev(srtm1arc, "~/dem_carvev.tif")
th_carve <- function(dem = NULL, output = NULL, keep_dem = TRUE, load_output = FALSE) {
    if(is.null(pkg.env$temp_working_dir)) cli::cli_abort("Please, run the setup function first!")

    if(grepl("\\.tif$|\\.tiff$", dem)) { # If DEM is a TIFF file
        if(!file.exists(dem)) cli::cli_abort("File {dem} did not found!")

        if(keep_dem) {
            cli::cli_inform("Storing original DEM for later use...")
            temp_dem <- terra::rast(dem)

            dem_file <- paste0(pkg.env$temp_working_dir, "input_dem.tif")
            terra::writeRaster(temp_dem, dem_file, overwrite = TRUE)
        } else {
            dem_file <- dem
        }
    } else if(methods::is(dem, "SpatRaster")) { # If DEM is a terra::SpatRaster file
        if(!keep_dem) cli::cli_warn("keep_dem = FALSE option ignored!")

        cli::cli_inform("Storing original DEM for later use...")

        dem_file <- paste0(pkg.env$temp_working_dir, "input_dem.tif")
        terra::writeRaster(dem, dem_file, overwrite = TRUE)
    } else if(is.null(dem)) { # If DEM should be recovered from workflow
        if(!keep_dem) cli::cli_warn("keep_dem = FALSE option ignored!")

        cli::cli_inform("Recovering DEM from workflow...")

        dem_file <- pkg.env$workflow[which(pkg.env$workflow$layer == "keep_dem"), "path"]
    } else { # Invalid DEM
        cli::cli_abort("Either the DEM is not a TIFF file or it isn't a {.cls {class(terra::rast())}} object")
    }

    output_file <- paste0(pkg.env$temp_working_dir, "dtm_carvedv.tif")

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
