#' Remove pits using PFS algorithm
#'
#' @param d8 D8 flow directions grid. May be a SpatRaster object or a file path indicating the source of the raster.
#' @param drainage D8 drainage network grid. May be a SpatRaster object or a file path indicating the source of the raster
#' @param contrib Contributing area raster. May be a SpatRaster object or a file path indicating the source of the raster
#' @param output A text file indicating river mouths
#' @param keep_input Keep inputs
#' @param load_output Load outputs
#'
#' @return A SpatRaster object of the pitless DEM
#' @export
#'
#' @examples
th_mouths <- function(d8 = NULL, drainage = NULL, contrib = NULL, output, keep_input = TRUE, load_output = FALSE) {
    if(is.null(pkg.env$temp_working_dir)) cli::cli_abort("Please, run the setup function first!")

    d8_file <- check_input(d8, "D8", keep_input)
    drainage_file <- check_input(drainage, "D8", keep_input)
    contrib_file <- check_input(contrib, "D8", keep_input)

    mouths_file <- paste0(pkg.env$temp_working_dir, "mouths.txt")
    output_file <- paste0(pkg.env$temp_working_dir, "ordered_mouths.txt")
    otto_mouths_file <- paste0(pkg.env$temp_working_dir, "otto_mouths.txt")

    ref_raster <- terra::rast(drainage_file)

    run_th_command(
        "mouths",
        c(d8_file, drainage_file),
        mouths_file
    )

    run_th_command(
        "orderedmouths",
        c(contrib_file, mouths_file),
        output_file
    )

    mouths_t <- utils::read.table(output_file, sep = "\t", col.names = c("row", "column"))

    otto_n_max_digits <- ceiling(nrow(mouths_t) / 9)

    otto_codes <- ""
    for(i in 1:otto_n_max_digits) {
        otto_grid <- expand.grid(otto_codes, 1:9)
        otto_codes <- paste0(otto_grid$Var1, otto_grid$Var2)
    }

    mouths_t["otto_radical"] <- otto_codes[1:nrow(mouths_t)]

    otto_mouths_removed <- suppressWarnings(file.remove(otto_mouths_file))
    for(i in seq_along(mouths_t$otto_radical)) {
        write(mouths_t[i, "otto_radical"], otto_mouths_file, append = TRUE)
        write(paste(mouths_t[i, "row"], mouths_t[i, "column"], sep = "\t"), otto_mouths_file, append = TRUE)
    }

    mouths_xy <- as.data.frame(
        terra::xyFromCell(
            ref_raster,
            terra::cellFromRowCol(
                ref_raster,
                mouths_t$row + 1,
                mouths_t$column + 1
            )
        )
    )

    mouths_xy["otto_radical"] <- mouths_t$otto_radical
    mouths_xy["row"] <- mouths_t$row
    mouths_xy["column"] <- mouths_t$column

    mouths_points <- sf::st_as_sf(mouths_xy, coords = c("x", "y"), crs = terra::crs(terra::rast(ref_raster)))
    mouths_points_file <- paste0(pkg.env$temp_working_dir, "mouths_points.gpkg")

    sf::write_sf(mouths_points, mouths_points_file)

    update_workflow("mouths", mouths_file)
    update_workflow("ordered_mouths", output_file)
    update_workflow("otto_mouths", otto_mouths_file)
    update_workflow("mouths_points_file", mouths_points)

    if(load_output) return(mouths_points)
}
