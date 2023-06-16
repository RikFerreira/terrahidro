#' Initial setup to terrahidro package
#'
#' @export
pkg.env <- new.env()

th_setup <- function() {
    pkg.env$temp_working_dir <- paste0(tempdir(), "/terrahidro/")

    if(!dir.exists(pkg.env$temp_working_dir)) {
        dir.create(pkg.env$temp_working_dir)

        cli::cli_inform(c(
                "v" = "Temporary working directory created!",
                "i" = "Directory: {pkg.env$temp_working_dir}"
        ))
    } else {
        cli::cli_inform(c(
            "!" = "Temporary working directory already exists!",
            "i" = "Directory: {pkg.env$temp_working_dir}"
        ))
    }

    pkg.env$workflow <- data.frame(
        layer = c(
            "dem",
            "dem_carvev", "dem_simplepits", "dem_pfs",
            "carve",
            "mouths", "ordered_mouths", "otto_mouths", "mouths_points",
            "d8", "drainage", "contributing_area"
        ),
        path = c("")
    )
}
