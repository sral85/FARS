# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#' Reads the data in a csv-file given by filename into an data.frame.
#'
#' @param x A filename as a character. The corresponding file will be read.
#' @return An object of the type tbl_df. If the file does not exist, an error will occur.
#' @examples
#' fars_read(make_file(2015))
#' fars_read("accident_2015.csv.bz2")
#' @export
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Creates the filename of the fars-data for a given year
#'
#' @param year A year as an integer value. If \code{year} is not in this form, it will be coerced to integer.
#' @return The function returns the filename of the data from the FARS system of the corresponding \code{year} .
#' @examples
#' make_filename(2015)
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads the month and year columns of multiple fars-data files and returns it as a list of dataframes.
#'
#' @param years A vector of integer values.
#' @return A list of data.frames (tbl_df) containing the month and year columns.
#' @examples
#' fars_read_years(c(2013,2014))
#' add(10, 1)
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr select
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' Summarize fars data.
#'
#' @param years A vector of integer values.
#' @return A data.frame giving the counts of the present elements for each month by year.
#' @examples
#'fars_summarize_years(c(2013,2014))
#' @export
#' @importFrom tidyr spread
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr bind_rows
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Returns a map of an specified state including the accident locations.
#'
#' @param state.num An integer value representing the state.
#' @param year The integer value representing the year to be considered.
#' @return Returns a map of an specified state given by \code{state.num} including the accident locations.
#' @examples
#' fars_map_state(30, 13)
#' @importFrom maps map
#' @importFrom graphics points
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}

