#' reads a FARS .csv data set
#' 
#' @description
#' this function reads data set availavle in csv files from the NHTSA Fatality Analysis Reporting System (FARS)
#' @return Returns a data frame from a csv file in tabular format
#' @note dplyr and tbl_df is used to format the returned data frame
#' @note if file does not exist in the repository or if the file path is incorrect then the function will stop working 
#' @note if the given information is correct the function will return the Data Frame Tbl of the csv file
#' @param filename A character string giving the name of the .csv file
#' @return the function returns a tibble (data.frame) based on the CSV file
#' @return the csv file in the format of Data Frame Tbl
#' @note files need to be located in the working directory.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
	if(!file.exists(filename))
		stop("file '", filename, "' does not exist")
	data <- suppressMessages({
		readr::read_csv(filename, progress = FALSE)
	})
	dplyr::tbl_df(data)
}
#' creates a file name
#'
#' @param year numerical of integer input indicating a year of a data set
#' @return a character string in a format "accident_<year>.csv.bz2" which can be used as a file name with the given year argument in a form "accident_<year>.csv.bz2" 
#' @export
make_filename <- function(year) {
	year <- as.integer(year)
	sprintf("accident_%d.csv.bz2", year)
}
#' creates new data files from the FARS data
#'
#' @param years Desired range of years as numeric values. if a year is incorrect, an error essage will generate
#' @return Returns a filtered list of dataframes with a year and month variable or NULL if it doesn't exist
#' @importFrom dplyr mutate select %>%
#' @details Uses the dplyr function to create a new data files with month and year variables. if the year is not valid, an error essage will generate
#' @details Uses the function make_filename and fars_read
#' @export
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
#' counts number of accidents records for the month of the specified year
#'
#' @note this function returns the data frame of monthly record counts for the specified year(s) 
#' @note the first column of the returned data frame is the month
#' @note the second column is the record counts of the corresponding month for the passed year(s)
#' @param years an integer year, a list of integer years and a vector of integer
#' @return Returns a pivot tibble (data frame) with months in rows and selected years in columns containing the number of accidents
#' @return a warning for every input year that does not exist in the datasets
#' @return an error (no results) if a non numeric or a non integer input is passed
#' @return a data frame with the first column of the returned data frame is the month and the rest of the columns of the monthly counts for the passed year(s)
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#' @details use the function far_read_years
#' @export
fars_summarize_years <- function(years) {
	dat_list <- fars_read_years(years)
	dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#' plot the map of accidents with the state ploted on the map
#'
#' @note this function is to create map, which plots the location of the record on the map for the specified state and year
#' @note the state number should be integer or numerical and should exist in the data, otherwise the function terminates with an error
#' @param state.num the number of a state is a numeric or an integer in the US as used in the FARS datasets
#' @param year field which becomes an integer input
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dplyr filter
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