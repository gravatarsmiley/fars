% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars.R
\name{fars_read}
\alias{fars_read}
\title{reads a FARS .csv data set}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string giving the name of the .csv file}
}
\value{
Returns a data frame from a csv file in tabular format

the function returns a tibble (data.frame) based on the CSV file

the csv file in the format of Data Frame Tbl
}
\description{
this function reads data set availavle in csv files from the NHTSA Fatality Analysis Reporting System (FARS)
}
\note{
dplyr and tbl_df is used to format the returned data frame

if file does not exist in the repository or if the file path is incorrect then the function will stop working

if the given information is correct the function will return the Data Frame Tbl of the csv file

files need to be located in the working directory.
}
