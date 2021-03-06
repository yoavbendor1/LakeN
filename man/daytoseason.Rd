% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daytoseason.R
\name{daytoseason}
\alias{daytoseason}
\title{day to season}
\usage{
daytoseason(input)
}
\arguments{
\item{input}{(-) input data as dataframe}
}
\value{
season classification of the date vector
}
\description{
this function adds a column indicating the season of each measurement day
the seasons are assigned according to the following principles
dates        season   nominal value
22/12-21/3 - winter - 1
22/3-21/6 - spring -2
22/6-21/9 - summer -3
22/9-21/12 - fall -4
}
\author{
Yoav BD
}

