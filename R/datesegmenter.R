#' date segmenter
#'
#' this function extracts a segment of a data set according to a selected date range
#' if the time span ranges acrros Jan 1st, be sure to change the year in the ma.date value
#'
#' @param     max.date    (-) the maximum range date to be extracted
#' @param     min.date    (-) the minimum range date to be extracted
#' @param     first.data.col    (-) the first column of data for interpolation
#' @param     input    (-) input data as dataframe
#'
#' @author Yoav BD
#' @return only the data that falls into the required date range

datesegmenter <-
  function(max.date,min.date,first.data.col=3,input){

    # initialize the result vector
    result=input
    temp.index=data.frame(matrix(data=NA, ncol=1, nrow=nrow(input)))

    year1=format(unique(as.Date(strftime(as.Date(max.date), format = "%Y-01-01"))),'%Y')
    year2=format(unique(as.Date(strftime(as.Date(min.date), format = "%Y-01-01"))),'%Y')

    if (year1==year2){
      max.date = as.Date(strftime(max.date, format = "2012-%m-%d")) # max value of date range to extract
      min.date = as.Date(strftime(min.date,  format = "2012-%m-%d")) # min value of date range to extract
    }

    if (year1!=year2){
      max.date = as.Date(strftime(max.date, format = "2013-%m-%d")) # max value of date range to extract
      min.date = as.Date(strftime(min.date,  format = "2012-%m-%d")) # min value of date range to extract
    }


    for (ii in 1:nrow(input)){
      # Convert dates from any year to 2012 dates

      d =as.Date(strftime(input$Date[ii], format="2012-%m-%d"))

      temp.1=ifelse (d > max.date | d < min.date, 0,
                     ifelse (d >= min.date & d <= max.date, 1))

      if (temp.1==0){
      d =as.Date(strftime(input$Date[ii], format="2013-%m-%d"))

      temp.1=ifelse (d > max.date | d < min.date, 0,
                     ifelse (d >= min.date & d <= max.date, 1))
      }
      temp.index[ii,1]=as.integer(temp.1)
    }

    #result[,first.data.col]=input[,first.data.col]*temp.index

    row_sub = apply(temp.index, 1, function(row) all(row ==1 ))
    result=result[row_sub,]

    return(result)
  }
