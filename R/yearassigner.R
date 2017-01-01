#' year assigner
#'
#' this function assigns the year of wach mesurment into a new column
#'
#' @param     input    (-) input data as dataframe
#'
#' @author Yoav BD
#' @return the input data with an additional date formatted column indicating the year of the measurement
yearassigner <-
  function(input){

    # initialize the result vector
    Year=matrix(data=NA, nrow=nrow(input), ncol=1)
    result=cbind(input, Year)

    for (ii in 1:nrow(input)){
      Year[ii]=format(unique(as.Date(strftime(as.Date(input$Date[ii]), format = "%Y-01-01"))),'%Y')
    }

    result=cbind(input, Year)

    #result[,first.data.col]=input[,first.data.col]*temp.index
    return(result)
  }

