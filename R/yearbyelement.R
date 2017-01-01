#' year by element
#'
#' this function assigns the year value by the element name
#'
#' @param     input    (-) input data as list with element name to be used
#'
#' @author Yoav BD
#' @return the input data as a list with the year value of each row according to the element

yearbyelement <-
  function(input){
    result=input
    # initialize the result vector

    years=names(input)

    for (jj in 1:length(years)){
      Year=matrix(data=NA, nrow=nrow(input[[jj]]), ncol=1)
      for (ii in 1:nrow(input[[jj]])){
        Year[ii]=years[jj]
      }
      result[[years[jj]]]$Year=Year
    }

    return(result)
  }

