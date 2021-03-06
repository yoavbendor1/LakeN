#' date difference
#'
#' this function calculates the difference between measurments in days for caculation of time-dependent processes
#'
#' @param     input    (-) input data as list of data splitted into data frames by years
#'
#'
#' @author Yoav BD
#' @return Date_diff a list of vactros including the difference between dates and the sequence of accumulated time between dates

datedifference <-
  function(input){

    years=names(input)

    # initialize the result vector
    Date_diff=vector("list", length=0)
    Date_seq=vector("list", length=0)

    for (jj in 1:length(years)){
      temp=as.data.frame(matrix(data=NA, nrow=length(input[[jj]]$Date)-1, ncol=1))
      temp.2=as.data.frame(matrix(data=1, nrow=length(input[[jj]]$Date), ncol=1))

      for (ii in 1:nrow(temp)){
        temp[ii,] =abs(as.integer(as.Date(input[[jj]]$Date[ii])-as.Date(input[[jj]]$Date[ii+1])))
      }

      temp.2[2:nrow(temp.2),]=temp[1:nrow(temp),]
      for (ii in 2:nrow(temp.2)){
        temp.2[ii,] =as.integer(temp.2[ii,]+temp.2[ii-1,])
      }

      Date_diff[[years[jj]]]=as.data.frame(temp)
      Date_seq[[years[jj]]]=as.data.frame(temp.2)
    }
    result=list(Date_diff,Date_seq)

    return(result)
  }
