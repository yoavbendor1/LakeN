#' date difference df
#'
#' this function calculates the difference between measurments in days for caculation of time-dependent processes, and is adjusted for a dataframe (rather than datedifference wich is suitable for a year splitted list)
#'
#' @param     input    (-) input data as list of data splitted into data frames by years
#'
#'
#' @author Yoav BD
#' @return Date_diff a list of vectros including the difference between dates and the sequence of accumulated time between dates

datedifferencedf <-
  function(input){

    # initialize the result vector
    Date_diff=vector("list", length=0)
    Date_seq=vector("list", length=0)


      temp=as.data.frame(matrix(data=NA, nrow=length(input$Date)-1, ncol=1))
      temp.2=as.data.frame(matrix(data=1, nrow=length(input$Date), ncol=1))

      for (ii in 1:nrow(temp)){
        temp[ii,] =abs(as.integer(as.Date(input$Date[ii])-as.Date(input$Date[ii+1])))
      }

      temp.2[2:nrow(temp.2),]=temp[1:nrow(temp),]
      for (ii in 2:nrow(temp.2)){
        temp.2[ii,] =as.integer(temp.2[ii,]+temp.2[ii-1,])
      }

      Date_diff=as.data.frame(temp)
      Date_seq=as.data.frame(temp.2)

    result=list(Date_diff,Date_seq)

    return(result)
  }
