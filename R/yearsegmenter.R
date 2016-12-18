#' date segmenter
#'
#' this function extracts a segment of a data set according to a selected date range
#'
#' @param     first.data.col    (-) the first column of data for interpolation
#' @param     input    (-) input data as dataframe
#'
#' @author Yoav BD
#' @return only the data that falls into the required date range

yearsegmenter <-
  function(first.data.col=3,input){

    # find number of years in the input
    start.years=unique(as.Date(strftime(as.Date(input$Date), format = "%Y-01-01")))
    end.years=as.Date(strftime(as.Date(start.years), format = "%Y-12-31"))

    start.years=as.data.frame(start.years)
    end.years=as.data.frame(end.years)

    years=format(start.years,'%Y')

    # initialize the result vector
    result=vector("list", length=0)

    for (jj in 1:nrow(start.years)){

      temp.ind=as.data.frame(matrix(data=0, ncol=1, nrow=nrow(input)))

      for (ii in 1:nrow(input)){

        # Convert dates from any year to 2012 dates
        temp.1=ifelse (as.Date(input$Date[ii]) >= as.Date(start.years[jj,]) & as.Date(input$Date[ii]) <= as.Date(end.years[jj,]), yes=1, no=0)
        temp.ind[ii,]=as.integer(temp.1)
      }

      row_sub = apply(temp.ind, 1, function(row) all(row ==1 ))

      result[[years[jj,]]]=as.data.frame(input[row_sub,])
    }
      #result[,first.data.col]=input[,first.data.col]*temp.index
      return(result)
    }

