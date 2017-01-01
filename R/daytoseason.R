#' day to season
#'
#' this function adds a column indicating the season of each measurement day
#' the seasons are assigned according to the following principles
#' dates        season   nominal value
#' 22/12-21/3 - winter - 1
#' 22/3-21/6 - spring -2
#' 22/6-21/9 - summer -3
#' 22/9-21/12 - fall -4
#'
#'
#' @param     input    (-) input data as dataframe
#'
#' @author Yoav BD
#' @return season classification of the date vector

daytoseason <-
  function(input){

    # initialize the result vector
    season=data.frame(matrix(data=NA, ncol=2, nrow=length(input)))

    WS = as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
    SE = as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
    SS = as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
    FE = as.Date("2012-9-21",  format = "%Y-%m-%d") # Fall Equinox

    for (ii in 1:length(input)){
    # Convert dates from any year to 2012 dates
    # use 2012 because it is a leap year
    d =as.Date(strftime(input[ii], format="2012-%m-%d"))

    temp.1=ifelse (d >= WS | d < SE, "Winter",
            ifelse (d >= SE & d < SS, "Spring",
                    ifelse (d >= SS & d < FE, "Summer", "Fall")))

    temp.2=ifelse (d >= WS | d < SE, "1",
                   ifelse (d >= SE & d < SS, "2",
                           ifelse (d >= SS & d < FE, "3", "4")))

    season[ii,1]=temp.1
    season[ii,2]=as.integer(temp.2)

    }
    df=cbind(input,season)
    colnames(df, do.NULL = TRUE, prefix = "col")
    colnames(df) <- c("Date", "Season","Season_ind")

    return(df)
  }
