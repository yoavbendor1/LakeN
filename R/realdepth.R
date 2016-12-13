#' real depth
#'
#' this function assigns the real depth of every measurment according to the measurment date
#'
#'
#' @param     depth.date    (date-m) [as.Date-integer] a tabel (as a data frame) that has the water level of the measurment
#' @param     measurment.date    (date) [as.Date] the date during which the measurment was carried
#' @param     input    (-) input data as dataframe
#'
#' @author Yoav BD
#' @return interpolated data for the lake according to the provided maximum depth

realdepth <-
  function(depth.date=data3,measurment.date,input){

    data1.split.interp.depth=input

    #match the best date to the measurment day
    #if the date is found it is used, but in the case of its absence, the closest day is used
    date.match=which(abs(unlist(as.Date(data3$date))-as.Date(measurment.date))== min(abs(unlist(as.Date(data3$date)-as.Date(measurment.date)))))

    #extract the appropriate depth
    level.match=depth.date$level[date.match]

    #calculate the new depths of the measurments
    temp.depth=as.numeric(unlist(input$Depth))
    new.depth=level.match-temp.depth

    data1.split.interp.depth[,"Depth"]=new.depth

    return(data1.split.interp.depth)
  }
