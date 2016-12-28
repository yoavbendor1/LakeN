#' find oxycline
#'
#' this function deterimnes the best estimation for the depth of the oxycline
#'
#' @param     input    (-) input data as list which has a column with name Oxygen and a column named Depth
#' @param     oxygen.data.col    (-) input data as list which has a column with name Oxygen and a column named Depth
#' @param     threshold    (-) derivative threshold to define max depth. if max derivative is below this number, max depth value will be assigned as thermocline depth
#' @param     max.depth    (m) max depth value to assign when derivative is smaller than thershold
#' @param     smooth.param    (-) smoothing parameter for oxycline depth. finite double between 0 and 1. default=0.1
#'
#'
#' @author Yoav BD
#' @return only the data that falls into the required date range

findoxylcline <-
  function(oxygen.data.col=NA,max.depth=NA,threshold=NA,input,depth.interval=10, smooth.param=0.1){

    temp.names=(names(input[[1]]))
    result=vector("list", length=0)

    #if no oxygen column was provided (oxygen.data.col=NA), find which column is oxygen data
    if (is.na(oxygen.data.col)) oxygen.data.col=which(temp.names=='Oxygen')

    #if no max.depth value was provided (max.depth=NA), find maximum depth
    if (is.na(max.depth)) max.depth=max(input[[1]]$Depth)

    #if no max.depth value was provided (max.depth=NA), find maximum depth
    if (is.na(threshold)) threshold=0.5

    # find number of measurment days
    num.measurements=length(c(names(input)))

    # initialize the result vector
    result1=rep(NA, num.measurements)


    for (ii in 1:num.measurements){

      temp.data = input[[ii]]$Oxygen
      temp.depth = input[[ii]]$Depth

      #temp.data=lowess(x=seq(from=1, to=length(temp.depth), by=1),y=temp.data,f=smooth.param)
      temp.der=abs(diff(temp.data)/diff(temp.depth))

      test=t.test(temp.data[1:depth.interval],temp.data[max.depth-depth.interval:max.depth])
      result1[ii]=which.max(temp.der)

      if (abs(mean(c(test$conf.int[[2]],test$conf.int[[1]])))<threshold)  result1[ii]=max.depth
    }

    result2=lowess(x=seq(from=1, to=length(result1), by=1),y=result1,f=smooth.param)
    result2=result2$y
    result=list(result1, result2)
    return(result)
  }

