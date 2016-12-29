#' find oxycline
#'
#' this function deterimnes the best estimation for the depth of the oxycline
#'
#' @param     input    (-) input data as list which has a column with name Oxygen and a column named Depth
#' @param     oxygen.data.col    (-) input data as list which has a column with name Oxygen and a column named Depth
#' @param     p.value    (-) detremine wether a significant difference between the upper layer and deepest part of the lake exists
#' @param     max.depth    (m) max depth value to assign when derivative is smaller than thershold
#' @param     smooth.param    (-) smoothing parameter for oxycline depth. finite double between 0 and 1. default=0.1
#' @param     threshold.depth    (m)
#' @param     threshold    (concentration)
#'
#' @author Yoav BD
#' @return only the data that falls into the required date range

findoxylcline <-
  function(oxygen.data.col=NA,max.depth=NA,p.value=NA,threshold.depth=25,threshold=5,input,depth.interval=10, smooth.param=0.1){

    temp.names=(names(input[[1]]))
    result=vector("list", length=0)

    #if no oxygen column was provided (oxygen.data.col=NA), find which column is oxygen data
    if (is.na(oxygen.data.col)) oxygen.data.col=which(temp.names=='Oxygen')

    #if no max.depth value was provided (max.depth=NA), find maximum depth
    if (is.na(max.depth)) max.depth=max(input[[1]]$Depth)

    #if no max.depth value was provided (max.depth=NA), find maximum depth
    if (is.na(p.value)) p.value=0.5

    # find number of measurment days
    num.measurements=length(c(names(input)))

    # initialize the result vector
    result1=rep(NA, num.measurements)
    result2=rep(NA, num.measurements)


    for (ii in 1:num.measurements){

      temp.data = input[[ii]]$Oxygen
      temp.depth = input[[ii]]$Depth

      #temp.data=lowess(x=seq(from=1, to=length(temp.depth), by=1),y=temp.data,f=smooth.param)
      temp.der=abs(diff(temp.data)/diff(temp.depth))

      depth.ind=which.min(abs(temp.depth-depth.interval))

      test=t.test(temp.data[1:depth.ind],temp.data[(length(temp.data)-depth.ind):length(temp.data)], alternative = "greater")

      result1[ii]=input[[ii]]$Depth[which.max(temp.der)]

      if (abs(test$p.value)>p.value)  result1[ii]=max.depth
      if (temp.data[which.min(abs(temp.depth-round(threshold.depth)))]>threshold)  result1[ii]=max.depth


    }

    result2=lowess(x=seq(from=1, to=length(result1), by=1),y=result1,f=smooth.param)
    result2=result2$y
    result=list(result1, result2)
    return(result)
  }

