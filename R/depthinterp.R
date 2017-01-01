#' interpolate data by depth
#'
#' this function intepolates a list comprising the N measurments of a lake
#' package zoo is required to fill NA values in the matrix
#'
#'
#' @param     maxdepth    (m) max depth to be interpolated for the lake
#' @param     intreval    (m) the depth difference between each of the interpolation points, default is 1 m
#' @param     first.data.col    (-) the first column of data for interpolation
#' @param     col.num    (-) number of columns in the original data table
#' @param     input    (-) input data as dataframe
#' @param     depth.col (-) the column that contains the depths of measurments
#'
#' @author Yoav BD
#' @return interpolated data for the lake according to the provided maximum depth

depthinterp <-
  function(maxdepth=40,intreval=1, first.data.col=3,input,col.num=ncol(input), depth.col=2 ){
    #extract date of measurment
    temp.date=as.Date(unlist(input$Date))
    Date=as.Date(rep(temp.date[1],maxdepth))

    #initialize internal variables
    interp.depth=seq(from=1, to=maxdepth, by=1)
    # extract measurments depths
    input.depth=as.numeric(unlist(input[,c(depth.col)]))

    #find columns that are all NA and fill them with zero prior to interpolation
    zero.index=NA
    zero.index=which(sapply(input, function(x)all(is.na(x))))
    #zero.index=df[ , colSums(is.na(df)) == 0]
    if (is.integer(zero.index)){
      input[,zero.index]=c(rep.int(0, times=nrow(input)))}

    #initialize output matrix [df] for interpolation
    df=data.frame(matrix(data=NA, nrow=maxdepth, ncol=ncol(input)-first.data.col+1))

    # initialize interpolation loop
    for(jj in first.data.col:col.num){
      input.var=as.numeric(unlist(input[,c(jj)]))
      app.var=approx(input.depth,input.var, xout=interp.depth, method="linear")
      app.var=as.numeric(unlist(app.var[2]))
      df[,jj-first.data.col+1]=c(as.numeric(app.var))
    }

    # assign column names to the matrix
    df.names=names(input)
    df.names=df.names[first.data.col:col.num]
    colnames(df, do.NULL = FALSE)
    colnames(df)=c(df.names)
    rownames(df, do.NULL = FALSE)
    rownames(df)=c(interp.depth)

    # replace NA with last available values
    df=na.locf(df)
    Depth=interp.depth
    df2=data.frame(Date,Depth,df)

    return(df2)
  }
