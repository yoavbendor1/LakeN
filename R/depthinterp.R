#' interpolate data by depth
#'
#' this function intepolates a list comprising the N measurments of a lake
#' package zoo is required to complete NA values in the matrix
#'
#'
#' @param     maxdepth    (m) max depth to be interpolated for the lake
#' @param     intreval    (m) the depth difference between each of the interpolation points, default is 1 m
#' @param     first.data.col    (-) the first column of data for interpolation
#' @param     col.num    (-) number of columns in the original data table
#' @param     input    (-) input data as list
#' @param     depth.col (-) the column that contains the depths of measurments
#'
#' @author Yoav BD
#' @return interpolated data for the  (arbitrary units)

depthinterp <-
  function(maxdepth=40,intreval=1, first.data.col=3,input,col.num=as.integer(ncol(i[[1]])), depth.col=2 ){

    #initialize internal variables
    interp.depth=seq(from=1, to=maxdepth, by=1)
    # extract measurments depths
    input.depth=as.numeric(unlist(input[,c(depth.col)]))

    #initialize output matrix for interpolation
    df=matrix(data=NA, nrow=maxdepth, ncol=col.num-first.data.col+1)

    # initialize interpolation loop
    for(ii in first.data.col:col.num){
      input.var=as.numeric(unlist(input[,c(ii)]))
      app.var=approx(input.depth,input.var, xout=interp.depth, method="linear")
      app.var=as.numeric(unlist(app.var[2]))
      df[,ii-first.data.col+1]=c(app.var)
    }

    # assign column names to the matrix
    df.names=names(i[[1]])
    df.names=df.names[first.data.col:col.num]
    colnames(df, do.NULL = FALSE)
    colnames(df)=c(df.names)
    rownames(df, do.NULL = FALSE)
    rownames(df)=c(interp.depth)

    # replace NA with last available values
    df=na.locf(df)
    Depth=interp.depth
    df2=data.frame(Depth,df)

    return(df2)
  }
