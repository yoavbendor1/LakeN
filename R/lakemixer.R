#' lake mixer
#'
#' this function simulate lake mixing
#'
#' @param     input    (-) input data as list which has a column with name Oxygen and a column named Depth
#' @param     first.data.col    (-) the first column of data to mix
#'
#' @author Yoav BD
#' @return the geochemical properties of the mixed lake

lakemixer <-
  function(input, max.depth=40){
    # find the sum of the specie in the water column
    tmp.sum=sum(input)
    # redistribute the sum according to a quadratic equation
    nPartition = max.depth;
    r = (max.depth^2*(seq(from=1, to=max.depth, by=1)-max.depth)^2)
    r=r/sum(r)


    tmp=data.frame(rep(0,max.depth))

    d = abs(r-0.5);
    idx = which.min(d);
    tmp = round(tmp.sum*r);
    tmp[idx] = tmp[idx]+(tmp.sum-sum(tmp))
    result=tmp


return(result)
}


