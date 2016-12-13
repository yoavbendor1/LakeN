#' conctoquant
#'
#' this function transforms concentrations into quantities using the hypsometric curve
#'
#'
#' @param     hypso.curve    (m-sqm) [as.double or as.integer] an hypsometric curve containig the area of each depth of the lake
#' @param     input    (-) input data as dataframe
#' @param     first.data.col    (-) the first column of data for interpolation
#'
#' @author Yoav BD
#' @return quantified data for the lake according to the hypsometric curve

conctoquant <-
  function(hypso.curve=data2,input, first.data.col=3){
    #extract date of measurment
   # temp.date=as.Date(unlist(input$Date))
   # Date=as.Date(rep(temp.date[1],nrow(input)))

    #initialize the data frame to be modified
    data1.split.quant.df=data.frame(input)

    #extract the Depth col
    depth.vec=as.numeric(unlist(input$Depth))

    #match the best Water Depth to the measurment
    #if the depth is found it is used, but in the case of its absence, the closest value is used
    for (jj in 1:length(depth.vec)){
      depth.match=which.min(abs((hypso.curve$WD)-(unlist(input$Depth[jj]))))

      #extract the appropriate area
      area.match=hypso.curve$SA[depth.match]

      #quantify the amount of each specie using the matching surface area based on the hypsometric curve
      temp.quant=as.double(unlist(input[c(jj),c(first.data.col:ncol(input))]))
      temp.quant.2=as.double(temp.quant*area.match)

      data1.split.quant.df[c(jj),c(first.data.col:ncol(input))]=temp.quant.2
    }

   # data1.split.quant.df[,"Depth"]=depth.vec
   # data1.split.quant.df = data1.split.quant.df[!is.na(data1.split.quant.df)]

    return(data1.split.quant.df)
  }
