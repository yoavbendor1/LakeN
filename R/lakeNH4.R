#' lakeNH4
#'
#' This model carries the calculations of the NH4 cycle in the lake for a series of time-series data
#' @param model.data a data frame that has the first time step and is ready to incorporate all the results of the calculations. row number should equal the length of time series times the depths interpolated. the data.frame should include columns named Date, Depth, and NH4
#' @param max.depth (m) maximum depth to be interpolated
#' @param mean.rand.NH4 (max.depth) the mean value for the noise factor of the process
#' @param sd.rand.NH4 (concentration) the sd value for the noise factor of the process
#' @param data.dates (date in official non-ambigous form "yyyy-mm-dd" ) the dates of the data to be evaluated
#' @param mix.index (date in official non-ambigous form "yyyy-mm-dd" )the last mix of the lake. default is first date of time series
#' @param mix.delay.ind (days) an integer indicating time since last mixing of the lake
#' @param first.mix (date in official non-ambigous form "yyyy-mm-dd" )the last mix of the lake. default is the middle date of the time series
#' @param first.mix.delay.ind (days) an integer indicating time since the first mixing of the lake in that year
#' @param parameters.NH4.consumption (integers) a set of parameters for the linear fit of NH4 consumption. number of coefficients should match interpolation depth
#' @param NH4.comsumption.correction.factor (integer) the mean value for the noise factor of the process
#' @param parameters.NH4.buildup (integers) a set of parameters for the linear fit of NH4 buildup. number of coefficients should match interpolation depth
#' @param date.dif.for.model.seq (integers) a vector of integers indicating the time difference between each of the measurments
#' @param oxycline.depth.date (Date-depth) the depth of the oxycline in each of dates to be analyzed
#' @param crit.depth (m) the depth of the oxycline from which the lake is considered mixed
#' @param mixing.ratio.NH4 (-) a value indicating a factor to multiply after mixing to aacount for consumption during mixing
#' @param dur2 (d) delay between first mixing of the seaon and the onset of NH4 consumption
#' @param dur3 (d) maximum length of NH4 consumption period

#' @author Y. Ben Dor
#' @return only the data that falls into the required date range

lakeNH4 = function (model.data,mean.rand.NH4=3e5, sd.rand.NH4=3e5/10, data.dates=as.Date(unique(mode.data$Date)), mix.index=as.Date(data.dates[1]),
                    mix.delay.ind=0, first.mix=as.Date(as.Date(data.dates[length(data.dates)/2])), parameters.NH4.consumption, NH4.comsumption.correction.factor=1, date.dif.for.model.seq,
                    oxycline.depth.date,crit.depth=35, mixing.ratio.NH4=0.95,parameters.NH4.buildup, dur2=30, dur3=120, first.mix.delay.ind=1.5*dur3, max.depth)
{

  # collect data on mixing-stratification conditions
  indicator=data.frame(indicator=rep(0,length(data.dates)))
  row.names(indicator)=data.dates

  # initialize indexes for counting
  num.strat=0
  num.mix=0
  num.none=0
  ii.mix=0

  # find mode function
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  start.row=as.integer(max.depth+1)
  end.row=as.integer(2*max.depth)

  for (ii in 2:length(data.dates)){
    if.index=0
    rm(tmp.NH4, tmp.nitrate, tmp.data)
    # extract the data for the previous date
    tmp.data=data.frame(model.data[((ii-2)*max.depth+1):(((ii-2)*max.depth)+max.depth),])

    # initialize empty variables for loop
    tmp.NH4=data.frame(NH4=rep(0,max.depth))
    tmp.nitrate=data.frame(nitrate=rep(0,max.depth))

    # create noise for calculations
    NH4.noise=data.frame(NH4.noise=sign(rnorm(max.depth, mean=0, sd=1))*rnorm(max.depth, mean=mean.rand.NH4, sd=sd.rand.NH4))

    # if the mixing depth has reached the critical depth than the lake is mixed
    # the lake cannot be mixed twice within a time span smaller than mix.delay
    if (mix.index!=data.dates[ii]){
      mix.delay.ind=as.integer(abs(as.Date(mix.index)-as.Date(data.dates[ii])))}

    if (first.mix!=data.dates[ii]){
      first.mix.delay.ind=as.integer(abs(as.Date(first.mix)-as.Date(data.dates[ii])))}

    if (first.mix.delay.ind>dur2 & first.mix.delay.ind<dur3){
      indicator[ii,]=4
      tmp.NH4$NH4[1:max.depth]=tmp.data$NH4[1:max.depth]+NH4.comsumption.correction.factor*parameters.NH4.consumption[1:max.depth]*(date.dif.for.model.seq[ii-1])
      if.index=1
      num.mix=num.mix+1
    }

    if (oxycline.depth.date$oxycline_int[ii]<crit.depth & if.index!=1){
      # the lake is stratified
      num.strat=num.strat+1
      indicator[ii,]=1
      if.index=1

      #   if (first.mix.delay.ind<dur2){
      # NH4 buildup (only in hypolimnion)
      epi.depth=oxycline.depth.date$oxycline_int[ii]
      tmp.NH4$NH4[1:epi.depth]=tmp.data$NH4[1:epi.depth]
      tmp.NH4$NH4[(epi.depth+1):max.depth]=tmp.data$NH4[(epi.depth+1):max.depth]+parameters.NH4.buildup[(epi.depth+1):max.depth]*(date.dif.for.model.seq[ii-1])
    }


    if (oxycline.depth.date$oxycline_int[ii] > crit.depth & if.index!=1){
      indicator[ii,]=2
      # during first mix of the season the amount is reduced by mixing.ratio
      tmp.NH4$NH4=mixing.ratio.NH4*lakemixer(input=tmp.data$NH4)
      mix.index=as.Date(data.dates[ii])
      if (mean(indicator$indicator[(ii-4):(ii-1)])==1) first.mix=data.dates[ii]
      if.index=1
      num.mix=num.mix+1
    }

    # if the lake was recently mixed but stratification is not yet fully developed
    if (if.index!=1){
      indicator[ii,]=3
      num.none=num.none+1
    }


    tmp.NH4$NH4=tmp.NH4+NH4.noise
    # break when all data is exhausted
    if (start.row>nrow(data.for.model)) break
    tmp.NH4$NH4[tmp.NH4$NH4<0]=0
    model.data$NH4[start.row:end.row]=unlist(tmp.NH4)

    start.row=as.integer(end.row+1)
    end.row=as.integer(start.row+max.depth-1)
  }
  return(model.data)
}
