#' date splitter
#'
#' this function splits the input data.frame into a list of data frames according to a specified date range
#'
#' @param     input    (-) input data as dataframe
#'
#' @author Yoav BD
#' @return only the data that falls into the required date range

datesplitter <-
  function(input, max.date,min.date){

# initialize the result vector
years=format(unique(as.Date(strftime(as.Date(input$Date), format = "%Y-01-01"))),'%Y')

min.year=format(unique(as.Date(strftime(as.Date(min.date), format = "%Y-01-01"))),'%Y')
max.year=format(unique(as.Date(strftime(as.Date(max.date), format = "%Y-01-01"))),'%Y')

min.month=format(unique(as.Date(strftime(as.Date(min.date), format = "%Y-%m-01"))),'%m')
max.month=format(unique(as.Date(strftime(as.Date(max.date), format = "%Y-%m-01"))),'%m')

min.day=format(unique(as.Date(strftime(as.Date(min.date), format = "%Y-%m-%d"))),'%d')
max.day=format(unique(as.Date(strftime(as.Date(max.date), format = "%Y-%m-%d"))),'%d')

max.date.vec=matrix(data=NA, ncol=1, nrow=length(years))
min.date.vec=matrix(data=NA, ncol=1, nrow=length(years))

for (jj in 1:length(years)){
  if (min.month < 10) min.format=paste(years[jj],min.month, min.day, sep="-")
  if (min.month >= 10) min.format=paste(years[jj],min.month, min.day, sep="-")

  if (max.month < 10) max.format=paste(years[jj],max.month, max.day, sep="-")
  if (max.month >= 10) max.format=paste(years[jj],max.month, max.day, sep="-")

  max.date.vec[jj,1]=format(unique(as.Date(strftime(as.Date(max.date), format = max.format))),'%Y-%m-%d')
  min.date.vec[jj,1]=format(unique(as.Date(strftime(as.Date(min.date), format = min.format))),'%Y-%m-%d')
}

if (min.date.vec[1,1] > max.date.vec[1,1]) max.date.vec[1,1]=NA
if (min.date.vec[length(min.date.vec),1] > max.date.vec[length(max.date.vec),1]) min.date.vec[length(min.date.vec),1]=NA

min.date.vec=min.date.vec[!is.na(min.date.vec)]
max.date.vec=max.date.vec[!is.na(max.date.vec)]

result=vector("list", length=0)
for (jj in 1:length(max.date.vec)){
  temp.index=matrix(data=NA, ncol=1, nrow=nrow(input))

  for (ii in 1:nrow(input)){

    temp.1=ifelse (input$Date[ii] > max.date.vec[jj] | input$Date[ii] < min.date.vec[jj], 0,
                   ifelse (input$Date[ii] >= min.date.vec[jj] & input$Date[ii] <= max.date.vec[jj], 1))

    temp.index[ii,1]=temp.1

  }
  row_sub = apply(temp.index, 1, function(row) all(row ==1))
  result[[years[jj+1]]]=as.data.frame(input[row_sub,])
}

#result[,first.data.col]=input[,first.data.col]*temp.index
return(result)
}

