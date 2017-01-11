#' NSE
#'
#' Nash-Sutcliffe Efficiency Performance Metric
#' @param m modelled value (any unit)
#' @param o observed value (any unit)
#'
#' @author N. Tague
#' @return only the data that falls into the required date range

NSE = function (m, o)
{
  err = m - o
  meanobs = mean(o)
  mse = sum(err*err)
  ovar = sum((o-meanobs)*(o-meanobs))
  nse = 1.0 - mse/ovar
  nse

  return (as.double(nse))
}
