#' fit equation
#'
#' this function retrieves the equation for a linear model fit
#'
#' @param     input    (-) input data as dataframe
#'
#' @author Yoav BD
#' @return the equation of the fit as character

fitequation <-
  function(input){
      m =input
      eq <- substitute(italic(y) == b + a %.% italic(x)*","~~italic(r)^2~"="~r2,
                       list(b = format(coef(m)[1], digits = 2, scientific=TRUE),
                            a = format(coef(m)[2], digits = 2, scientific=TRUE),
                            r2 = format(summary(m)$r.squared, digits = 2)))
      return(as.character(as.expression(eq)))
  }


