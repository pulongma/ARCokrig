% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mvcokm.condsim.R
\name{mvcokm.condsim}
\alias{mvcokm.condsim}
\title{Conditional simulation at new inputs in autoregressive cokriging models for multivarite output}
\usage{
mvcokm.condsim(obj, input.new, nsample = 30)
}
\arguments{
\item{obj}{a \code{\link{mvcokm}} object construted via the function \code{\link{mvcokm}} in 
this package}

\item{input.new}{a matrix including new inputs for making prediction}

\item{nsample}{a numerical value indicating the number of samples}
}
\description{
This function makes prediction based on conditional simulation in
 autogressive cokriging models for multivariate output
}
\seealso{
\code{\link{mvcokm}}, \code{\link{mvcokm.fit}}, \code{\link{mvcokm.predict}}, \code{\link{ARCokrig}}
}
\author{
Pulong Ma <mpulong@gmail.com>
}
