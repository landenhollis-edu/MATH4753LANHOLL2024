% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myncurve.R
\name{myncurve}
\alias{myncurve}
\title{My N Curve}
\usage{
myncurve(mu, sigma, a)
}
\arguments{
\item{mu}{the mean}

\item{sigma}{the standard deviation}

\item{a}{the top bound for the shading}
}
\value{
a list containing mu, sigma, and the calculated probability
}
\description{
My N Curve
}
\examples{
myncurve(mu=10, sigma=2, a=12)
}
