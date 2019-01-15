\name{banknote_authentication}
\alias{banknote_authentication}
\docType{data}
\title{Bank Note Authentication Data Set}
\description{
Bank Note Authentication Data Set is used to show the functionality of nonet package. This Data Set has below attribute information as variance of Wavelet Transformed image (continuous), skewness of Wavelet Transformed image (continuous), curtosis of Wavelet Transformed image (continuous),entropy of image (continuous), class (integer).
}
\usage{data("banknote_authentication")}
\format{
  A data frame with 1372 observations on the following 5 variables.
  \describe{
    \item{\code{variance}}{a numeric vector}
    \item{\code{skewness}}{a numeric vector}
    \item{\code{curtosis}}{a numeric vector}
    \item{\code{entropy}}{a numeric vector}
    \item{\code{class}}{a numeric vector}
  }
}
\details{
Data were extracted from images that were taken from genuine and forged banknote-like specimens. For digitization, an industrial camera usually used for print inspection was used. 
The final images have 400x 400 pixels. Due to the object lens and distance to the investigated object gray-scale pictures with a resolution of about 660 dpi were gained. 
Wavelet Transform tool were used to extract features from images.
}
\source{
This DataSet is fetched from UCI Website. URL is https://archive.ics.uci.edu/ml/datasets/banknote+authentication 
}
\references{
Owner of database: Volker Lohweg (University of Applied Sciences, Ostwestfalen-Lippe, volker.lohweg '@' hs-owl.de) Donor of database: Helene DÃ¶rksen (University of Applied Sciences, Ostwestfalen-Lippe, helene.doerksen '@' hs-owl.de) 
Date received: August, 2012  
}
\examples{
data(banknote_authentication)
dataframe <- data.frame(banknote_authentication)
head(dataframe)
str(banknote_authentication)
}
\keyword{datasets}
