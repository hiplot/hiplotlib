% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{capitalize}
\alias{capitalize}
\title{Capitalize a string}
\usage{
capitalize(string, locale = "en")
}
\arguments{
\item{string}{Input vector. Either a character vector, or something
coercible to one.}

\item{locale}{Locale to use for comparisons. See
\code{\link[stringi:stri_locale_list]{stringi::stri_locale_list()}} for all possible options.
Defaults to "en" (English) to ensure that default behaviour is
consistent across platforms.}
}
\description{
Capitalize a string
}
\examples{
capitalize("aBcDeF")
}
