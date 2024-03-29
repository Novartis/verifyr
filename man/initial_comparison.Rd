% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initial_comparison.R
\name{initial_comparison}
\alias{initial_comparison}
\title{Initial comparison of content of two files}
\usage{
initial_comparison(old, new, omit = NULL)
}
\arguments{
\item{old}{character, giving the full file path and name of the old file (required)}

\item{new}{character, giving the full file path and name of the new file (required)}

\item{omit}{character, giving the string to identify the rows which should be omitted in the comparison (optional)}
}
\value{
Returns a vector \code{diff_print} with basic info about the differences in the content of the files
}
\description{
\code{initial_comparison} returns basic info about the differences of two files.
}
\examples{
verifyr::initial_comparison(old = paste0(fs::path_package("/extdata/base_files/",
                                                          "14-1.01.rtf",
                                                          package = "verifyr")),
                            new = paste0(fs::path_package("/extdata/compare_files/",
                                                          "14-1.01.rtf",
                                                          package = "verifyr")))

}
