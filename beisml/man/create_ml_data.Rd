% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_ml_data.R
\name{create_ml_data}
\alias{create_ml_data}
\title{Data divider for machine learning}
\usage{
create_ml_data(df, proportion = c(0.8, 0, 0.2), validation = FALSE,
  seed_val = 1537)
}
\arguments{
\item{df}{a data.frame object}

\item{proportion}{the proportion to allocate to training, test and validation.
Default split is training 0.8, test 0.2 and validation 0}

\item{validation}{whether to include a validation split to the data}

\item{seed_val}{set the seed value for reproducibility default 1537}
}
\value{
Returs up to 3 different data sets straight into global environment
}
\description{
`create_ml_data()`
}
\examples{
create_ml_data(df = mtcars)
}
