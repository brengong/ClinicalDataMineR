% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{BPcleaner}
\alias{BPcleaner}
\title{BPcleaner}
\usage{
BPcleaner(DT, type)
}
\arguments{
\item{DT}{a data table containing a column named "recno". If type = "prior", a column labeled "bpprior", if type = "post", a column labeled "bpafter"}

\item{type:}{either prior of post}
}
\value{
a data table containing one column labeled "orprocedures" and a second colum labeled "recno" containing the initial record number the diagnosis originated from.
}
\description{
transforms blood pressure readings from a continuous to categorical variable.
}
\author{
Brendan Gongol
}
