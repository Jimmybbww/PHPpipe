% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dilut2Tecan.R
\name{Dilut2Tecan}
\alias{Dilut2Tecan}
\title{Dilut2Tecan}
\usage{
Dilut2Tecan(outPath, db, array, start, appendix = F, exclude = F, n.out = 95)
}
\arguments{
\item{outPath}{a character string naming a output path.}

\item{db}{connection handle returned by `RODBC::odbcConnect`.}

\item{array}{array number.}

\item{start}{the first work ID to be diluted.}

\item{appendix}{a file containing re-diluted, manual diluted and exclusion samples.}

\item{exclude}{a *logical* value indicating whether the samples to be excluded.}

\item{n.out}{the maximum number of sample.}
}
\description{
Create Tecan format (.gwl) and samples list (.txt).
}
