% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloadInputs.R
\docType{methods}
\name{downloadInputs}
\alias{downloadInputs}
\title{Download and store hash value from downloaded file}
\usage{
downloadInputs(inputs, cacheRepo = NULL, mask = NULL, quick = FALSE,
  cascade = FALSE, quiet = TRUE)
}
\arguments{
\item{inputs}{A character string. Represents the url of file to be downloaded.}

\item{cacheRepo}{A repository used for storing cached objects.}

\item{mask}{A character string. A shapefile used to crop downloaded rasters or shapefiles to a specific region}

\item{quick}{Logical. If \code{TRUE}, checksum is compiled using the combination
of the filename and its size.
If \code{FALSE} (default), cheksum is compiled using the object.}

\item{cascade}{Logical. If \code{FALSE}, file is untar and/or unzip. Default is \code{FALSE}.}

\item{quiet}{Logical. If \code{TRUE}, suppress status messages (if any), and the progress bar.}
}
\value{
Invoked for its side-effect of downloading files to the \code{destfile/} directory.
}
\description{
To avoid redownloading existing files, verify if the file exists locally.
If the file exists, compare the checksum value to that logged by original download.
}
\examples{
downloadInputs("NFDB_PT")

}
\author{
Melina Houle

Jean Marchal
}
