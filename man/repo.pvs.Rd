% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_pvs.R
\name{repo.pvs}
\alias{repo.pvs}
\title{Report for PVS}
\usage{
repo.pvs(
  dfr,
  form,
  title = "Automatic report for PVS",
  subtitle = NULL,
  author = "International Potato Center",
  format = c("html", "word", "pdf"),
  server = FALSE,
  server_dir_name = "directory",
  server_file_name = "filename"
)
}
\arguments{
\item{dfr}{A data frame with data for a pvs form.}

\item{form}{Form number.}

\item{title}{Report title.}

\item{subtitle}{Report subtitle.}

\item{author}{Report author.}

\item{format}{The output file format for the report, \code{"html"} by default.
Other options are \code{"word"} and \code{"pdf"}.}

\item{server}{Logical. If \code{"FALSE"} works with local machines.
Otherwise works in server environments.}

\item{server_dir_name}{If \code{"server = TRUE"}, this is the directory name in the server.}

\item{server_file_name}{If \code{"server = TRUE"}, this is the file name in the server.}
}
\value{
It returns automatic reports for the different forms on a PVS book.
}
\description{
Produce standard reports for PVS data.
}
\examples{
repo.pvs(pvsf1, 1)
repo.pvs(pvsf2, 2)
repo.pvs(pvsf3, 3)
repo.pvs(pvsf6, 6)
repo.pvs(pvsf6, 7)
repo.pvs(pvsf9, 9)
}
\author{
Raul Eyzaguirre.
}
