% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pty_elston.R
\name{pty.elston}
\alias{pty.elston}
\title{Pepa tells you about the Elston index}
\usage{
pty.elston(
  traits,
  geno,
  env = NULL,
  rep = NULL,
  dfr,
  means = "single",
  lb = 1,
  author = "International Potato Center",
  format = c("html", "word", "pdf")
)
}
\arguments{
\item{traits}{List of traits.}

\item{geno}{The genotypes.}

\item{env}{The environments.}

\item{rep}{The replications.}

\item{dfr}{The name of the data frame containing the data.}

\item{means}{The genotypic means to compute the index, \code{"single"} or
\code{"fitted"}. The default is \code{"single"}. See details for more information.}

\item{lb}{Lower bound. \code{1} for \eqn{k = min(x)} and \code{2} for
\eqn{k = (n \times min(x) - max(x)) / (n - 1)}.}

\item{author}{Report author.}

\item{format}{The output file format for the report, \code{"html"} by default.
Other options are \code{"word"} and \code{"pdf"}.}
}
\value{
It returns an explanation about the Elston index.
}
\description{
Explain the results of the Elston index in plain English.
}
\details{
Type \code{?elston} for additional details.
}
\examples{
pty.elston(c("rytha", "bc", "dm", "star", "nocr"), "geno", dfr = spg)
}
\author{
Raul Eyzaguirre.
}
