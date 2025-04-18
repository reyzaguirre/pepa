% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_spld.R
\name{repo.spld}
\alias{repo.spld}
\title{Automatic report for a split plot design}
\usage{
repo.spld(
  dfr,
  vars,
  mpf,
  spf,
  sspf = NULL,
  rep,
  pe = FALSE,
  title = NULL,
  subtitle = NULL,
  author = "International Potato Center",
  format = c("html", "word", "pdf")
)
}
\arguments{
\item{dfr}{The name of the data frame.}

\item{vars}{The variables to analize.}

\item{mpf}{The main plots factor.}

\item{spf}{The sub plots factor.}

\item{sspf}{The sub sub plot factor (if any).}

\item{rep}{The replications.}

\item{pe}{Logical. If \code{"pe = TRUE"} multiple comparison tests for principal effects
are included even if interaction is significat, only in the case of 2 factors.
Default to \code{"pe = FALSE"}.}

\item{title}{Report title.}

\item{subtitle}{Report subtitle.}

\item{author}{Report author.}

\item{format}{The output file format for the report, \code{"html"} by default.
Other options are \code{"word"} and \code{"pdf"}.}
}
\value{
It returns an explanation about the split-plot or a split-split-plot design
fitted model.
}
\description{
Produces an automatic report for selected variables in an experiment
with a split-plot or a split-split-plot design.
}
\details{
It fits a linear model for a split-plot or a split-split-plot design and explains
the results.
}
\examples{
# Create designs
mpf <- paste0("mpf", 1:2)
spf <- paste0("spf", 1:3)
sspf <- paste0("sspf", 1:4)
d1 <- cr.spld(c("mpf", "spf"), list(mpf, spf), 3)$book
d2 <- cr.spld(c("mpf", "spf", "sspf"), list(mpf, spf, sspf), 2)$book
# Some random data
d1$yield <- rnorm(18, 10)
d2$yield <- rnorm(48, 10)
# Fit models
repo.spld(d1, "yield", "mpf", "spf", NULL, "block")
repo.spld(d2, "yield", "mpf", "spf", "sspf", "block")
}
\author{
Raul Eyzaguirre.
}
