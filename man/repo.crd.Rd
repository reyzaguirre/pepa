% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repo_crd.R
\name{repo.crd}
\alias{repo.crd}
\title{Automatic report for a Completely Randomized Design (CRD)}
\usage{
repo.crd(
  dfr,
  vars,
  trt,
  trt.lab = "treatment",
  eu = NULL,
  mc = FALSE,
  title = "Automatic report for a Completely Randomized Design (CRD)",
  subtitle = NULL,
  author = "International Potato Center",
  format = c("html", "word", "pdf")
)
}
\arguments{
\item{dfr}{The name of the data frame.}

\item{vars}{The variables to analize.}

\item{trt}{The treatments.}

\item{trt.lab}{The labels for treatments.}

\item{eu}{The experimental unit. Must be defined in case of subsamples.}

\item{mc}{Logical. If \code{"mc = TRUE"} multiple comparison tests are included
even if the factor effect is not significat. Default to \code{"mc = FALSE"}.}

\item{title}{Report title.}

\item{subtitle}{Report subtitle.}

\item{author}{Report author.}

\item{format}{The output file format for the report, \code{"html"} by default.
Other options are \code{"word"} and \code{"pdf"}.}
}
\value{
It returns an explanation about the CRD fitted model.
}
\description{
Produces an automatic report for selected variables in an experiment with a CRD.
}
\details{
It fits a linear model for a CRD and explains the results.

Under the assumption of fixed effects an ANOVA table is computed.
In case of subsampling the ANOVA table is computed over the means for each
experimental unit, and a second ANOVA table is computed considering subsampling.
If the ANOVA results in a significant value then the Tukey HSD method for pairwise
differences is applied. Assumptions of the model are evaluated with residual plots.

Under the assumption of random effects the model is estimated using REML and the
variance components are shown.
}
\examples{
repo.crd(pjpz09, c("trw", "vw"), "geno", "genotype")
# With a small data set
tmp <- pjpz09[1:18, ]
repo.crd(tmp, c("trw", "vw", "crw"), "geno", "genotypes")
}
\author{
Raul Eyzaguirre.
}
