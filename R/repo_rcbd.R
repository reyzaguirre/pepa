#' Automatic report for a Randomized Complete Block Design (RCBD)
#'
#' Produces an automatic report for selected variables in an experiment with a RCBD.
#' @param dfr The name of the data frame.
#' @param vars The variables to analize.
#' @param trt The treatments.
#' @param trt.lab The labels for treatments.
#' @param rep The replications.
#' @param eu The experimental unit. Must be defined in case of subsamples.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param mc Logical. If \code{"mc = TRUE"} multiple comparison tests are included
#' even if the factor effect is not significat. Default to \code{"mc = FALSE"}.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @details It fits a linear model for a RCBD and explains the results.
#'
#' Under the assumption of fixed effects an ANOVA table is computed with missing
#' values estimated up to a specified percentage (10\% by default).
#' In case of subsampling the ANOVA table is computed over the means for each
#' experimental unit, and a second ANOVA table is computed considering subsampling
#' only if there are no missing values.
#' If the ANOVA results in a significant value for treatments then the Tukey HSD
#' method for pairwise differences is applied. Assumptions of the model are evaluated
#' with residual plots.
#'
#' Under the assumption of random effects the model is estimated using REML and the
#' variance components are shown. Missing values are not estimated in this case.
#' @return It returns an explanation about the RCBD fitted model.
#' @author Raul Eyzaguirre.
#' @examples
#' repo.rcbd(pjpz09, c("trw", "vw"), "geno", "genotype", "rep")
#' # With a small data set
#' tmp <- pjpz09[1:18, ]
#' repo.rcbd(tmp, c("trw", "vw", "crw"), "geno", "genotype", "rep")
#' @import st4gi
#' @importFrom utils browseURL
#' @export

repo.rcbd <- function(dfr, vars, trt, trt.lab = "treatment", rep, eu = NULL,
                      maxp = 0.1, mc = FALSE,
                      title = "Automatic report for a Randomized Complete Block Design (RCBD)",
                      subtitle = NULL,
                      author = "International Potato Center",
                      format = c("html", "word", "pdf")) {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste0(dirfiles, "/rmd/rcbd.Rmd")
  fileURL <- paste0(dirfiles, "/rmd/rcbd.html")
  fileDOCX <- paste0(dirfiles, "/rmd/rcbd.docx")
  filePDF <- paste0(dirfiles, "/rmd/rcbd.pdf")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(dfr = dfr,
                                  vars = vars,
                                  trt = trt,
                                  trt.lab = trt.lab,
                                  rep = rep,
                                  eu = eu,
                                  maxp = maxp,
                                  mc = mc,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if (format == "html_document")
    try(browseURL(fileURL))

  if (format == "word_document")
    try(system(paste("open", fileDOCX)))

  if (format == "pdf_document")
    try(system(paste("open", filePDF)))

}
