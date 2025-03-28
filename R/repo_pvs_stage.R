#' Report for PVS at stage
#'
#' Produce standard reports for PVS data at different stages.
#' @param dfr A data frame with data for a pvs at some stage.
#' @param stage \code{flowering}, \code{harvest} or \code{post-harvest}.
#' This is only for titles in the report.
#' @param geno Name of the genotypes.
#' @param rep Replications.
#' @param msm Mother score for men.
#' @param msw Mother score for women.
#' @param msg Mother score global.
#' @param bsm Baby score for men.
#' @param bsw Baby score for women.
#' @param bsg Baby score global.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @return It returns an automatic report for the PVS data at different stages:
#' flowering, harvest, posthavest.
#' @author Raul Eyzaguirre.
#' @examples
#' repo.pvs.stage(pvsf2, 'flowering', 'INSTN', 'REP',
#'                'MSM', 'MSWM', 'MSGLO', 'BSM', 'BSWM', 'BSGLO')
#' @import ggplot2
#' @importFrom factoextra fviz_pca
#' @export

repo.pvs.stage <- function(dfr, stage = NULL, geno = 'geno', rep = 'rep',
                           msm = 'msm', msw = 'msw', msg = 'msg',
                           bsm = 'bsm', bsw = 'bsw', bsg = 'bsg',
                           title = "Automatic report for PVS",
                           subtitle = "Best genotypes at",
                           author = "International Potato Center",
                           format = c("html", "word", "pdf")) {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  subtitle <- paste(subtitle, stage)

  fileRmd <- paste0(dirfiles, "/rmd/pvs_stage.Rmd")
  fileURL <- paste0(dirfiles, "/rmd/pvs_stage.html")
  fileDOCX <- paste0(dirfiles, "/rmd/pvs_stage.docx")
  filePDF <- paste0(dirfiles, "/rmd/pvs_stage.pdf")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(dfr = dfr,
                                  stage = stage,
                                  geno = geno,
                                  rep = rep,
                                  msm = msm,
                                  msw = msw,
                                  msg = msg,
                                  bsm = bsm,
                                  bsw = bsw,
                                  bsg = bsg,
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
