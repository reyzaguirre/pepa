#' Automatic report for a MET with a RCBD
#'
#' Produces an automatic report for selected traits in a multi environment
#' trial (MET) with a RCBD in each environment.
#' @param traits The traits to analize.
#' @param trt The treatments.
#' @param env The environments.
#' @param rep The replications.
#' @param dfr The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @details It fits a linear model for a MET with a RCBD for the selected traits.
#' If data is unbalanced, missing values are estimated up to an specified maximum
#' proportion, 10\% by default. Treatments and environments are considered as fixed
#' factors while the blocks are considered as random and nested into the environments
#' for ANOVA.
#' @return It returns an automatic report about the MET with a RCBD fitted model.
#' @author Raul Eyzaguirre.
#' @examples
#' repo.met.agro(c("rytha", "fytha"), "geno", "env", "rep", megaclones)
#' @import agricolae
#' @importFrom utils browseURL
#' @export

repo.met.agro <- function(traits, trt, env, rep, dfr, maxp = 0.1,
                          title = "Automatic report for a MET with a RCBD",
                          subtitle = NULL,
                          author = "International Potato Center",
                          format = c("html", "word", "pdf"),
                          server = FALSE,
                          server_dir_name = "directory",
                          server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/met_agro.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/met_agro.html")
    fileDOCX <- paste0(dirfiles, "/rmd/met_agro.docx")
    filePDF <- paste0(dirfiles, "/rmd/met_agro.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "met_agro.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "met_agro.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  trt = trt,
                                  env = env,
                                  rep = rep,
                                  dfr = dfr,
                                  maxp = maxp,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if (!server) {

    if (format == "html_document")
      try(browseURL(fileURL))

    if (format == "word_document")
      try(system(paste("open", fileDOCX)))

    if (format == "pdf_document")
      try(system(paste("open", filePDF)))

  } else {

    file.copy(fileDOCX, fileDOCX_server_name, overwrite = TRUE)

  }

}
