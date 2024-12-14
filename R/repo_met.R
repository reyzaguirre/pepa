#' Automatic report for a MET with a RCBD
#'
#' Produces an automatic report for selected variables in a multi environment
#' trial (MET) with a RCBD in each environment.
#' @param dfr The name of the data frame containing the data.
#' @param vars The variables to analize.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param pe Logical. If \code{"pe = TRUE"} multiple comparison tests for principal effects
#' are included even if interaction is significat. Default to \code{"pe = FALSE"}.
#' @param se Logical. If \code{"se = TRUE"} multiple comparison tests for simple effects
#' are included even if interaction is not significat. Default to \code{"se = FALSE"}.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @details It fits a linear model for a MET with a RCBD for the selected variable.
#' If data is unbalanced, missing values are estimated up to an specified maximum
#' proportion, 10\% by default. Genotypes and environments are considered as fixed
#' factors while the blocks are considered as random and nested into the environments
#' for ANOVA. For variance components estimation all the factors are treated as random.
#' @return It returns an automatic report about the MET with a RCBD fitted model.
#' @author Raul Eyzaguirre.
#' @examples
#' repo.met(megaclones, c("rytha", "fytha"), "geno", "env", "rep")
#' @importFrom agricolae LSD.test HSD.test
#' @importFrom utils browseURL
#' @export

repo.met <- function(dfr, vars, geno, env, rep, maxp = 0.1,
                     pe = FALSE, se = FALSE,
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

    fileRmd <- paste0(dirfiles, "/rmd/met.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/met.html")
    fileDOCX <- paste0(dirfiles, "/rmd/met.docx")
    filePDF <- paste0(dirfiles, "/rmd/met.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "met.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "met.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(dfr = dfr,
                                  vars = vars,
                                  geno = geno,
                                  env = env,
                                  rep = rep,
                                  maxp = maxp,
                                  pe = pe,
                                  se = se,
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
