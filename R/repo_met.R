#' Automatic report for a MET with a RCBD
#'
#' Produces an automatic report for selected traits in a multi environment
#' trial (MET) with a RCBD in each environment.
#' @param traits The traits to analize.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications.
#' @param data The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a MET with a RCBD for the selected trait.
#' If data is unbalanced, missing values are estimated up to an specified maximum
#' proportion, 10\% by default. Genotypes and environments are considered as fixed
#' factors while the blocks are considered as random and nested into the environments
#' for ANOVA. For variance components estimation all the factors
#' are treated as random.
#' @return It returns an automatic report about the MET with a RCBD fitted model.
#' @examples
#' repo.met(c("rytha", "fytha"), "geno", "env", "rep", megaclones)
#' @import agricolae
#' @importFrom utils browseURL
#' @export

repo.met <- function(traits, geno, env, rep, data, maxp = 0.1,
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
                    params = list(traits = traits,
                                  geno = geno,
                                  env = env,
                                  rep = rep,
                                  data = data,
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
