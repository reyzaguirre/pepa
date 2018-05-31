#' Automatic report for a Completely Randomized Design (CRD)
#'
#' Produces an automatic report for selected traits in an experiment with a CRD.
#' @param traits The traits to analize.
#' @param geno The genotypes.
#' @param data The name of the data frame.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' @param server logical
#' @param server_dir_name directory name in the server
#' @param server_file_name  file name in the server, without extensions
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a CRD and explains the results.
#'
#' Under the assumption of fixed effects an ANOVA table is computed. If the ANOVA
#' results in a significant value then the Tukey HSD method for pairwise differences
#' is applied. Assumptions of the model are evaluated with residual plots.
#'
#' Under the assumption of random effects the model is estimated using REML and the
#' variance components are shown.
#' @return It returns an explanation about the CRD fitted model.
#' @examples
#' repo.crd(c("trw", "vw"), "geno", pjpz09)
#'
#' # With a small data set
#' temp <- pjpz09[1:18, ]
#' repo.crd(c("trw", "vw", "crw"), "geno", temp)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

repo.crd <- function(traits, geno, data, maxp = 0.1,
                     title = "Automatic report for a Completely Randomized Design (CRD)",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf"),
                     server = FALSE,
                     server_dir_name = "foo",
                     server_file_name = "foo2"
                     ) {



  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

 server<- server

  if(!server){

    fileRmd <- paste(dirfiles, "/rmd/crd.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/crd.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/crd.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/crd.pdf", sep = "")

  } else{

    dirfiles <-  server_dir_name

    fileRmd <-  paste0(dirfiles, "crd.Rmd")   #paste(dirfiles, "/rmd/crd.Rmd", sep = "")
    fileRmd_server_name <-  paste0(dirfiles,  server_file_name, ".Rmd") #r.arias

    #fileURL <- paste(dirfiles, "/rmd/crd.html", sep = "")
    fileDOCX <-  paste0(dirfiles, "crd.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name , ".docx")  #paste(dirfiles, "/rmd/crd.docx", sep = "")
    #filePDF <- paste(dirfiles, "/rmd/crd.pdf", sep = "")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  geno = geno,
                                  data = data,
                                  maxp = maxp,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  #Important Note: #this code copy the word file and create a new one with a unique_name.docx.
  if(server){
      file.copy(fileDOCX, fileDOCX_server_name, overwrite = TRUE)
  }

  if(!server){
      if(format == "html_document") try(browseURL(fileURL))
      if(format == "word_document") try(system(paste("open", fileDOCX)))
      if(format == "pdf_document")  try(system(paste("open", filePDF)))
  }
}
