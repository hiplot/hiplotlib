#' @title Export snippets
#'
#' @description \code{addPackageSnippets} copies all (missing) snippet definitions
#'   in 'inst/rstudio/Rsnippets.txt' and 'Rmdsnippets.txt' to the RStudios user snippet location.
#'
#' @return boolean invisible(FALSE) if nothing was added, invisible(TRUE) if snipped definitions were added
#' @export
#' @references https://stackoverflow.com/questions/62220596/can-r-packages-add-code-snippets-to-users-snippet-files
#' @examples
#' \dontrun{
#' addPackageSnippets()
#' }
addPackageSnippets <- function() {
  added <- FALSE

  # if not on RStudio or RStudioServer exit
  #
  if (!nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    return(NULL)
  }

  # Name of files containing snippet code to copy
  #
  pckgSnippetsFiles <- c("Rsnippets.txt", "Rmdsnippets.txt")

  # Name of files to copy into. Order has to be the same
  # as in 'pckgSnippetsFiles'
  #
  rstudioSnippetsFiles <- c("r.snippets", "markdown.snippets")

  # Path to directory for RStudios user files depends on OS
  if (rstudioapi::versionInfo()$version < "1.3") {
    rstudioSnippetsPathBase <- file.path(path.expand("~"), ".R", "snippets")
  } else {
    if (.Platform$OS.type == "windows") {
      rstudioSnippetsPathBase <- file.path(Sys.getenv("APPDATA"), "RStudio", "snippets")
    } else {
      rstudioSnippetsPathBase <- file.path(path.expand("~"), ".config/rstudio", "snippets")
    }
  }

  # Read each file in pckgSnippetsFiles and add its contents
  #
  for (i in seq_along(pckgSnippetsFiles)) {

    # Try to get template, if template is not found skip it
    #
    pckgSnippetsFilesPath <- system.file("rstudio", pckgSnippetsFiles[i], package = "hiplotlib")
    if (pckgSnippetsFilesPath == "") {
      next()
    }

    # load package snippets definitions
    #
    pckgSnippetsFileContent <- readLines(pckgSnippetsFilesPath, warn = FALSE)

    # Extract names of package snippets
    #
    pckgSnippetsFileDefinitions <- pckgSnippetsFileContent[grepl("^snippet (.*)", pckgSnippetsFileContent)]


    # Construct path for destination file
    #
    rstudioSnippetsFilePath <- file.path(rstudioSnippetsPathBase, rstudioSnippetsFiles[i])

    # If targeted RStudios user file does not exist, raise error (otherwise we would 'remove')
    # the default snippets from the 'user file'
    #
    if (!file.exists(rstudioSnippetsFilePath)) {
      stop(paste0(
        "'", rstudioSnippetsFilePath, "' does not exist yet\n.",
        "Use RStudio -> Tools -> Global Options -> Code -> Edit Snippets\n",
        "To initalize user defined snippets file by adding dummy snippet\n"
      ))
    }

    # Extract 'names' of already existing snitppets
    #
    rstudioSnippetsFileContent <- readLines(rstudioSnippetsFilePath, warn = FALSE)
    rstudioSnippetDefinitions <- rstudioSnippetsFileContent[grepl("^snippet (.*)", rstudioSnippetsFileContent)]

    # replace two spaces with tab, ONLY at beginning of string
    #
    pckgSnippetsFileContentSanitized <- gsub("(?:^ {2})|\\G {2}|\\G\t", "\t", pckgSnippetsFileContent, perl = TRUE)

    # find defintions appearing in packageSnippets but not in rstudioSnippets
    # if no snippets are missing go to next file
    #
    snippetsToCopy <- setdiff(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    snippetsNotToCopy <- intersect(trimws(pckgSnippetsFileDefinitions), trimws(rstudioSnippetDefinitions))
    if (length(snippetsToCopy) == 0) {
      # cat(paste0("(\nFollowing snippets will NOT be added because there is already a snippet with that name: ",
      #            paste0(snippetsNotToCopy, collapse=", ") ,")"))
      next()
    }

    # Inform user about changes, ask to confirm action
    #
    if (interactive()) {
      cat(paste0(
        "You are about to add the following ", length(snippetsToCopy),
        " snippets to '", rstudioSnippetsFilePath, "':\n",
        paste0(paste0("-", snippetsToCopy), collapse = "\n")
      ))
      if (length(snippetsNotToCopy) > 0) {
        cat(paste0(
          "\n(The following snippets will NOT be added because there is already a snippet with that name:\n",
          paste0(snippetsNotToCopy, collapse = ", "), ")"
        ))
      }
      answer <- readline(prompt = "Do you want to procedd (y/n): ")
      if (substr(answer, 1, 1) == "n") {
        next()
      }
    }

    # Create list of line numbers where snippet definitons start
    # This list is used to determine the end of each definition block
    #
    allPckgSnippetDefinitonStarts <- grep("^snippet .*", pckgSnippetsFileContentSanitized)

    for (s in snippetsToCopy) {
      startLine <- grep(paste0("^", s, ".*"), pckgSnippetsFileContentSanitized)

      # Find last line of snippet definition:
      # First find start of next defintion and return
      # previous line number or lastline if already in last definiton
      #
      endLine <- allPckgSnippetDefinitonStarts[allPckgSnippetDefinitonStarts > startLine][1] - 1
      if (is.na(endLine)) {
        endLine <- length(pckgSnippetsFileContentSanitized)
      }

      snippetText <- paste0(pckgSnippetsFileContentSanitized[startLine:endLine], collapse = "\n")

      # Make sure there is at least one empty line between entries
      #
      if (tail(readLines(rstudioSnippetsFilePath, warn = FALSE), n = 1) != "") {
        snippetText <- paste0("\n", snippetText)
      }

      # Append snippet block, print message
      #
      cat(paste0(snippetText, "\n"), file = rstudioSnippetsFilePath, append = TRUE)
      cat(paste0("* Added '", s, "' to '", rstudioSnippetsFilePath, "'\n"))
      added <- TRUE
    }
  }

  if (added) {
    cat("Restart RStudio to use new snippets")
  }

  return(invisible(added))
}
