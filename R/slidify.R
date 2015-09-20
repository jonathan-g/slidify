#' Convert an Rmd document into HTML5
#' 
#' @param inputFile path to the Rmd file to slidify
#' @param knit_deck whether the file needs to be run through knit
#' @param return_page should the function return the payload
#' @param save_payload should the payload be saved to the slide directory
#' @param envir is the environment in which to execute code.
#' @export
slidify <- pagify <- function(inputFile, knit_deck = TRUE, 
  return_page = FALSE, save_payload = FALSE, envir = parent.frame()){
  
  ## REMOVE LINES AFTER KNITR IS UPDATED ------
  options('knitr.in.progress' = TRUE)
  on.exit(options('knitr.in.progress' = FALSE))
  ## -------------------------------------------
  
  .SLIDIFY_ENV <<- new.env()
  site = ifelse(file.exists('site.yml'), yaml.load_file('site.yml'), list())
  page = parse_page(inputFile, knit_deck, envir = envir)
  
  page = modifyList(page, as.list(.SLIDIFY_ENV))
  render_page(page, payload = list(site = site), return_page, save_payload)
}

#' Convert a directory of Rmd documents into HTML5
#' 
#' @noRd
#' @export
blogify <- function(blogDir = ".", envir = parent.frame()){
  site = yaml.load_file('site.yml')
  cwd   = getwd(); on.exit(setwd(cwd))
  setwd(blogDir)
  rmdFiles = dir(".", recursive = TRUE, pattern = '*.Rmd')
  pages = parse_pages(rmdFiles, envir = envir)
  tags = get_tags(pages)

  is_post = grepl('^posts', sapply(pages, '[[', 'link'))
  posts = pages[is_post] 
  posts = posts %|% sort_posts_on_date %|% add_next_post
  pages = c(posts, pages[!is_post])
  pages = render_pages(pages, site, tags, return_page = TRUE)
  message('Blogification Successful :-)')
  return(invisible(list(pages = pages, site = site, tags = tags)))
}

#' check that slidify libraries are installed
#' @noRd
check_slidifyLibraries <- function(pkg = 'slidifyLibraries', min_ver = 0.3){
  if (require(pkg) && packageVersion(pkg) <= min_ver){
    stop(paste("Stop! You need to update", pkg))
  }
  if (pkg != 'slidifyLibraries') check_slidifyLibraries()
  return(invisible())
}

#' Custom Knit function for RStudio
#' 
#' @noRd
#' @export
knit2slides <- function(inputFile, encoding) {
  
  # render slides
  slidify(inputFile)
  
  # indicate success and let RStudio know the path to the output file
  outputFile = sprintf("%s.html", 
                       basename(tools::file_path_sans_ext(inputFile)))
  message("Output created: ", outputFile)
}



