add_urls <- function(deck, pkg = 'slidifyLibraries'){
  deck$url[['lib']] = deck$url[['lib']] %||% system.file('libraries', package = pkg)
  urls = with(deck, modifyList(url, list(
    widgets    = file.path(url$lib, 'widgets'),
    framework = file.path(url$lib, 'frameworks', framework),
    theme = file.path(url$lib, 'frameworks', framework, theme),
    highlighters = file.path(url$lib, 'highlighters')
  )))
  urls$layouts = with(urls, c(
    file.path(framework, 'layouts'),
    file.path(assets, 'layouts'),
    file.path(widgets, deck$widgets, 'layouts')
  ))
  urls$highlighter = file.path(urls$highlighters, deck$highlighter)
  deck$url = urls
  return(deck)
}
