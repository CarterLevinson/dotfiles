# vim: set ft=r:

.First <- function() {
  options(repos = structure(c(CRAN="http://cran.rstudio.com/")))
  options(browser = Sys.getenv("BROWSER"))
  options(pdfviewer = Sys.getenv("VIEWER"))
  options(editor = Sys.getenv("VISUAL"))
  options(pager = c("less", "-R"))
  options(width = 250)
  if (interactive()) {
    options(run.main = FALSE)
  }
  options(defaultPackages = c(getOption("defaultPackages"), ""))
}

ls_packages <- function() {
  packages <- as.data.frame(installed.packages()[, c(1, 3)])
  print(packages)
}

render_plot <- function(x, y = NULL, type = "p", main = NULL, ...) {
  dots <- list(...)
  png(filename = "/tmp/plot.png")
  plot(x = x, y = y, type = type, main = main)
  dev.off()
  system("kitty +kitten icat /tmp/plot.png")
}
