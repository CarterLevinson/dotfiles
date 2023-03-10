# vim: set ft = R
.First <- function() {
  options(repos=structure(c(CRAN="http://cran.rstudio.com/")))
  options(browser=Sys.getenv("BROWSER"))
  options(pdfviewer=Sys.getenv("VIEWER"))
  options(editor=Sys.getenv("VISUAL"))
  options(pager=c("less", "-R"))
  options(width=250)
  options(defaultPackages=c(getOption("defaultPackages"), "data.table"))
}

list_installed_packages <- function() {
  package_list <- as.data.frame(installed.packages()[, c(1, 3)])
  print(package_list)
}

render_plot <- function(x, y=NULL, type = "p", log = "", main = NULL) {
  png(filename = "/tmp/plot.png")
  plot(x = x, y = y, type = type,main = main)
  dev.off()
  system("kitty +kitten icat /tmp/plot.png")
}
