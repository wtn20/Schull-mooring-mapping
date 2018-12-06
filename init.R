#list.of.packages <- c("shiny", "leaflet", "RColorBrewer", "readxl", "tidyverse", "glue")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

my_packages = c("shiny", "leaflet", "RColorBrewer", "readxl", "tidyverse", "glue")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))