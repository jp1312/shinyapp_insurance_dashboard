message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))


# set working directory as the directory where this file is
# setwd("C:/Users/pc/Documents/github/shinyapp_insurance_dashboard")
# could not work with old version of shiny

shiny::runApp('./shiny/')
