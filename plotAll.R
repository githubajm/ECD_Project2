# ----------------------------------
# Adam Moses
# Exploratory Data Analysis - coursera
# Project 2
# ----------------------------------

plotAll <- function(
     NEI = NULL, 
     SCC = NULL
     )
{
     # check that NEI and SCC  were passed in, if not load then from the source files
     message("Checking on data...")
     if (is.null(NEI))
          NEI <- readRDS("summarySCC_PM25.rds")
     if (is.null(SCC))
          SCC <- readRDS("Source_Classification_Code.rds")

     # source all the plot source files
     message("Sourcing script files...")
     source("plot1.R")
     source("plot2.R")
     source("plot3.R")
     source("plot4.R")
     source("plot5.R")
     source("plot6.R")
     
     # run each of the plots
     message("Running each plot script function...")
     plot1(NEI, SCC)
     plot2(NEI, SCC)
     plot3(NEI, SCC)
     plot4(NEI, SCC)
     plot5(NEI, SCC)
     plot6(NEI, SCC)

     message("Complete!")
     
     return(1)
}
