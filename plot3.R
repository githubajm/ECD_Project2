# ----------------------------------
# Adam Moses
# Exploratory Data Analysis - coursera
# Project 2
# ----------------------------------

plot3 <- function(
     NEI = NULL, 
     SCC = NULL, 
     writeToFileNotScreen = TRUE
     )
{
     # check for required libraries
     require(ggplot2)
 
     # check that NEI and SCC  were passed in, if not load then from the source files
     if (is.null(NEI))
          NEI <- readRDS("summarySCC_PM25.rds")
     if (is.null(SCC))
          SCC <- readRDS("Source_Classification_Code.rds")
     
     # create a subset of the NEI for 1999 to 2008 for fips 24510
     subNEI <- subset(NEI, fips == "24510" & year >= 1999 & year <= 2008)
     
     # setup the years of interest vector
     yearsOfInterest <- unique(subNEI$year)
     
     # types of sources
     typesOfSources <- unique(subNEI$type)
     
     # create a vector for the emission totals
     emissionTotals <- c()
     yearsTrack <- c()
     typesTrack <- c()
     
     # iterate through the years
     for (curType in typesOfSources)
     {
          for (curYear in yearsOfInterest)
          {
               # for each year get the sum of the emissions and add it to the vector
               emissionTotals <- c(emissionTotals, 
                                   sum(subset(subNEI, 
                                              type == curType 
                                              & year == curYear)[,4]))
               yearsTrack <- c(yearsTrack, curYear)
               typesTrack <- c(typesTrack, curType)
          }
     }
     
     # rebuild the collected data in a data frame
     usefulData <- data.frame(
                         year = yearsTrack, 
                         type = typesTrack, 
                         total = emissionTotals
                         )
     
     # plot the data
     kPlot <- qplot(
                    year, 
                    total, 
                    data = usefulData, 
                    col = type,
                    geom = c("line", "point"),
                    xlab = "Year",
                    ylab = "Total Emissions (Tons)"
                    )
     
     # add a title
     kPlot <- kPlot + ggtitle("PM2.5 Emission Totals For Baltimore By Source Type")
     kPlot <- kPlot + labs(color = "Source Type")
     
     # display the plot
     print(kPlot)
          
     # write to the file if flagged to
     if (writeToFileNotScreen)
     {
          dev.print(
                    file="plot3.png", 
                    device = png, 
                    width = 800, 
                    height = 550
                    )    
     }

     return(1)
}