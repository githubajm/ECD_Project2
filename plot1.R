# ----------------------------------
# Adam Moses
# Exploratory Data Analysis - coursera
# Project 2
# ----------------------------------

plot1 <- function(
     NEI = NULL, 
     SCC = NULL, 
     writeToFileNotScreen = TRUE
     )
{
     # check that NEI and SCC  were passed in, if not load then from the source files
     if (is.null(NEI))
          NEI <- readRDS("summarySCC_PM25.rds")
     if (is.null(SCC))
          SCC <- readRDS("Source_Classification_Code.rds")
     
     # setup the years of interest vector
     yearsOfInterest <- c(1999 ,2002, 2005, 2008)
     
     # create a vector for the emission totals
     emissionTotals <- c()
     
     # iterate through the years
     for (curYear in yearsOfInterest)
     {
          # for each year get the sum of the emissions and add it to the vector
          emissionTotals <- c(emissionTotals, sum(subset(NEI, year == curYear)[,4]))
     }
     
     # create the palette object
     palCreate <- colorRampPalette(c("red", "blue"))
     
     # create the colors from the palette
     palColors = palCreate(length(yearsOfInterest))
     
     # open the png file
     if (writeToFileNotScreen)
          png("plot1.png", width = 800, height = 550)
     
     # init the graphics
     par()
     
     # make the plot years vs emission totals
     plot(
          yearsOfInterest, 
          emissionTotals, 
          type = "l", 
          xlab = "Year", 
          ylab = "Total Emissions (Tons)",
          main = "PM2.5 Emission Totals For United States",
          col = "black"
          )

     # create a slope line marking the change     
     abline(
          lsfit(yearsOfInterest, emissionTotals), 
          col = "green",
          lwd = 2)

     # draw some points denoting the data
     points(
            yearsOfInterest, 
            emissionTotals, 
            cex = 3, 
            lwd = 2,
            col = palColors
            )

     # shutoff the graphics
     if (writeToFileNotScreen)
          dev.off()

     return(1)
}