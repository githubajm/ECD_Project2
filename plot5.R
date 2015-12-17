# ----------------------------------
# Adam Moses
# Exploratory Data Analysis - coursera
# Project 2
# ----------------------------------

plot5 <- function(
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
     
     # grab the rows of the SCC realted to motor vehicles
     # NOTE: Using anything categorized as "Onroad" on the Data.Category
     # variable since this referes to all things on the road, i.e. vehicles
     # In theory other choices could have been made here in
     # terms of parsing the SCC table for what accounts for a motor
     # vehicle bust this semed most appropriate.
     subSCC <- subset(SCC, Data.Category == "Onroad")
     
     # get the unique SCC codes from the subsetted SCC table
     usefulSCCCodes <- unique(subSCC$SCC)
     
     # create a subset of the NEI for 1999 to 2008 that occured
     # in Baltimore (fips 24510) and match an SCC code from the codes 
     # gathered for motor vehicles
     subNEI <- subset(NEI, (year >= 1999) 
                      & (year <= 2008) 
                      & (fips == 24510)
                      & (SCC %in% usefulSCCCodes))
     
     # setup the years of interest vector
     yearsOfInterest <- unique(subNEI$year)
     
     # types of sources
     typesOfSources <- unique(subNEI$type)
     
     # create a vector for the emission totals
     emissionTotals <- c()
     yearsTrack <- c()
     
     # iterate through the years
     for (curYear in yearsOfInterest)
     {
          # for each year get the sum of the emissions and add it to the vector
          emissionTotals <- c(emissionTotals, 
                              sum(subset(subNEI, 
                                         year == curYear)[,4]))
          yearsTrack <- c(yearsTrack, curYear)
     }

     # rebuild the collected data in a data frame
     usefulData <- data.frame(
                         year = yearsTrack, 
                         total = emissionTotals
                         )
     
     # plot the data
     kPlot <- qplot(
                    year, 
                    total, 
                    data = usefulData, 
                    geom = c("line", "point"),
                    xlab = "Year",
                    ylab = "Total Emissions (Tons)"
                    )
     
     # add a title
     kPlot <- kPlot + ggtitle("PM2.5 Emission Totals For Baltimore City\n(Motor Vehicle Sources Only)")

     # display the plot
     print(kPlot)
          
     # write to the file if flagged to
     if (writeToFileNotScreen)
     {
          dev.print(
                    file="plot5.png", 
                    device = png, 
                    width = 800, 
                    height = 550
                    )    
     }

     return(1)
}