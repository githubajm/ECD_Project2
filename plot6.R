# ----------------------------------
# Adam Moses
# Exploratory Data Analysis - coursera
# Project 2
# ----------------------------------

plot6 <- function(
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
     subNEIBaltimore <- subset(NEI, (year >= 1999) 
                      & (year <= 2008) 
                      & (fips == "24510")
                      & (SCC %in% usefulSCCCodes))
     
     # create a subset of the NEI for 1999 to 2008 that occured
     # in Los Angeles (fips 06037) and match an SCC code from the codes 
     # gathered for motor vehicles
     subNEILosAngeles <- subset(NEI, (year >= 1999) 
                               & (year <= 2008) 
                               & (fips == "06037")
                               & (SCC %in% usefulSCCCodes))
          
     # setup the years of interest vector
     yearsOfInterest <- unique(NEI$year)
     
     # create a vector for the emission totals
     emissionTotalsBaltimore <- c()
     emissionTotalsLosAngeles <- c()

     # iterate through the years
     for (curYear in yearsOfInterest)
     {
          # for each year get the sum of the emissions and add it to the vector
          # do this for Baltimore and Los Angeles
          emissionTotalsBaltimore <- c(emissionTotalsBaltimore, 
                              sum(subset(subNEIBaltimore, 
                                         year == curYear)[,4]))
          emissionTotalsLosAngeles <- c(emissionTotalsLosAngeles, 
                                       sum(subset(subNEILosAngeles, 
                                                  year == curYear)[,4]))          
     }
     
     # use library quantmod to calculate percent change year to year
     # for each city
     changeBaltimore  <- ((emissionTotalsBaltimore - emissionTotalsBaltimore[1]) 
     / emissionTotalsBaltimore[1]) * 100.0
     changeLosAngeles  <- ((emissionTotalsLosAngeles - emissionTotalsLosAngeles[1]) 
                          / emissionTotalsLosAngeles[1]) * 100.0

     # set the NA's to zero
     changeBaltimore[is.na(changeBaltimore)] <- 0.0
     changeLosAngeles[is.na(changeLosAngeles)] <- 0.0
     
     # recast from the quantmod object
     changeBaltimore <- as.numeric(changeBaltimore)
     changeLosAngeles <- as.numeric(changeLosAngeles)
     
     # create the city name vector
     cityVec <- c()
     cityVec[1:4] <- "Baltimore City"
     cityVec[5:8] <- "Los Angeles County"

     # rebuild the collected data in a data frame
     usefulData <- data.frame(
                         year = c(yearsOfInterest, yearsOfInterest),
                         city = cityVec,
                         total = c(emissionTotalsBaltimore, emissionTotalsLosAngeles),
                         change = c(changeBaltimore, changeLosAngeles)
                         )
     
     # plot the percent change data by year seperated by location
     kPlot <- qplot(
          year,
          change,
          col = city,
          data = usefulData, 
          geom = c("line", "point"),
          xlab = "Year",
          ylab = "Percent Change"
     )
     
     # add a title and legend title
     kPlot <- kPlot + ggtitle("PM2.5 Motor Vehicle Emission Percent Change Since 1999\n(Baltimore City vs. Los Angeles County)")
     kPlot <- kPlot + labs(color = "Location")
     
     # display the plot
     print(kPlot)
     
     # write to the file if flagged to
     if (writeToFileNotScreen)
     {
          dev.print(
               file="plot6.png", 
               device = png, 
               width = 800, 
               height = 550
          )    
     }

     return(1)
}