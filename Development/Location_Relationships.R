# Test the relationship between the number of PBDB occurrences and the GDP and population of U.S. states.

library("RCurl")

options(timeout=600)
FossilURL<-"https://paleobiodb.org/data1.2/occs/list.csv?base_name=eukaryota&show=loc,strat,lith,lithext,geo"
GotURL<-getURL(FossilURL)
FossilsFrame<-read.csv(text=GotURL,header=TRUE)

# Create a matrix of the number of PBDB occurrences per state
StateOccurrences<-as.matrix(table(FossilsFrame[,"state"]))

# Load U.S. state GDP and population data
StatePopGDP<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/state2015_pop_gdp.csv")
colnames(StatePopGDP)[1]<-"Pop2015"
colnames(StatePopGDP)[2]<-"GDP2015"

