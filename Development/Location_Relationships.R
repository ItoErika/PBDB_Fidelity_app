# Test the relationship between the number of PBDB occurrences and the GDP and population of U.S. states.

library("RCurl")

options(timeout=600)
FossilURL<-"https://paleobiodb.org/data1.2/occs/list.csv?base_name=eukaryota&show=loc,strat,lith,lithext,geo"
GotURL<-getURL(FossilURL)
FossilsFrame<-read.csv(text=GotURL,header=TRUE)

# Create a matrix of the number of PBDB occurrences per state
StateOccurrences<-as.matrix(table(FossilsFrame[,"state"]))
# Create a state column
state<-rownames(StateOccurrences)
StateOccurrences<-cbind(state,StateOccurrences)
colnames(StateOccurrences)[2]<-"NumOccurrences"

# Load U.S. state GDP and population data
StatePopGDP<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/state2015_pop_gdp.csv")
colnames(StatePopGDP)[1]<-"state"
colnames(StatePopGDP)[2]<-"Pop2015"
colnames(StatePopGDP)[3]<-"GDP2015"

# Merge population, GDP, and occurrences data
StateData<-merge(StatePopGDP, StateOccurrences, by="state", all.x=TRUE)

# Load Macrostrat data
# Download all marine, sedimentary unit names from Macrostrat Database
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&environ_class=marine&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

# Load intersected location tuples table 
LocationTuples<-dbGetQuery(Connection,"SELECT* FROM column_locations.intersections") 

# merge the Location to UnitsFrame by col_id
UnitsFrame<-merge(UnitsFrame,LocationTuples,by="col_id", all.x=TRUE)

# Create a matrix of the number of marine/sedimentary units in each U.S. state
StateMarineUnits<-as.matrix(table(UnitsFrame[,"location"]))
# Create a state column for the matrix
state<-rownames(StateMarineUnits)
# Bind the column to the matrix
StateMarineUnits<-cbind(state,StateMarineUnits)
colnames(StateMarineUnits)[2]<-"NumMarineUnits"

# Merge population, GDP, occurrences, and marine units data
merge(StateData, StateMarineUnits, by="state", all.x=TRUE)
