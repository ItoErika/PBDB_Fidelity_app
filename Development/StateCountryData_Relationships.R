# Test the relationship between the number of PBDB occurrences and the GDP and population of U.S. states.

library("RCurl")

options(timeout=600)
FossilURL<-"https://paleobiodb.org/data1.2/occs/list.csv?base_name=eukaryota&show=genus,loc,strat,lith,lithext,geo"
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
StateData<-merge(StateData, StateMarineUnits, by="state", all.x=TRUE)

# Remove unwanted columns
StateData<-na.omit(StateData)
StateData<-StateData[-9,]

# Create a vector of unique references for each state
References<-as.matrix(tapply(FossilsFrame[,"reference_no"],FossilsFrame[,"state"],function(x) length(unique(x))))
# make a vector for states
state<-rownames(References)
# bind states column to References matrix 
References<-cbind(state, References)
# name columns
colnames(References)[2]<-"NumReferences"
  
# merge references data to StateData
StateData<-merge(StateData, References, by="state", all.x=TRUE)

# Create a matrix of unique genus for each state (richness)
Richness<-as.matrix(tapply(FossilsFrame[,"genus"],FossilsFrame[,"state"],function(x) length(unique(x))))
# make a vector for states
state<-rownames(Richness)  
# bind states column to Richness matrix 
Richness<-cbind(state, Richness)
# name columns
colnames(Richness)[2]<-"NumGenera"
  
# merge richness data to StateData
StateData<-merge(StateData, Richness, by="state", all.x=TRUE)
   
# Load country IHDI data 
CountryData<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/countryihdi.csv")
# change column name for merge
colnames(CountryData)[1]<-"cc"

# Create a matrix of the number of PBDB occurrences per country
CountryOccurrences<-as.matrix(table(FossilsFrame[,"cc"]))
# Create a country code column
cc<-rownames(CountryOccurrences)
CountryOccurrences<-cbind(cc,CountryOccurrences)
colnames(CountryOccurrences)[2]<-"NumOccurrences"

# Merge CountryData with CountryOccurrences
CountryData<-merge(CountryData, CountryOccurrences, by="cc", all.x=TRUE)
  
# Create a vector of unique references for each country
CReferences<-as.matrix(tapply(FossilsFrame[,"reference_no"],FossilsFrame[,"cc"],function(x) length(unique(x))))
# make a vector for country codes
cc<-rownames(CReferences)
# bind cc column to CReferences matrix 
CReferences<-cbind(cc, CReferences)
# name columns
colnames(CReferences)[2]<-"NumReferences"
  
# merge references data to StateData
CountryData<-merge(CountryData, CReferences, by="cc", all.x=TRUE)

# Create a matrix of unique genus for each country (richness)
CRichness<-as.matrix(tapply(FossilsFrame[,"genus"],FossilsFrame[,"cc"],function(x) length(unique(x))))
# make a vector for country codes
cc<-rownames(CRichness)  
# bind states column to CRichness matrix 
CRichness<-cbind(cc, CRichness)
# name columns
colnames(CRichness)[2]<-"NumGenera"
  
# merge richness data to StateData
CountryData<-merge(CountryData, CRichness, by="cc", all.x=TRUE)
