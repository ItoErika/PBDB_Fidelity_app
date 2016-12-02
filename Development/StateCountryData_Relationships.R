# Test the relationship between the number of PBDB occurrences and the GDP and population of U.S. states.

library("RCurl")
library("RPostgreSQL")

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

# Load U.S. state area, GDP, and population data
StateData<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/state2015_pop_gdp.csv")

# Convert state land area column from square miles into square kilometers
StateData[,2]<-StateData[,2]/0.38610

colnames(StateData)[1]<-"state"
colnames(StateData)[2]<-"LandArea.sqkm"
colnames(StateData)[3]<-"Pop2015"
colnames(StateData)[4]<-"GDP2015"

# Merge population, GDP, and occurrences data
StateData<-merge(StateData, StateOccurrences, by="state", all.x=TRUE)

# Load Macrostrat data
# Download all marine, sedimentary unit names from Macrostrat Database
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

# Load intersected location tuples table 
Driver <- dbDriver("PostgreSQL") # Establish database driver
Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")
LocationTuples<-dbGetQuery(Connection,"SELECT* FROM column_locations.intersections") 

# merge the Location to UnitsFrame by col_id
UnitsFrame<-merge(UnitsFrame,LocationTuples,by="col_id", all.x=TRUE)

# Create a matrix of the number of sedimentary units in each U.S. state
StateSedUnits<-as.matrix(table(UnitsFrame[,"location"]))
# Create a state column for the matrix
state<-rownames(StateSedUnits)
# Bind the column to the matrix
StateSedUnits<-cbind(state,StateSedUnits)
colnames(StateSedUnits)[2]<-"NumSedUnits"

# Merge population, GDP, occurrences, and sedimentary units data
StateData<-merge(StateData, StateSedUnits, by="state", all.x=TRUE)

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
  
# Add Fidelity output data to StateData
StateProportion<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/StateProportion.csv")
StateData<-merge(StateData, StateProportion, by.x="state", by.y="X", all.x=TRUE)
  
# Convert columns in state data into correct format
StateData[,"state"]<-as.character(StateData[,"state"])
StateData[,"LandArea.sqkm"]<-as.numeric(as.character(StateData[,"LandArea.sqkm"]))
StateData[,"Pop2015"]<-as.numeric(as.character(StateData[,"Pop2015"]))
StateData[,"GDP2015"]<-as.numeric(as.character(StateData[,"GDP2015"]))
StateData[,"NumOccurrences"]<-as.numeric(as.character(StateData[,"NumOccurrences"]))
StateData[,"NumSedUnits"]<-as.numeric(as.character(StateData[,"NumSedUnits"]))
StateData[,"NumReferences"]<-as.numeric(as.character(StateData[,"NumReferences"]))
StateData[,"NumGenera"]<-as.numeric(as.character(StateData[,"NumGenera"]))
StateData[,"Fidelity"]<-as.numeric(as.character(StateData[,"Fidelity"]))
StateData[,"Candidates"]<-as.numeric(as.character(StateData[,"Candidates"]))
StateData[,"Coverage"]<-as.numeric(as.character(StateData[,"Coverage"]))
  
# Add columns of PBDB and macrostrat data normalized according to area of states to StateData
StateData[,"AreaNormOcc"]<-StateData[,"NumOccurrences"]/StateData[,"LandArea.sqkm"]
StateData[,"AreaNormUnits"]<-StateData[,"NumSedUnits"]/StateData[,"LandArea.sqkm"]
StateData[,"AreaNormRef"]<-StateData[,"NumReferences"]/StateData[,"LandArea.sqkm"]
StateData[,"AreaNormGen"]<-StateData[,"NumGenera"]/StateData[,"LandArea.sqkm"]
   
# Load country IHDI data 
CountryData<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/countryihdi.csv")

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
  
# merge references data to CountryData
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
  
# Convert CountryData into a data frame
CountryData<-as.data.frame(CountryData)
CountryData[,"cc"]<-as.character(CountryData[,"cc"])
CountryData[,"ihdi"]<-as.numeric(as.character(CountryData[,"ihdi"]))
# Remove commas from area column
CountryData[,"LandArea.sqkm"]<-gsub(",","",CountryData[,"LandArea.sqkm"])
CountryData[,"LandArea.sqkm"]<-as.numeric(as.character(CountryData[,"LandArea.sqkm"]))
CountryData[,"NumOccurrences"]<-as.numeric(as.character(CountryData[,"NumOccurrences"]))
CountryData[,"NumReferences"]<-as.numeric(as.character(CountryData[,"NumReferences"]))
CountryData[,"NumGenera"]<-as.numeric(as.character(CountryData[,"NumGenera"]))

# Replace NA's with zeroes
CountryData[,c("NumOccurrences","NumReferences","NumGenera")][is.na(CountryData[,c("NumOccurrences","NumReferences","NumGenera")])]<-0
  
# Create and add area normalized PBDB data columns to CountryData
CountryData["AreaNormOcc"]<-CountryData[,"NumOccurrences"]/CountryData[,"LandArea.sqkm"]
CountryData["AreaNormRef"]<-CountryData[,"NumReferences"]/CountryData[,"LandArea.sqkm"]
CountryData["AreaNormGen"]<-CountryData[,"NumGenera"]/CountryData[,"LandArea.sqkm"]

#################################################### ANALYSIS ############################################################  

# STATE POPULATION PLOT
quartz(height=10,width=10)
layout(matrix(c(1,2,3,4),2,2))
plot(StateData[,"Pop2015"],StateData[,"NumOccurrences"],xlab="State Population in 2015",ylab="# of PBDB Occurrences",col="dodgerblue3",lwd=2,pch=16,main="PBDB Occurrences vs State Population")
plot(StateData[,"Pop2015"],StateData[,"NumReferences"],xlab="State Population in 2015",ylab="# of PBDB References",col="dodgerblue3",lwd=2,pch=16,main="PBDB References vs State Population")
plot(StateData[,"Pop2015"],StateData[,"NumGenera"],xlab="State Population in 2015",ylab="# of Genera",col="dodgerblue3",lwd=2,pch=16,main="Richness vs State Population")  
plot(StateData[,"Pop2015"],StateData[,"Coverage"],xlab="State Population in 2015",ylab="Fidelity Coverage",col="dodgerblue3",lwd=2,pch=16,main="Coverage vs State Population")  
 
# STATE GDP PLOT 
quartz(height=10,width=10)
layout(matrix(c(1,2,3,4),2,2))
plot(StateData[,"GDP2015"],StateData[,"NumOccurrences"],xlab="State GDP in 2015",ylab="# of PBDB Occurrences",col="dodgerblue3",lwd=2,pch=16,main="PBDB Occurrences vs GDP")
plot(StateData[,"GDP2015"],StateData[,"NumReferences"],xlab="State GDP in 2015",ylab="# of PBDB References",col="dodgerblue3",lwd=2,pch=16,main="PBDB References vs GDP")
plot(StateData[,"GDP2015"],StateData[,"NumGenera"],xlab="State GDP in 2015",ylab="# of Genera",col="dodgerblue3",lwd=2,pch=16,main="Richness vs GDP")
plot(StateData[,"GDP2015"],StateData[,"Coverage"],xlab="State GDP in 2015",ylab="Fidelity Coverage",col="dodgerblue3",lwd=2,pch=16,main="Coverage vs GDP") 
  
# COUNTRY IHDI PLOT
quartz(height=10,width=10)
plot(CountryData[,"ihdi"], CountryData[,"NumOccurrences"],xlab="Country IHDI",ylab="# of PBDB Occurrences",col="forest green",lwd=2,pch=16,main="PBDB Occurrences vs IHDI")
plot(CountryData[,"ihdi"], CountryData[,"NumReferences"],xlab="Country IHDI",ylab="# of PBDB References",col="forest green",lwd=2,pch=16,main="PBDB References vs IHDI")
plot(CountryData[,"ihdi"], log(CountryData[,"NumGenera"]),xlab="Country IHDI",ylab="log(# of Genera)",lwd=2,pch=16,col="forest green",main="Richness vs IHDI")
 
# ANALYZE UNNORMALIZED DATA
  
cor.test(StateData[,"GDP2015"],StateData[,"NumOccurrences"], method="spearman")
# S = 20242, p-value = 0.8228
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho: -0.0327551 
  
cor.test(StateData[,"GDP2015"],StateData[,"NumSedUnits"], method="spearman")
# S = 22397, p-value = 0.328
# alternative hypothesis: true rho is not equal to 0
#sample estimates: rho -0.1427077 
  
cor.test(StateData[,"GDP2015"],StateData[,"NumReferences"], method="spearman")
# S = 18232, p-value = 0.6337
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.06980126 
  
cor.test(StateData[,"GDP2015"],StateData[,"NumGenera"], method="spearman")
# S = 19730, p-value = 0.9641
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho -0.006632653 
  
cor.test(StateData[,"Pop2015"],StateData[,"NumOccurrences"], method="spearman")
# S = 15530, p-value = 0.1519
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.2076531 
  
cor.test(StateData[,"Pop2015"],StateData[,"SedUnits"], method="spearman")
# S = 14789, p-value = 0.08913
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.2454654 
  
cor.test(StateData[,"Pop2015"],StateData[,"NumReferences"], method="spearman")
# S = 16866, p-value = 0.3391
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.1395005 

cor.test(StateData[,"Pop2015"],StateData[,"NumGenera"], method="spearman") 
# S = 14432, p-value = 0.06739
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.2636735 
  
cor.test(CountryData[,"ihdi"], CountryData[,"NumOccurrences"], method="spearman")
# S = 286110, p-value = 5.483e-11
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho  0.5013716 
  
cor.test(CountryData[,"ihdi"], CountryData[,"NumReferences"], method="spearman")
# S = 288340, p-value = 8.11e-11
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.4974978 
  
cor.test(CountryData[,"ihdi"], CountryData[,"NumGenera"], method="spearman")
# S = 274520, p-value = 6.561e-12
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho  0.5215739 
  
# ANALYZE NORMALIZED DATA 
cor.test(StateData[,"GDP2015"],StateData[,"AreaNormOcc"], method="spearman")
# S = 18422, p-value = 0.6808
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.06010204 
  
cor.test(StateData[,"GDP2015"],StateData[,"AreaNormUnits"], method="spearman")
# S = 21002, p-value = 0.6243
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho -0.07153061 
  
cor.test(StateData[,"GDP2015"],StateData[,"AreaNormRef"], method="spearman")
# S = 14816, p-value = 0.09107
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho  0.2440816 
  
cor.test(StateData[,"GDP2015"],StateData[,"AreaNormGen"], method="spearman")
# S = 19694, p-value = 0.9741
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho  -0.004795918 
  
cor.test(StateData[,"Pop2015"],StateData[,"AreaNormOcc"], method="spearman")
# S = 13732, p-value = 0.03704
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.2993878 
  
cor.test(StateData[,"Pop2015"],StateData[,"AreaNormUnits"], method="spearman")
# S = 16532, p-value = 0.2818
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho  0.1565306 
  
cor.test(StateData[,"Pop2015"],StateData[,"AreaNormRef"], method="spearman")
# S = 18294, p-value = 0.6483
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.06663265 
  
cor.test(StateData[,"Pop2015"],StateData[,"AreaNormGen"], method="spearman") 
# S = 14012, p-value = 0.04743
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.285102 
  
cor.test(CountryData[,"ihdi"], CountryData[,"AreaNormOcc"], method="spearman")
# S = 218460, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.6192684 
  
cor.test(CountryData[,"ihdi"], CountryData[,"AreaNormRef"], method="spearman")
# S = 214300, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.6265281 
  
cor.test(CountryData[,"ihdi"], CountryData[,"AreaNormGen"], method="spearman")
# S = 227920, p-value = 2.631e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates: rho 0.6027868 
  
# Sources: 
# State Area Source: http://www.census.gov/prod/cen2010/cph-2-1.pdf
# Country Area Source: https://www.cia.gov/library/publications/the-world-factbook/fields/2147.html
