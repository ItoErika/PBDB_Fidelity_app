
# Number of documents app was run on
length(unique(OutputMaster[,"docid"])) # 4,468





# Extract sedimentary units from the Macrostrat API
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
UnitURL<-RCurl::getURL(UnitsURL)
UnitsFrame<-read.csv(text=UnitURL, header=TRUE)

# Download data for geologic formations from the Macrostrat database API
FormationsURL<-"https://macrostrat.org/api/defs/strat_names?rank=fm&format=csv"
FormationsURL<-RCurl::getURL(FormationsURL)
FormationsFrame<-read.csv(text=FormationsURL, header=TRUE)

# Download Concept ids
ConceptURL<-"https://macrostrat.org/api/defs/strat_names?all&format=csv"
ConceptURL<-RCurl::getURL(ConceptURL)
ConceptKey<-unique(read.csv(text=ConceptURL,header=TRUE)[,c("strat_name_id","concept_id")])

# Subset UnitsFrame to only include formations
FormationData<-subset(UnitsFrame, UnitsFrame[,"strat_name_long"]%in%FormationsFrame[,"strat_name_long"])
# Find the number of unique sedimentary units 
FormationData<-merge(FormationData,ConceptKey,by.x="strat_name_id",by.y="strat_name_id",all.x=TRUE)
