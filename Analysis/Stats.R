# Download output
stats_master<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Final_Outputs/fidelity_13Mar2017_results/Master/stats_master.csv")

# Total size of the GDD corpus at time of model run
# "In the 2.7-2.8 mil range. I'd say closer to 2.7" - Ian

# Number of documents app was run on
sum(stats_master[which(stats_master[,"StepDescription"]=="Initial Data"),"NumberDocuments"]) # 76,111





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
