library("RCurl")

# Download output
output_master<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Final_Outputs/fidelity_13Mar2017_results/Master/output_master.csv")
stats_master<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Final_Outputs/fidelity_13Mar2017_results/Master/stats_master.csv")

# Total size of the GDD corpus at time of model run: 
# "In the 2.7-2.8 mil range. I'd say closer to 2.7" - Ian

# Number of documents app was run on:
sum(stats_master[which(stats_master[,"StepDescription"]=="Initial Data"),"NumberDocuments"]) # 76,111

# Total number of sedimentary formations in Macrostrat: 5,022
# Extract sedimentary units from the Macrostrat API which do not have fossils reported in the Paleobiology Database.
print(paste("Download Macrostrat unit and age data",Sys.time()))
# Download all sedimentary unit data from Macrostrat Database
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
UnitURL<-RCurl::getURL(UnitsURL)
UnitsFrame<-read.csv(text=UnitURL, header=TRUE)

# Download data for geologic formations from the Macrostrat database API
FormationsURL<-"https://macrostrat.org/api/defs/strat_names?rank=fm&format=csv"
FormationsURL<-RCurl::getURL(FormationsURL)
FormationsFrame<-read.csv(text=FormationsURL, header=TRUE)

# Subset the strat name long column of units frame to only include formations
SedUnits<-subset(UnitsFrame[,"strat_name_long"], UnitsFrame[,"strat_name_long"]%in%FormationsFrame[,"strat_name_long"])
length(unique(SedUnits)) # 5,022

# Total number of cleaned sedimentary formations in Macrostrat: 4,682
# Extract sedimentary units from the Macrostrat API which do not have fossils reported in the Paleobiology Database.
print(paste("Download Macrostrat unit and age data",Sys.time()))
# Download all sedimentary unit data from Macrostrat Database
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
UnitURL<-RCurl::getURL(UnitsURL)
UnitsFrame<-read.csv(text=UnitURL, header=TRUE)

# Download data for geologic formations from the Macrostrat database API
FormationsURL<-"https://macrostrat.org/api/defs/strat_names?rank=fm&format=csv"
FormationsURL<-RCurl::getURL(FormationsURL)
FormationsFrame<-read.csv(text=FormationsURL, header=TRUE)

# Download geologic time scale data from the Macrostrat API
IntervalsURL<-"https://macrostrat.org/api/defs/intervals?all&format=csv"
IntervalsURL<-RCurl::getURL(IntervalsURL)
IntervalsFrame<-read.csv(text= IntervalsURL, header=TRUE)

# First, remove ambiguoulsy named formations from UnitsFrame
UnitsFrame<-UnitsFrame[-which(UnitsFrame[,"strat_name_long"]=="Muddy Sandstone"|UnitsFrame[,"strat_name_long"]=="Mutual Formation"|UnitsFrame[,"strat_name_long"]=="Sandy Limestone"),]
# Second, remove Precambrian units from UnitsFrame
# Extract the maximum age for units of interest
Max_age<-IntervalsFrame[which(IntervalsFrame[,"name"]=="Precambrian"),"t_age"]
# Make sure the top age of the units are less than the max age (less than the Cambrian-Proterozoic boundary age)
UnitsFrame<-UnitsFrame[which(UnitsFrame[,"t_int_age"]<Max_age),]

# Create three dictionaries:
# (1) formations without fossils, (2) formations with fossils, (3) the first two dictionaries combined
# Take sum of pbdb_collections values associated with each strat name 
Collections<-tapply(UnitsFrame[,"pbdb_collections"], UnitsFrame[,"strat_name_long"], sum)
# Extract strat names with a sum of zero pbdb_collections (units with no fossil occurrences according to PBDB)
BarrenUnits<-names(which(Collections==0))
# Subset barren units to only include formations
BarrenUnits<-subset(BarrenUnits, BarrenUnits%in%FormationsFrame[,"strat_name_long"])
# Extract the strat names with a at least one pbdb collection record
FossilUnits<-names(which(Collections>0))
# Subset fossil units to only include formations
FossilUnits<-subset(FossilUnits, FossilUnits%in%FormationsFrame[,"strat_name_long"])
# Bind all candidate formations together
Formations<-c(BarrenUnits, FossilUnits)

length(unique(BarrenUnits))+length(unique(FossilUnits)) # 4,682

# Verify with stats table: 
# NOTE: must subtract one because of the post-app removal of "Sandy Limestone"
sum(unique(stats_master[which(stats_master[,"StepDescription"]=="Make dictionaries of formation names"),c("Barren_Units","Fossil_Units")]))-1
# 4,682

#Total number of non-candidate units (in PBDB):
length(FossilUnits) # 2,021

# Verify with stats table: 
unique(stats_master[which(stats_master[,"StepDescription"]=="Make dictionaries of formation names"),"Fossil_Units"])
# 2,021

# Total number of candidate units (not in PBDB):
length(BarrenUnits) # 2,661

# Verify with stats table: 
# NOTE: must subtract one because of the post-app removal of "Sandy Limestone"
unique(stats_master[which(stats_master[,"StepDescription"]=="Make dictionaries of formation names"),"Barren_Units"])-1
# 2,661

# Total number of units matched to any document (candidate and non-candidate): 1,847
# NOTE: Must remove hits for "Sandy Limestone"
length(unique(output_master[which(output_master[,"Formation"]!="Sandy Limestone"),"Formation"]))
# 1,847

# Total number of non-candidate units matched to any document: 1,115
output_master_clean<-output_master[which(output_master[,"Formation"]!="Sandy Limestone"),]
fossil_output<-output_master_clean[which(output_master_clean[,"PBDB_occ"]==TRUE),]
length(unique(fossil_output[,"Formation"]))
# 1,115

# Total number of candidate units matched to any document: 732
unfossil_output<-output_master_clean[which(output_master_clean[,"PBDB_occ"]==FALSE),]
length(unique(unfossil_output[,"Formation"]))
# 732

# Total number of candidate units not matched to any document
2661-732
#1,929

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
