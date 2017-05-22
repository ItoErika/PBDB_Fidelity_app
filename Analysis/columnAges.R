# Load libraries
library("RCurl")
library("rgdal")

# Download the sedimentary units frame from Macrostrat
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

# Remove ambiguoulsy named formations from UnitsFrame and FormationsFrame
UnitsFrame<-UnitsFrame[-which(UnitsFrame[,"strat_name_long"]=="Muddy Sandstone"|UnitsFrame[,"strat_name_long"]=="Mutual Formation"|UnitsFrame[,"strat_name_long"]=="Sandy Limestone"|UnitsFrame[,"strat_name_long"]=="White Dolomite"),]
FormationsFrame<-FormationsFrame[-which(FormationsFrame[,"strat_name_long"]=="Muddy Sandstone"|FormationsFrame[,"strat_name_long"]=="Mutual Formation"|FormationsFrame[,"strat_name_long"]=="Sandy Limestone"|FormationsFrame[,"strat_name_long"]=="White Dolomite"),]

# Subset UnitsFrame to only include formations 
FormationUnits<-subset(UnitsFrame, UnitsFrame[,"strat_name_long"]%in%FormationsFrame[,"strat_name_long"])

# Remove Precambrian units from FormationUnits
# Extract the maximum age for units of interest
Max_age<-IntervalsFrame[which(IntervalsFrame[,"name"]=="Precambrian"),"t_age"]
# Make sure the top age of the formations are less than the max age (less than the Cambrian-Proterozoic boundary age)
FormationUnits<-FormationUnits[which(FormationUnits[,"t_int_age"]<Max_age),]

# Create three dictionaries: (1) formations without fossils, (2) formations with fossils, (3) the first two dictionaries combined
# Convert the strat_name_long column of formation units to character
FormationUnits[,"strat_name_long"]<-as.character(FormationUnits[,"strat_name_long"])
# Take sum of pbdb_collections values associated with each strat name 
Collections<-tapply(FormationUnits[,"pbdb_collections"], FormationUnits[,"strat_name_long"], sum)
# Extract strat names with a sum of zero pbdb_collections (units with no fossil occurrences according to PBDB)
CandidateUnits<-names(which(Collections==0))
# Extract the strat names with a at least one pbdb collection record
FossilUnits<-names(which(Collections>0))
# Bind all formations together
Formations<-c(CandidateUnits, FossilUnits)

# Subset UnitsFrame to only includes units from the Formations dictionary
SubsetUnitsFrame<-subset(UnitsFrame, UnitsFrame[,"strat_name_long"]%in%Formations)
# Extract data of interest
ColumnAges<-unique(SubsetUnitsFrame[,c("col_id", "t_int_age", "b_int_age")])

# Extract all col_ids in increasing order
col_ids<-unique(sort(ColumnAges[,"col_id"]))
# Create time bins representing millions of years ago (Cenozoic-Paleozoic)
Bins<-seq(1,541)
# Create an empty matrix with a column for each col_id and a row for each time bin
AgesMatrix<-matrix(data=NA, nrow=length(Bins), ncol=length(col_ids))
for(j in 1:length(col_ids)){
    for(i in 1:length(Bins)){
        AgesMatrix[i,j]<-as.numeric(any(ColumnAges[which(ColumnAges[,"col_id"]==col_ids[j]),"b_int_age"]>=Bins[i] & ColumnAges[which(ColumnAges[,"col_id"]==col_ids[j]),"t_int_age"]<=Bins[i]))
        }
     }

# Assign appropriate row and column names
rownames(AgesMatrix)<-1:541
colnames(AgesMatrix)<-col_ids

# Download official geologic time scale colors from macrostrat
ColorsURL<-"https://macrostrat.org/api/defs/intervals?true_clors&format=csv"
GotURL<-getURL(ColorsURL)
TimeScaleData<-read.csv(text=GotURL,header=TRUE)

# Download age names
Ages<-downloadTime("international%20ages")
# subset TimeScaleColors to only include ages
AgeData<-subset(TimeScaleData,TimeScaleData[,"name"]%in%rownames(Ages))
# Make sure AgeData is sorted in ascending age order (present to past)
AgeData<-AgeData[order(AgeData[,"t_age"]),]

# Extract appropriate hex color codes for each million year time bin (Cenozoic - Paleozoic)
# Note: the min() in the loop below accounts for ages with integer boundaries (causes more than one color code to be selected)
# By adding min(), we always select hex color code for the younger age
BinColors<-vector(length=length(Bins))
for(i in 1:length(Bins)){
    BinColors[i]<-as.character(AgeData[min(which(AgeData[,"b_age"]>=Bins[i]&AgeData[,"t_age"]<=Bins[i])),"color"])
    }

# Create a duplicate of AgesMatrix to replace with hex code color codes
TempMatrix<-AgesMatrix
# Replace each 0 in TempMatrix with the hex color code for white
TempMatrix[TempMatrix==0]<-"#FFFFFF"
# Replace each 1 in TempMatrix with the appropriate time bin color hex color code
ColoredAgesMatrix<-matrix(data=NA, nrow=length(Bins), ncol=length(col_ids))
for(i in 1:length(Bins)){
    ColoredAgesMatrix[i,]<-gsub("1", BinColors[i], TempMatrix[i,])
    }
     
# Download North American Macrostrat column data
ColumnData<-readOGR("https://macrostrat.org/api/columns?format=geojson_bare&project_id=1")
#plot(ColumnData, col=ColoredAgesMatrix[1,])
