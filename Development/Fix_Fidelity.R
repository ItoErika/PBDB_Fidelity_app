# Methods and Functions are camelCase. Variables and Data Structures are lower snake_case
# Fields generally follow snake_case for better SQL compatibility
# Dependency functions are not embedded in master functions
# []-notation is used wherever possible, and $-notation is avoided.
# []-notation is slower, but more explicit and works for atomic vectors

######################################### Load Required Libraries ###########################################
# Increase the timeout time and change the fancyquote settings
options(timeout=600, "useFancyQuotes"=FALSE)

# Install the readr package to load in the data, this was Simon Goring's idea
if (suppressWarnings(require("readr"))==FALSE) {
        install.packages("reader",repos="http://cran.cnr.berkeley.edu/");
        library("readr");
        }

# If running from UW-Madison
# Load or install the doParallel package
#if (suppressWarnings(require("doParallel"))==FALSE) {
#        install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/");
#        library("doParallel");
#        }

# Install the geocarrot package from github
if (suppressWarnings(require("geocarrot"))==FALSE) {
        devtools::install_github("aazaff/geocarrot");
        library("geocarrot");
        }
             
#############################################################################################################
######################################## LOAD DATA FUNCTIONS, FIDELITY ######################################
#############################################################################################################
# No functions at this time

#####################################$#### LOAD DATA SCRIPT, FIDELITY ##########$############################
# Load in test documents from the geocarrot package
# I don't like this because it is implicit on lazy_loading, which I find wasteful, but seems to be the industry norm
docs<-geocarrot::usgs_gdd

# Download North American occurrences data from the Paleobiology Database
pbdb_occs<-read.csv("https://paleobiodb.org/data1.2/occs/list.csv?&cc=NOA",stringsAsFactors=FALSE)

# Download geologic unit data from the Macrostrat database. 
# Extract sedimentary units from the Macrostrat API which do not have fossils reported in the Paleobiology Database.
units_frame<-read.csv("https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv",stringsAsFactors=FALSE) 

# Download data for geologic formations from the Macrostrat database API
strat_names<-read.csv("https://macrostrat.org/api/defs/strat_names?&format=csv", header=TRUE, stringsAsFactors=FALSE)

# Download geologic time scale data from the Macrostrat API
intervals_frame<-read.csv("https://macrostrat.org/api/defs/intervals?all&format=csv", header=TRUE, stringsAsFactors=FALSE)       

# Download sedimentary lithologies from Macrostrat
sed_liths<-read.csv("https://macrostrat.org/api/v1/defs/lithologies?lith_class=sedimentary&format=csv")

#############################################################################################################
####################################### CLEAN DATA FUNCTIONS, FIDELITY ######################################
#############################################################################################################
# No functions at this time

######################################### CLEAN DATA SCRIPT, FIDELITY #######################################
# Subset only members, formation, and group
strat_names<-subset(strat_names, strat_names[,"rank"]%in%c("Mbr", "Fm", "Gp")) 

# Remove ambiguoulsy named formations from units_frame and strat_names
units_frame<-units_frame[-which(units_frame[,"strat_name_long"]=="Muddy Sandstone"|units_frame[,"strat_name_long"]=="Mutual Formation"|units_frame[,"strat_name_long"]=="Sandy Limestone"),]
strat_names<-strat_names[-which(strat_names[,"strat_name_long"]=="Muddy Sandstone"|strat_names[,"strat_name_long"]=="Mutual Formation"|strat_names[,"strat_name_long"]=="Sandy Limestone"),]
# second, subset UnitsFrame to only include members, formations, and groups                               
units_frame<-subset(units_frame, units_frame[,"strat_name_long"]%in%strat_names[,"strat_name_long"])

# Third, remove Precambrian units from 
# Make sure the top age of the formations are less than the max age (less than the Cambrian-Proterozoic boundary age)
units_frame<-units_frame[which(units_frame[,"t_int_age"]<541),]

#############################################################################################################
###################################### EXTRACT TUPLES FUNCTIONS, FIDELITY ###################################
#############################################################################################################
# No functions at this time

###################################### EXTRACT TUPLES SCRIPT, FIDELITY ######################################
# Start a cluster for multicore, 2 by default or higher if passed as command line argument
#CommandArgument<-commandArgs(TRUE)
#if (length(CommandArgument)==0) {
#        Cluster<-makeCluster(2)
#        } else {
#        Cluster<-makeCluster(as.numeric(CommandArgument[1]))
#        }

# Run findPoses on docs                            
proper_nouns<-apply(docs,1,geocarrot::consecutivePoses,"NNP")
# Remove rows where there were no clusters found and bind into a matrix
proper_nouns<-na.omit(do.call(rbind,proper_nouns))
# Convert Clusters into a data frame and change the format of sentid to be numeric
proper_nouns<-as.data.frame(proper_nouns, stringsAsFactors=FALSE)
proper_nouns[,"sentid"]<-as.numeric(as.character(proper_nouns[,"sentid"]))
                            
# Find adjective and noun pairs
adjective_noun<-apply(docs, 1, findParent,"amod")
# Remove rows where there were no linked words
adjective_noun<-na.omit(do.call(rbind,adjective_noun))
# Convert LinkedWords into a data frame and change the format of sentid to be numeric
adjective_noun<-as.data.frame(adjective_noun, stringsAsFactors=FALSE)
adjective_noun[,"sentid"]<-as.numeric(as.character(adjective_noun[,"sentid"]))

# Search for the word "formation" in the proper noun clusters
fm_clusters<-proper_nouns[grep(proper_nouns[,"NNP"], pattern=" formation", perl=TRUE, ignore.case=TRUE),]
# Search for the word "fossil" in the proper noun clusters
fossil_adj<-adjective_noun[grep(adjective_noun[,"child"], pattern="fossil", perl=TRUE, ignore.case=TRUE),]
           
                           
                                
 






# (1) geologic units without fossils, (2) geologic units with fossils, (3) the first two dictionaries combined
# Extract the short version of geologic unit names from the mbr, fm, and gp columns in SubsetUnits
UnitsTemp<-SubsetUnits[,c("Mbr","Fm","Gp")]
ShortNames<-apply(UnitsTemp, 1, function(x) which(x!=""))
ShortNames<-vector(mode="character", length=nrow(SubsetUnits))
for (i in 1:nrow(SubsetUnits)){
        ShortNames[i]<-as.character(UnitsTemp[i,which(UnitsTemp[i,]!="")])[1]  
        }
# Bind ShortNames into the SubsetUnits data frame
SubsetUnits[,"strat_name_short"]<-ShortNames                     
                                                   
# Convert the strat_name_long column of formation units to character
SubsetUnits[,"strat_name_long"]<-as.character(SubsetUnits[,"strat_name_long"])

# Take sum of pbdb_collections values associated with each strat name 
CollectionsL<-tapply(SubsetUnits[,"pbdb_collections"], SubsetUnits[,"strat_name_long"], sum)
CollectionsS<-tapply(SubsetUnits[,"pbdb_collections"], SubsetUnits[,"strat_name_short"], sum)
# Extract the units with a sum of zero pbdb_collections (units with no fossil occurrences according to PBDB)              
NOPBDBL<-names(which(CollectionsL==0))
NOPBDBS<-names(which(CollectionsS==0))
# Extract the units with a at least one pbdb collection record
PBDBL<-names(which(CollectionsL>0))
PBDBS<-names(which(CollectionsS>0))

# Bind all long unit names together to make a long name dictionary
AllUnitsL<-c(NOPBDBL,PBDBL)
# Bind all short unit names together to make a short name dictionary
AllUnitsS<-c(NOPBDBS,PBDBS)                            
                                                                              
# Search for formal geologic unit names in the proper noun clusters                           
UnitHitsS<-sapply(AllUnitsS, function(x,y) agrep(x,y, ignore.case=TRUE, max.distance=0.2), Clusters[,"Proper"])                               
