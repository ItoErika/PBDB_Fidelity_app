# Custom functions are camelCase. Arrays, parameters, and arguments are PascalCase
# Dependency functions are not embedded in master functions, and are marked with the flag dependency in the documentation
# []-notation is used wherever possible, and $-notation is avoided.

######################################### Load Required Libraries ###########################################
# Install libraries if necessary and load them into the environment
if (suppressWarnings(require("RCurl"))==FALSE) {
    install.packages("RCurl",repos="http://cran.cnr.berkeley.edu/");
    library("RCurl");
    }

if (suppressWarnings(require("velociraptr"))==FALSE) {
    install.packages("velociraptr",repos="http://cran.cnr.berkeley.edu/");
    library("velociraptr");
    }

if (suppressWarnings(require("pbapply"))==FALSE) {
    install.packages("pbapply",repos="http://cran.cnr.berkeley.edu/");
    library("pbapply");
    }

if (suppressWarnings(require("RPostgreSQL"))==FALSE) {
    install.packages("RPostgreSQL",repos="http://cran.cnr.berkeley.edu/");
    library("RPostgreSQL");
    }

# Currently mac only
if (suppressWarnings(require("doParallel"))==FALSE) {
    install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/");
    library("doParallel");
    }

# Start a cluster for multicore, 3 by default 
# Can make it higher if passed as a command line argument through terminal
# CommandArgument<-commandArgs(TRUE)
# if (length(CommandArgument)==0) {
#    Cluster<-makeCluster(3)
#    } else {
#    Cluster<-makeCluster(as.numeric(CommandArgument[1]))
#    }

#############################################################################################################
######################################### DATA DOWNLOAD, FIDELITY ###########################################
#############################################################################################################
# No data download functions at this time.

############################################### Download Datasets ###########################################
# Load in the data processed from the initial Fidelity app and preparation
# This should eventually be moved to postgres tables in the long-term
FormationMatrix<-read.csv("~/Box Sync/FidelityManuscripts/NationalFidelity/May2017Files/FormationMatrix.csv",row.names=1)
FormationKey<-read.csv("~/Box Sync/FidelityManuscripts/NationalFidelity/May2017Files/FormationKey.csv",stringsAsFactors=FALSE)

# Download timescale information
Epochs<-downloadTime("international%20epochs")
Periods<-downloadTime("international%20periods")

# Download official geologic time scale colors from macrostrat
ColorsURL<-"https://macrostrat.org/api/defs/intervals?true_clors&format=csv"
GotURL<-getURL(ColorsURL)
TimeScaleColors<-read.csv(text=GotURL,header=TRUE)

# Download the concept id informaton
ConceptURL<-"https://macrostrat.org/api/defs/strat_names?all&response=long&format=csv"
ConceptURL<-RCurl::getURL(ConceptURL)
ConceptKey<-unique(read.csv(text=ConceptURL,header=TRUE)[,c("strat_name_long","strat_name_id","concept_id")])

# Redownload the initial sedimentary units frame information
UnitsURL<-"https://macrostrat.org/api/units?lith_class=sedimentary&project_id=1&response=long&format=csv"
UnitURL<-RCurl::getURL(UnitsURL)
UnitsFrame<-read.csv(text=UnitURL, header=TRUE)

#############################################################################################################
######################################### DATA FORMATTING, FIDELITY #########################################
#############################################################################################################
# Quickly create a key defining the columns of the "sub-matrices"
keyMatrix<-function(FormationMatrix) {
    ColumnTypes<-sapply(strsplit(colnames(FormationMatrix),"_"),function(x) x[[1]])
    Subsets<-unique(ColumnTypes)
    FinalMatrix<-matrix(NA,nrow=length(Subsets),ncol=2,dimnames=list(Subsets,c("Start","End")))
    for (i in 1:length(Subsets)) {
        FinalMatrix[Subsets[i],"Start"]<-head(which(ColumnTypes==Subsets[i]),1)
        FinalMatrix[Subsets[i],"End"]<-tail(which(ColumnTypes==Subsets[i]),1)
        }
    return(FinalMatrix)
    }

# Create separate sub matrices using assign
# Standard R protocol would suggest a list, but it is convenient in this case to have separate objects for each submatrix
splitMatrix<-function(FormationMatrix,MatrixKey) {
    # Change the column names
    colnames(FormationMatrix)<-sapply(strsplit(colnames(FormationMatrix),"_"),function(x) x[[2]])
    # Capitalize the rownames
    rownames(MatrixKey)<-gsub("(^[[:alpha:]])", "\\U\\1",rownames(MatrixKey), perl=TRUE)
    for (submatrix in rownames(MatrixKey)) {
        assign(submatrix,FormationMatrix[,MatrixKey[submatrix,"Start"]:MatrixKey[submatrix,"End"]],envir=.GlobalEnv)
        }
    # print the names so you know what objects were made
    print(rownames(MatrixKey))
    }

# Remove matches that do not pass the location check
checkLocation<-function(FormationKey) {
	ColLocations<-strsplit(FormationKey[,"col_locations"],", ")
	DocLocations<-strsplit(FormationKey[,"doc_locations"],", ")
	FinalVector<-vector("logical",length=nrow(FormationKey))
	for (i in 1:nrow(FormationKey)) {
		FinalVector[i]<-any(is.na(match(ColLocations[[i]],DocLocations[[i]]))!=TRUE)
		}
	FormationKey<-FormationKey[which(FinalVector),]
	return(FormationKey)
	}
				      
################################################## Format Data ##############################################
# Bind the ConceptKey to the unit_id
ConceptKey<-merge(ConceptKey,UnitsFrame[,c("strat_name_id","unit_id")],by="strat_name_id",all=FALSE)
ConceptKey<-apply(ConceptKey,1,function(x) c(x["unit_id"],paste(x[c("strat_name_long","concept_id")],collapse="|")))
ConceptKey<-transform(as.data.frame(t(ConceptKey),stringsAsFactors=FALSE),unit_id=as.numeric(unit_id))

# Merge the ConceptKey and FormationMatrix
FormationMatrix<-transform(merge(FormationMatrix,ConceptKey,by.x="row.names",by.y="unit_id",all.x=TRUE),Row.names=NULL,row.names=Row.names) 
				      
# Collapse multiple macrostrat units with the same name into one row of the attributes matrix
FormationMatrix<-by(FormationMatrix[,1:ncol(FormationMatrix)-1],FormationMatrix[,"V2"],function(x) apply(x,2,max))
FormationMatrix<-do.call(rbind,FormationMatrix)
    
# Create the sub-matrix key
MatrixKey<-keyMatrix(FormationMatrix)
# Split Formation matrix into distinct units
splitMatrix(FormationMatrix,MatrixKey)
		    
# Check to see if the location of a macrostrat column is mentioned in the document
FormationKey<-checkLocation(FormationKey)
# Attach the concept_id names to make the FormationKey compatible with the Formation Matrix
FormationKey<-merge(FormationKey,ConceptKey,by="unit_id")
		    
# Break down the Formation Key into the different categories (no fossils, pbdb fossils, gdd fossils)
FossilsNA<-subset(FormationKey,FormationKey[,"GDD_occ"]!=TRUE & FormationKey[,"PBDB_occ"]!=TRUE)
FossilsPBDB<-subset(FormationKey,FormationKey[,"GDD_occ"]!=TRUE & FormationKey[,"PBDB_occ"]==TRUE)
FossilsGDD<-subset(FormationKey,FormationKey[,"GDD_occ"]==TRUE & FormationKey[,"PBDB_occ"]!=TRUE)
		    
#############################################################################################################
########################################### MAKE BARPLOT, FIDELITY ##########################################
#############################################################################################################
# No functions at this time
		    
################################################# Make Figures ##############################################
# subset TimeScaleColors to only include Epochs
EpochColors<-subset(TimeScaleColors,TimeScaleColors[,"name"]%in%rownames(Epochs))
# extract only name and color columns from EpochColors
EpochColors<-EpochColors[,c("name","color")]
# create a color palette of colors for each epoch
EpochColors<-setNames(as.character(EpochColors[,"color"]),EpochColors[,"name"])
	
# subset TimeScaleColors to only include Periods
# PeriodColors<-subset(TimeScaleColors,TimeScaleColors[,"name"]%in%rownames(Periods))
# extract only name and color columns from EpochColors
# PeriodColors<-PeriodColors[,c("name","color")]
# create a color palette of colors for each epoch
# PeriodColors<-setNames(as.character(PeriodColors[,"color"]),PeriodColors[,"name"])

# Find the age distribution of fossiliferous candidate units
EpochGDD<-Epoch[which(rownames(Epoch)%in%FossilsGDD[,"V2"]),]
EpochGDD<-apply(EpochGDD,2,sum)		    
# Divide by the total number of candidate units
EpochNA<-Epoch[which(rownames(Epoch)%in%FossilsNA[,"V2"]),]
EpochNA<-apply(EpochNA,2,sum)+EpochGDD
EpochPercent<-EpochGDD/EpochNA
		    
# Make a bar plot showing the age distribution of gdd_occs units  
quartz(height=10,width=12)
layout(matrix(c(1,1,2,2),2,2,byrow=TRUE))
par(oma=c(4,1,0.5,0),mar=c(3,3,2,0.5),mgp=c(1.5,0.5,0))
barplot(rev(EpochGDD), names.arg=rev(colnames(Epoch)),ylab="geologic formations",col=rev(EpochColors),las=2,ylim=c(0,100))
barplot(rev(EpochPercent), names.arg=rev(colnames(Epoch)),ylab="geologic formations",col=rev(EpochColors),las=2,ylim=c(0,1))
abline(h=mean(EpochPercent),col="black",lty=3,lwd=4); abline(h=mean(EpochPercent)+(2*sd(EpochPercent)),col="grey",lty=3,lwd=2); abline(h=mean(EpochPercent)-(2*sd(EpochPercent)),col="grey",lty=3,lwd=2)

#############################################################################################################
############################################# MAKE MAP, FIDELITY ############################################
#############################################################################################################
# Find which epochs/periods/stages intersect which polygons
intersectSurface<-function(Epochs,PolygonAges) {
	FinalMatrix<-matrix(NA,nrow=length(unique(PolygonAges[,"id"])),ncol=nrow(Epochs))
	rownames(FinalMatrix)<-sort(unique(PolygonAges[,"id"]),decreasing=FALSE)
	colnames(FinalMatrix)<-Epochs[,"name"]
	for (i in 1:nrow(Epochs)) {
		AgeMatch<-by(PolygonAges,PolygonAges[,"id"], function(x,t,b) any(t<x[,"best_age_bottom"] & x[,"best_age_top"] <b) | any(t==x[,"best_age_top"] & b==x[,"best_age_bottom"]),Epochs[i,"t_age"],Epochs[i,"b_age"])
		FinalMatrix[,i]<-as.numeric(do.call(c, as.list(AgeMatch)))
		}
	return(FinalMatrix)
	}

# Find the number of "valid" formations per column
validFormations<-function(Epoch,SurfaceAges,FormationColumns) {
	FinalVector<-setNames(vector("numeric",length=nrow(FormationColumns)),rownames(FormationColumns))
	for (i in 1:nrow(FormationColumns)) {
		Formations<-which(Epoch[names(which(FormationColumns[i,]==1)),]==1,arr.ind=TRUE)
		if (is.vector(Formations)!=TRUE) {Formations<-Formations[,"col"]}
		Ages<-which(SurfaceAges[i,]==1)
		FinalVector[i]<-length(unique(names(Formations[which(Formations%in%Ages)])))
		}
	return(FinalVector)
	}
			     
################################################# Make Figures ##############################################
# Establish a remote connection to teststrata
system2("ssh",c("-L 5439:127.0.0.1:5432","-N","-T","teststrata"),wait=FALSE); Sys.sleep(1);

# Establish postgresql driver
Driver<-dbDriver("PostgreSQL") # Establish database driver		    
# Establish a postgres connection
Burwell<-dbConnect(Driver, dbname = "burwell", host = "localhost", port = 5439, user = "john")
Obis<-dbConnect(Driver, dbname = "obis", host = "localhost", port = 5432, user = "zaffos")
		    
# Extract the map of macrostrat columns using the API (use rpostgis::pgGetGeom if you want to pull direct from the burwell table)
MacrostratColumns<-readOGR("https://macrostrat.org/api/columns?project_id=1&format=geojson_bare","OGRGeoJSON")

# Extract the age of each column's polygons
PolygonAges<-dbGetQuery(Burwell,"SELECT B.id,A.best_age_top, A.best_age_bottom FROM carto.large AS A JOIN macrostrat.cols AS B ON ST_Intersects(A.geom,B.poly_geom) WHERE B.project_id=1;")
# Remove the bodies without ages. Most bodies without ages are water bodies or units of explicitly unknown age.
PolygonAges<-na.omit(PolygonAges)
		    
# To end the connection, find the pid of the process
GrepResults<-system2("ps",c("ax | grep teststrata"),stdout=TRUE)
# Parse the pids from your grep into a numeric vector
Processes<-as.numeric(sub(" .*","",GrepResults)) 
# Kill all pids identified in the grep
tools::pskill(Processes)
			     
# Write PolygonAges to postgres so we don't have to keep recalculating
dbSendQuery(Obis,"DROP TABLE IF EXISTS polygon_ages CASCADE;")
dbWriteTable(Obis,"polygon_ages",value=as.data.frame(PolygonAges),row.names=FALSE)

# Find the column-polygonage relationship
SurfaceAges<-intersectSurface(Epochs,PolygonAges)
# Create a matrix of formations by column
FormationColumns<-presenceMatrix(FossilsGDD,"col_id","V2")

# Account for the surface/subsurface relationships
ColumnColors<-validFormations(Epoch,SurfaceAges,FormationColumns)
Ramp<-colorRampPalette(c("white","#CE3E3D"))

# Reproject into albers equal area
MacrostratColumns<-spTransform(MacrostratColumns,CRS("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
			     
# This deals with the ridiculous y-limit issue with the sp plot methods
Width <- MacrostratColumns@bbox[3] - MacrostratColumns@bbox[1]
Height <- MacrostratColumns@bbox[4] - MacrostratColumns@bbox[2]
Aspect <- Height / Width
# Plot the raw map
quartz(width = 10, height = 10*Aspect)
par(mar = rep(0, 4), xaxs='i', yaxs='i')
plot(MacrostratColumns,col=Ramp(max(ColumnColors)+1)[ColumnColors+1],lwd=0.5)	
			     
#############################################################################################################
############################################ MAKE 3D-MAP, FIDELITY ##########################################
#############################################################################################################
# Create the age by column matrix
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
			     
# Add the color hexcodes to the column matrix
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
ColorMatrix<-matrix(data=NA, nrow=length(Bins), ncol=length(col_ids))
for(i in 1:length(Bins)){
    ColorMatrix[i,]<-gsub("1", BinColors[i], TempMatrix[i,])
    }
     
# Download North American Macrostrat column data
MacrostratColumns<-readOGR("https://macrostrat.org/api/columns?format=geojson_bare&project_id=1")
			     
# Make the plot
plot3D<-function(MacrostratColumns,ColorMatrix) {
	Width <- MacrostratColumns@bbox[3] - MacrostratColumns@bbox[1]
	Height <- MacrostratColumns@bbox[4] - MacrostratColumns@bbox[2]
	Aspect <- Height / Width
	for (i in 1:nrow(ColorMatrix)) {
		jpeg(sprintf("time_%03d.jpeg",i),width=10, height=10*Aspect)
		par(mar = rep(0, 4), xaxs='i', yaxs='i')
		plot(MacrostratColumns,col=ColorMatrix[i,],lwd=0.5)
		dev.off()
		}
	}		

################################################# Make Figures ##############################################			     
# Extract the map of macrostrat columns using the API (use rpostgis::pgGetGeom if you want to pull direct from the burwell table)
MacrostratColumns<-readOGR("https://macrostrat.org/api/columns?project_id=1&format=geojson_bare","OGRGeoJSON")
# Transform it to albers equal area north america projection
MacrostratColumns<-spTransform(MacrostratColumns,CRS("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
			     			     
			     
#############################################################################################################
############################################# MAKE DCA, FIDELITY ############################################
#############################################################################################################
# No functions at this time
		    
################################################# Make Figures ##############################################
                        
