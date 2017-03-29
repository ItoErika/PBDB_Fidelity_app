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
FormationMatrix<-read.csv("~/Box Sync/FidelityManuscripts/NationalFidelity/March2017Files/FormationMatrix.csv",row.names=1)
FormationKey<-read.csv("~/Box Sync/FidelityManuscripts/NationalFidelity/March2017Files/FormationKey.csv",row.names=1)

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

# Identify 

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
# Standard R protocal would suggest a list, but it is convenient in this case to have separate objects for each submatrix
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

# For two distinct stratigraphic units (concept_id) with the same name
# perform a location check to see which unit is being referred to in the document
# This step is currently on hold while waiting for the docid_region_tuples to process from GDD

# subset TimeScaleColors to only include epochs
EpochColors<-subset(TimeScaleColors,TimeScaleColors[,"name"]%in%rownames(Epochs))
# extract only name and color columns from EpochColors
EpochColors<-EpochColors[,c("name","color")]
# create a color palette of colors for each epoch
EpochColors<-setNames(as.character(EpochColors[,"color"]),EpochColors[,"name"])
	
# subset TimeScaleColors to only include epochs
PeriodColors<-subset(TimeScaleColors,TimeScaleColors[,"name"]%in%rownames(Periods))
# extract only name and color columns from EpochColors
PeriodColors<-PeriodColors[,c("name","color")]
# create a color palette of colors for each epoch
PeriodColors<-setNames(as.character(PeriodColors[,"color"]),PeriodColors[,"name"])

# Make a bar plot showing the RAW NUMBER of units in the EpochOutputMatrix that fall into each epoch category
# take the sum of all of the columns of EpochOutputMatrix
EpochSums<-apply(EpochOutputMatrix,2,sum)
# make raw data bar plot
quartz(height=10,width=12)
layout(matrix(c(1,1,2,2),2,2,byrow=TRUE))
par(oma=c(4,1,0.5,0),mar=c(3,3,2,0.5),mgp=c(1.5,0.5,0))
barplot(EpochSums, names.arg=colnames(EpochOutputMatrix),ylab="# of Fidelity Output Units",col=colors,las=2)
abline(h=mean(EpochSums),lwd=5,col="dark grey",lty=3)
	
# make a bar plot showing the PERCENTAGE of units in EpochOutputMatrix which fall into each epoch category
# subset UnitMatrix to only include epoch columns
EpochMatrix<-UnitMatrix[,rownames(Epochs)]
# find the total number of units in UnitMatrix that fall into each epoch category
EpochTotalSums<-apply(EpochMatrix,2,sum)
# divide EpochSums by EpochTotalSums
EpochPercentages<-EpochSums/EpochTotalSums
# make percentages barplot
barplot(EpochPercentages,names.arg=colnames(EpochOutputMatrix),ylab="% of Fidelity Output Units", col=colors,las=2)
abline(h=mean(EpochPercentages),lwd=5,col="dark grey",lty=3)
	
################################################### PERIODS BAR PLOT ##################################################

Periods<-downloadTime("international%20periods")
# get rid of preCambrian rows
# make a vector of preCambrian Periods
PreCambrian<-c("Ediacaran","Cryogenian","Tonian","Stenian","Ectasian","Calymmian","Statherian","Orosirian","Rhyacian",
"Siderian")
# remove PreCambrian rows
Periods<-Periods[-which(rownames(Periods)%in%PreCambrian),]

# subset OutputUnitMatrix to only epoch columns (NOTE: rownames of Periods are period names)
PeriodOutputMatrix<-OutputUnitMatrix[,rownames(Periods)]
	
# subset TimeScaleColors to only include periods
PeriodColors<-subset(TimeScaleColors,TimeScaleColors[,"name"]%in%rownames(Periods))
# extract only name and color columns from EpochColors
PeriodColors<-PeriodColors[,c("name","color")]	
# create a color palette of colors for each period
colors<-as.character(PeriodColors[,"color"])
names(colors)<-PeriodColors[,"name"]	
	
# Make a bar plot showing the RAW NUMBER of units in the PeriodOutputMatrix that fall into each period category
# take the sum of all of the columns of PeriodOutputMatrix
PeriodSums<-apply(PeriodOutputMatrix,2,sum)
# make raw data bar plot
barplot(PeriodSums, names.arg=colnames(PeriodOutputMatrix),xlab="Period",ylab="# of Fidelity Output Units",col=colors)	
	
# make a bar plot showing the PERCENTAGE of units in PeriodOutputMatrix which fall into each period category
# subset UnitMatrix to only include period columns
PeriodMatrix<-UnitMatrix[,rownames(Periods)]
# find the total number of units in UnitMatrix that fall into each epoch category
PeriodTotalSums<-apply(PeriodMatrix,2,sum)
# divide PeriodSums by PeriodTotalSums
PeriodPercentages<-PeriodSums/PeriodTotalSums
# make percentages barplot
barplot(PeriodPercentages,names.arg=colnames(PeriodOutputMatrix),xlab="Period",ylab="% of Fidelity Output Units", col=colors)
                        
                        
