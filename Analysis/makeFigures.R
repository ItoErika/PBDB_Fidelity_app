# Custom functions are camelCase. Arrays, parameters, and arguments are PascalCase
# Dependency functions are not embedded in master functions, and are marked with the flag dependency in the documentation
# []-notation is used wherever possible, and $-notation is avoided.

######################################### Load Required Libraries ###########################################
# Install libraries if necessary and load them into the environment
if (suppressWarnings(require("RCurl"))==FALSE) {
    install.packages("RCurl",repos="http://cran.cnr.berkeley.edu/");
    library("RCurl");
    }

if (suppressWarnings(require("RJSONIO"))==FALSE) {
    install.packages("RJSONIO",repos="http://cran.cnr.berkeley.edu/");
    library("RJSONIO");
    }

if (suppressWarnings(require("stringdist"))==FALSE) {
    install.packages("stringdist",repos="http://cran.cnr.berkeley.edu/");
    library("stringdist");
    }

# Currently mac only
if (suppressWarnings(require("doParallel"))==FALSE) {
    install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/");
    library("doParallel");
    }

if (suppressWarnings(require("plyr"))==FALSE) {
    install.packages("plyr",repos="http://cran.cnr.berkeley.edu/");
    library("plyr");
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
FormationKey<-read.csv("~/Box Sync/FidelityManuscripts/NationalFidelity/March2017Files/matchdata_master.csv",row.names=1)

#############################################################################################################
######################################### DATA ANALYSIS, FIDELITY ###########################################
#############################################################################################################
# Quickly create a key defining the columns of the "sub-matrices"
keyMatrix<-function(FormationMatrix) {
    ColumnTypes<-sapply(strsplit(colnames(FormationMatrix),"_"),function(x) x[[1]])
    Subsets<-unique(ColumnTypes)
    FinalMatrix<-matrix(NA,nrow=length(Subsets),ncol=2,dimnames=list(Subsets,c("Start","End")))
    for (i in 1:length(Subsets)) {
        FinalMatrix[Subsets[i],"Start"]<-head(which(ColumnNames==Subsets[i]),1)
        FinalMatrix[Subsets[i],"End"]<-tail(which(ColumnNames==Subsets[i]),1)
        }
    return(FinalMatrix)
    }

# Create separate sub matrices using assign
# Standard R protocal would suggest a list, but it is convenient in this case to have separate objects for each submatrix
splitMatrix<-function(FormationMatrix,MatrixKey) {
    # Capitalize the rownames
    rownames(MatrixKey)<-gsub("(^[[:alpha:]])", "\\U\\1",rownames(MatrixKey), perl=TRUE)
    for (submatrix in rownames(MatrixKey)) {
        assign(submatrix,FormationMatrix[,MatrixKey[submatrix,"Start"]:MatrixKey[submatrix,"End"]],envir=.GlobalEnv)
        }
    # print the names so you know what objects were made
    print(rownames(MatrixKey))
    }

################################################## Analyze Data #############################################
# Create a histogram of the age distribution
