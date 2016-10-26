# Load libraries
library("RCurl")

# Download all marine, sedimentary unit names from Macrostrat Database
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&environ_class=marine&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

# Subset UnitsFrame to only include units from UnitHitData
MatrixUnits<-unique(UnitHitData[,"UnitName"])
SubsetUnitsFrame<-UnitsFrame[which(UnitsFrame[,"strat_name_long"]%in%MatrixUnits),]

# Create a vector of lithology categories from SubsetUnitsFrame
Lithologies<-(c("amphibolite","ash","andesite","argillite","arkose","basalt","breccia","chalk","chert","clay","coal","conglomerate",
"dacite","diamictite","dolomite","gabbro","gneiss","gravel","graywacke","greywacke","evaporite","lignite","limestone","marble",
"marl","mudstone","oolitic limestone","phosophorite","phyllite","quartzite","rhyolite","sand","sandstone","schist","shale",
"siliciclastic","silt","siltstone","silty clay","silty sand","skeletal silt","slate","tuff","volcanic"))

# Create a matrix showing whether or not each lithology category corresponds with each row of SubsetUnitsFrame[,"lith"]
LithMatrix<-sapply(Lithologies,function(x,y) grepl(x,y,ignore.case=FALSE, perl = TRUE),SubsetUnitsFrame[,"lith"])

# Bind the LithMatrix to the "strat_name_long" column of SubsetUnitsFrame
# NOTE: this will convert logical data in LithMatrix to numerical data
LithMatrix<-cbind(SubsetUnitsFrame[,"strat_name_long"],LithMatrix)


# Create a community matrix of samples v. species, using elements within one of the PBDB columns
# (e.g., geoplate, early_interval) as the definition of a sample. This is a presence-absence matrix.
presenceMatrix<-function(DataPBDB,Rows="geoplate",Columns="genus") {
  FinalMatrix<-matrix(0,nrow=length(unique(DataPBDB[,Rows])),ncol=length(unique(DataPBDB[,Columns])))
  rownames(FinalMatrix)<-unique(DataPBDB[,Rows])
  colnames(FinalMatrix)<-unique(DataPBDB[,Columns])
  ColumnPositions<-match(DataPBDB[,Columns],colnames(FinalMatrix))
  RowPositions<-match(DataPBDB[,Rows],rownames(FinalMatrix))
  Positions<-cbind(RowPositions,ColumnPositions)
  FinalMatrix[Positions]<-1
  return(FinalMatrix)
  }
