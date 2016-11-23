# Goal: Find the units in the output which only have one hit in the GeoDeepDive documents
# REMEMBER: output cleaning step prior to extracting any units.

# Make a table showing the number of hits for each unit in CleanedOuput
UnitTable<-table(CleanedOutput[,"strat_name_long"])
# Extract the rows from CleanedOuptut with units that only have one hit
OneHitOutput<-CleanedOutput[which(CleanedOutput[,"strat_name_long"]%in%names(which(UnitTable==1))),]
# Take a random sample of 100 rows from OneHitOutput
Sample<-unique(OneHitOutput[,c("strat_name_long","DocID","Sentence")])
UnitsSample<-Sample[sample(c(1:nrow(Sample)),100,replace=FALSE),]
# Save the sample as a csv
write.csv(UnitsSample,file="~/Documents/DeepDive/PBDB_Fidelity/R/UnitsSample.csv",row.names=FALSE)
