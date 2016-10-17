# Take a random sample of 100 FossilData Rows to check accuracy
SampleData<-FossilData[sample(c(1:nrow(FossilData)),100,replace=FALSE,prob=NULL),]
# Save SampleData to a folder
write.csv(SampleData,file="~/Documents/DeepDive/PBDB_Fidelity/R/SampleFrame.csv",row.names=FALSE)
# Open the csv in excel or libre office and perform a manual accuracy test
# Renamed "SampleFrame_Completed.csv"

# ACCURACY TEST AFTER ADDING NON CANDIDATE MACROSTRAT DICTIONARY SEARCH AND NOISY WORD REMOVAL.
# Take a random sample of 100 FinalFossilData Rows to check accuracy
SampleData<-FinalFossilData[sample(c(1:nrow(FinalFossilData)),100,replace=FALSE,prob=NULL),]
# Save SampleData to a folder
write.csv(SampleData,file="~/Documents/DeepDive/PBDB_Fidelity/R/SampleFrame2.csv",row.names=FALSE)
# Open the csv in excel or libre office and perform a manual accuracy test
# Renamed "SampleFrame2_Completed.csv"
