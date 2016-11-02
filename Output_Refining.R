# Load output data from application
OutputData<-readRDS("~/Documents/DeepDive/PBDB_Fidelity/output_11_2_2016/Fidelity_OutputData.rds")

# Remove hits for microfossils and trace fossils within the output sentences
Micro<-grep("microfossil", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Trace<-grep("trace fossil", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
# Remove words or phrases that are likely to cause reading errors creating false hits
NoFossils<-grep(" no fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
LackFossils<-grep("lacks fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
LackOfFossils<-grep("lack of fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
AbsentFossils<-grep("absence of fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
VoidFossils<-grep("void of fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Correlative<-grep("correlative", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Equivalent<-grep("equivalent", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Above<-grep("above", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Below<-grep("below", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE) 
UnwantedRows<-unique(c(Micro,Trace,NoFossils,LackFossils,LackOfFossils,AbsentFossils,VoidFossils,Correlative,Equivalent,Above,Below))
                         
CleanedOutput<-OutputData[-UnwantedRows,]
                         
# Take a random sample of 100 Stage1Output Rows to check accuracy
# CleanedSampleOutput2<-CleanedOutput[sample(c(1:nrow(CleanedOutput)),100,replace=FALSE),]


# Save SampleOutput1 to a folder
# write.csv(CleanedSampleOutput2,file="~/Documents/DeepDive/PBDB_Fidelity/R/CleanedSampleOutput2.csv",row.names=FALSE)
# Open the csv in excel or libre office and perform a manual accuracy test
# Renamed "CleanedSampleOutput2_Completed.csv"                   

    
