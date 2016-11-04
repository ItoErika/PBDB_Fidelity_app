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



Make a sample table of unique unit name matches and sentences
Sample<-unique(CleanedOutput[,c("strat_name_long","Sentence")])
                         
# Take a random sample of 100  Sample rows to check accuracy
# CleanedSampleOutput3<-Sample[sample(c(1:nrow(Sample)),100,replace=FALSE),]


# Save SampleOutput1 to a folder
# write.csv(CleanedSampleOutput3,file="~/Documents/DeepDive/PBDB_Fidelity/R/CleanedSampleOutput3.csv",row.names=FALSE)
# Open the csv in excel or libre office and perform a manual accuracy test
# Renamed "CleanedSampleOutput2_Completed.csv"                   

    
