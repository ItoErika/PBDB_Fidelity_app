# Load output data from application
OutputData<-readRDS("~/Documents/DeepDive/PBDB_Fidelity/output_11_18_2016/Fidelity_OutputData.rds")

# Step11: Search for and remove words that create noise in the data ("underlying","overlying","overlain", "overlie", "overlies", "underlain", "underlie", and "underlies")
# Record start time
print(paste("Begin search for unwanted matches.", Sys.time()))
# NOTE: removing "underlie" and "overlie" should also get rid of "underlies" and "overlies"
Overlain<-grep("overlain", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlie<-grep("overlie", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlying<-grep("overlying", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlain<-grep("underlain", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlie<-grep("underlie", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlying<-grep("underlying", FossilData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
  
# Combine all of the noisy rows (sentences) into one vector 
NoisySentences<-unique(c(Overlain, Overlie,Underlain, Underlie, Underlying, Overlying))

# Remove noisy sentences from FossilData
FidelityData<-FossilData[-NoisySentences,]

# Remove hits for microfossils and trace fossils within the output sentences
Micro<-grep("microfossil", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Trace<-grep("trace fossil", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
# Remove words or phrases that are likely to cause reading errors creating false hits
NoFossils<-grep(" no fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Lack<-grep(" lack ", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Lacks<-grep(" lacks ", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
AbsentFossils<-grep("absence of fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
VoidFossils<-grep("void of fossils", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Correlative<-grep("correlative", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Equivalent<-grep("equivalent", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Above<-grep("above", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Below<-grep("below", OutputData[,"Sentence"], ignore.case=TRUE, perl=TRUE) 

UnwantedRows<-unique(c(Micro, Trace, NoFossils, Lack, Lacks, AbsentFossils, VoidFossils, Correlative, Equivalent, Above, Below))
                         
CleanedOutput<-OutputData[-UnwantedRows,]


# Make a sample table of unique unit name matches and sentences
Sample<-unique(CleanedOutput[,c("strat_name_long","Sentence")])
                         
# Take a random sample of 100  Sample rows to check accuracy
# CleanedSampleOutput3<-Sample[sample(c(1:nrow(Sample)),100,replace=FALSE),]


# Save SampleOutput1 to a folder
# write.csv(CleanedSampleOutput3,file="~/Documents/DeepDive/PBDB_Fidelity/R/CleanedSampleOutput3.csv",row.names=FALSE)
# Open the csv in excel or libre office and perform a manual accuracy test
# Renamed "CleanedSampleOutput2_Completed.csv"                   

# See how many times "formation" occurs in 
test1<-sapply(as.character(CleanedOutput[,"Sentence"]), function(x) strsplit(x, " "))
test2<-sapply(test1, function(x) paste(" ", x))
test3<-sapply(test2, function(x) paste(x, " "))
test4<-sapply(test3, function(x) grep(" formation ", ignore.case=TRUE, perl=TRUE, x))
test5<-sapply(test4, length)
test6<-which(test5>1)
CleanedOutput[test6,"Multi_Fm"]<-"TRUE"
CleanedOutput[which(is.na(CleanedOutput[,"Multi_Fm"])),"Multi_Fm"]<-"FALSE"
