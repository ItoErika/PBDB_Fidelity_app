############################################ LOAD CLEANEDOUTPUT ##################################################
# Load CleanedOutput
CleanedOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/CleanedOutput.csv")

# Find instances of micro and trace fossils in CleanedOutput sentences
# Locate hits for trace fossils within the output sentences
Trace<-grep("trace fossil", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Ichno<-grep("ichno", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Burrow<-grep("burrow", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Trackway<-grep("trackway", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
TraceSentences<-unique(c(Trace, Ichno, Burrow, Trackway)) 

# Locate hits for microfossils within the output sentences
Micro<-grep("microfossil", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Spore<-grep("spore", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Foram<-grep(" foram", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Radiolaria<-grep("radiolaria", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)                                          
Graptolite<-grep("graptolite", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Conodont<-grep("conodont", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Diatom<-grep("diatom", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Coccolith<-grep("coccolith", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Pollen<-grep("pollen", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Acritarch<-grep("acritarch", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
MicroSentences<-unique(c(Micro, Spore, Foram, Radiolaria, Graptolite, Conodont, Diatom, Coccolith, Pollen, Acritarch)) 

# Tag sentences with trace fossils
CleanedOutput[TraceSentences,"Trace"]<-"TRUE"

# Tag sentences with microfossils
CleanedOutput[MicroSentences,"Micro"]<-"TRUE"

# Create a subset of CleanedOutput with only candidate units
CheckOutput<-CleanedOutput[which(CleanedOutput[,"PBDB_occ"]==FALSE),]

# Make sure not to extract duplicate sentences in the sample
CheckOutput<-CheckOutput[!duplicated(CheckOutput[,"Sentence"]),]

# Take a random sample of 100 rows from CheckOutput
Sample<-CheckOutput[sample(c(1:nrow(CheckOutput)), 100, replace=FALSE),]

# Save the sample
# write.csv(Sample, "~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/Sample.csv", row.names=FALSE)
