# Load CleanedOutput
CleanedOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/CleanedOutput.csv")

# Find instances of micro and trace fossils in CleanedOutput sentences
# Locate hits for trace fossils within the output sentences
Trace<-grep("trace fossil", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Ichno<-grep("ichno", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
# Locate hits for microfossils within the output sentences
Micro<-grep("microfossil", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Spore<-grep("spore", CleanedOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)

TraceSentences<-unique(c(Trace, Ichno)) 
MicroSentences<-unique(c(Micro, Spore)) 
TraceOrMicroSentences<-unique(c(TraceSentences, MicroSentences)) 

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
