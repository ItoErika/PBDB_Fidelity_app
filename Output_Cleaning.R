# Load output data from application
InitialOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_12Apr2017/Fidelity_OutputData.csv")

# Remove hits for trace fossils within the output sentences
Trace<-grep("trace fossil", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
# Remove words or phrases that are likely to cause reading errors creating false hits
NoFossils<-grep(" no fossils", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Lack<-grep(" lack ", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Lacks<-grep(" lacks ", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
AbsentFossils<-grep("absence of fossils", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
VoidFossils<-grep("void of fossils", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Correlative<-grep("correlative", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Equivalent<-grep("equivalent", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Above<-grep("above", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Below<-grep("below", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlain<-grep("overlain", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
# NOTE: removing "underlie" and "overlie" should also get rid of "underlies" and "overlies"
Overlie<-grep("overlie", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlying<-grep("overlying", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlain<-grep("underlain", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlie<-grep("underlie", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlying<-grep("underlying", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Ichno<-grep("ichno", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)

NoisySentences<-unique(c(Trace, NoFossils, Lack, Lacks, AbsentFossils, VoidFossils, Correlative, Equivalent, Above, 
Below, Overlain, Overlie, Underlain, Underlie, Underlying, Overlying, Ichno))
                         
CleanedOutput<-InitialOutput[-NoisySentences,]

