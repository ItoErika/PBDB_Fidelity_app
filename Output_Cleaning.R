# Load output data from application
InitialOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/Fidelity_OutputData.csv")

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
# NOTE: removing "underlie" and "overlie" should also get rid of "underlies" and "overlies"
Overlie<-grep("overlie", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlying<-grep("overlying", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlain<-grep("overlain", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlie<-grep("underlie", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlying<-grep("underlying", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlain<-grep("underlain", InitialOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)

NoisySentences<-unique(c(NoFossils, Lack, Lacks, AbsentFossils, VoidFossils, Correlative,
Equivalent, Above, Below, Overlie, Overlying, Overlain, Underlie, Underlying, Underlain)) 

# Remove rows with noisy sentences from InitialOutput
CleanedOutput<-InitialOutput[-NoisySentences,]

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

# Create a cleaned output with no trace fossils, but including microfossils
NoTraceOutput<-CleanedOutput[-TraceSentences,]

# Create a cleaned output with no microfossils, but including trace fossils
NoMicroOutput<-CleanedOutput[-MicroSentences,]

# Create a cleaned output with no micro or trace fossils
NoMicroNoTraceOutput<-CleanedOutput[-TraceOrMicroSentences,]


