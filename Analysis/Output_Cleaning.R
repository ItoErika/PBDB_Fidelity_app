#################################################  LOAD LIBRARIES #######################################################
library("RCurl")

################################################  REMOVE UNWANTED UNITS #################################################

# Load output data from application
InitialOutput<-read.csv("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_fidelity_05May2017/Fidelity_OutputData.csv", row.names=1)

# Remove units which have a t_age greater than or equal to the Precambrian upper boundary (541 Ma)
# Note: this is to remove formations which we did not filter out using t_int_age rather than t_age 
# Using t_age rather than t_int_age, we catch the following units which should be removed: 
# Sandsuck Shale, Cochran Formation, Cambridge Argillite, and Deep Spring Formation
InitialOutput<-InitialOutput[-which(InitialOutput[,"Formation"]=="Sandsuck Shale"|InitialOutput[,"Formation"]=="Cochran Formation"|InitialOutput[,"Formation"]=="Cambridge Argillite"|InitialOutput[,"Formation"]=="Deep Spring Formation"),]
# Remove ambiguously named formations from InitialOutput
InitialOutput<-InitialOutput[-which(InitialOutput[,"Formation"]=="White Dolomite"),]                                          

############################### REMOVE ROWS WITH INCORRECT LOCATIONS FROM INITIAL OUTPUT ################################

# Remove rows where the col_locations are not in doc_locations (to improve accuracy of col_id to strat name match)
# Reformat location columns
InitialOutput[,"col_locations"]<-as.character(InitialOutput[,"col_locations"])
InitialOutput[,"doc_locations"]<-as.character(InitialOutput[,"doc_locations"])

# For each row in InitialOutput, search for each state/province in col_locations in doc_locations
LocationMatch<-vector(length=nrow(InitialOutput))
for(i in 1:length(LocationMatch)){
    # Determine if there is at least one location match from col_locations in doc_locations
    LocationMatch[[i]]<-any(sapply(sapply(unlist(strsplit(InitialOutput[i,"col_locations"], ", ")), function (x,y) grep (x, y, ignore.case=TRUE, perl=TRUE), InitialOutput[i,"doc_locations"]),length)==1)
    }  

# Remove rows from InitialOutput for which none of the col_locations are in the doc_locations                                          
InitialOutput<-InitialOutput[which(LocationMatch==TRUE),]      
                                          
##################################### SUBSET OUTPUT USING PBDB TAXA, DOCID TUPLES #######################################

# Download taxanomic names (genus and below) from the Paleobiology Database
TaxaURL<-"https://paleobiodb.org/data1.2/taxa/list.csv?rank=max_genus&all_records"
TaxaURL<-getURL(TaxaURL)
Taxa<-read.csv(text=TaxaURL, header=TRUE)

# Split Taxa into species and genera 
Species<-Taxa[which(Taxa[,"taxon_rank"]=="species"),"taxon_name"]
Genera<-Taxa[which(Taxa[,"taxon_rank"]=="genus"),"taxon_name"]

# Load taxonomic name, docid tuples
PBDB_Tuples<-read.table("~/Documents/DeepDive/PBDB_Fidelity/Paper_Materials/pbdb_doc_terms", sep="\t")
# Assign column names
colnames(PBDB_Tuples)<-c("docid", "taxon_name")
PBDB_Tuples[,"docid"]<-as.character(PBDB_Tuples[,"docid"])
PBDB_Tuples[,"taxon_name"]<-as.character(PBDB_Tuples[,"taxon_name"])

# Remove tuples with taxon names that are likely to create false hits in the literature
BadTaxa<-names(sort(table(PBDB_Tuples[,"taxon_name"]), decreasing=TRUE)[1:38])
BadTaxa<-BadTaxa[-which(BadTaxa%in%c("Aspergillus", "Cretacallis", "Fossilicallis", "Pinus", "Stephanitis"))]

# Remove bad taxa from PBDBTuples
PBDB_Tuples<-PBDB_Tuples[-which(PBDB_Tuples[,"taxon_name"]%in%BadTaxa),]

# Subset PBDB_Tuples to only include taxonomic names that are at the genus or species level
PBDB_Tuples<-subset(PBDB_Tuples, PBDB_Tuples[,"taxon_name"]%in%Genera|PBDB_Tuples[,"taxon_name"]%in%Species)
        
# Subset InitialOutput to only include documents from PBDB_Tuples
PBDBTupleOutput<-subset(InitialOutput, InitialOutput[,"docid"]%in%PBDB_Tuples[,"docid"])  

PBDBTupleOutput[,"Sentence"]<-as.character(PBDBTupleOutput[,"Sentence"])

########################## REMOVE SENTENCES WITH WORDS/PHRASES LIKELY TO CAUSE READING ERRORS ##########################

# Remove words or phrases that are likely to cause reading errors creating false hits
NoFossils<-grep(" no fossils", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Lack<-grep(" lack ", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Lacks<-grep(" lacks ", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Absence<-grep("absence", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Barren<-grep("barren", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
VoidFossils<-grep("void of fossils", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Correlative<-grep("correlative", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Equivalent<-grep("equivalent", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Above<-grep("above", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Below<-grep("below", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Beneath<-grep("beneath", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Relict<-grep("relict", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Rework<-grep("rework", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Unfossilierous<-grep("unfossilierous", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
                                          
# NOTE: removing "underlie" and "overlie" should also get rid of "underlies" and "overlies"
Overlie<-grep("overlie", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlying<-grep("overlying", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Overlain<-grep("overlain", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlie<-grep("underlie", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlying<-grep("underlying", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Underlain<-grep("underlain", PBDBTupleOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)

# Bind all noisy sentences
NoisySentences<-unique(c(NoFossils, Lack, Lacks, Absence, Barren, VoidFossils, Correlative, Equivalent, Above,
Below, Beneath, Relict, Rework, Unfossilierous, Overlie, Overlying, Overlain, Underlie, Underlying, Underlain)) 

# Remove rows with noisy sentences from InitialOutput
NoNoiseOutput<-PBDBTupleOutput[-NoisySentences,]
                                          
############################### CREATE OUTPUT VERSIONS WITHOUT MICRO OR TRACE FOSSILS ###################################

# Find instances of micro and trace fossils in NoNoiseOutput sentences
                                          
# Locate hits for trace fossils within the output sentences
Trace<-grep("trace fossil", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Ichno<-grep("ichno", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Burrow<-grep("burrow", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Trackway<-grep("trackway", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
                                          
# Locate hits for microfossils within the output sentences
Micro<-grep("microfossil", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Spore<-grep("spore", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Foram<-grep(" foram", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Radiolaria<-grep("radiolaria", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)                                          
Graptolite<-grep("graptolite", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Conodont<-grep("conodont", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Diatom<-grep("diatom", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Coccolith<-grep("coccolith", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Pollen<-grep("pollen", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
Acritarch<-grep("acritarch", NoNoiseOutput[,"Sentence"], ignore.case=TRUE, perl=TRUE)
                                        
TraceSentences<-unique(c(Trace, Ichno, Burrow, Trackway)) 
MicroSentences<-unique(c(Micro, Spore, Foram, Radiolaria, Graptolite, Conodont, Diatom, Coccolith, Pollen, Acritarch)) 
TraceOrMicroSentences<-unique(c(TraceSentences, MicroSentences)) 

# Create a NoNoiseOutput with no trace fossils, but including microfossils
NoTraceOutput<-NoNoiseOutput[-TraceSentences,]

# Create a NoNoiseOutput with no microfossils, but including trace fossils
NoMicroOutput<-NoNoiseOutput[-MicroSentences,]

# Create a NoNoiseOutput with no micro or trace fossils
NoMicroNoTraceOutput<-NoNoiseOutput[-TraceOrMicroSentences,]


