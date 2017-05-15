See [Stats Page](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Analysis/Stats.R) for script.

+ Run Date:\
**2017-05-05 = DELTA**
+ Number of documents in total corpus on run date:\
**3,454,922 documents fetched**
+ Number of initial documents matched to Macrostrat:\
**THETA = 115,314**
+ Number of documents the application was run on:\
**95,931**
+ How many fossil occurrences are in PBDB in North America at app run date (From PBDB API):\
**ALPHA = 491,180**
+ How many North American fossil occurrences are NOT matched to Macrostrat (sum Macrostrat occurrences, sum all North American PBDB occurrences, take difference):
    + North American PBDB Occurrences: **491,180**
    + Total Macrostrat occurrences: **464,655**
    + Difference: **GAMMA = 26,525**
+ Total number of sedimentary, Phanerozoic formations in Macrostrat:\
**EPSILON = 4,682**
+ How many  Phanerozoic sedimentary rock **formations** in Macrostrat have PBDB fossil occurrences:\
**BETA = 2,021**
+ Number of candidate formations (Macrostrat, sedimentary, Phanerozoic formations that do NOT have PBDB fossil occurrences) (EPSILON-BETA=**ZETA**, perform in-app check with length(....)):\
**ZETA = 2,661**
+ How much data is cut down from geolocation check:\
**Went from 31,402 to 26,817 documents (4,585 documents removed)**\
**Went from 2,255 to 2,168 candidate units (87 candidate units removed)**

+ Number of candidate units, non-candidate units (in PBDB), and documents in initial output:
    + **710 candidate units**
    + **1,020 documents in initial output mentioning candidate units**
    + **1,119 non-candidate units (in PBDB)**
    + **2,974 documents in initial output mentioning non-candidate units**
    
+ Number of candidate units, non-candidate units (in PBDB), and documents in [PBDBTupleOutput](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R) (InitialOutput after removing documents that do not contain either a species or genus taxonomic name):
    + **702 candidate formations**
    + **991 documents in cleaned output mentioning candidate units**
    + **1,114 non-candidate formations (in PBDB)**
    + **2,913 documents mentioning non-candidate units**    
    
+ Number of candidate units, non-candidate units (in PBDB), and documents in [CleanedOutput](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R) (PBDBTupleOutput with sentences with words or phrases likely to cause reading errors [e.g. "overlain", "underlain", "correlative", etc.] removed):
    + **604 candidate formations**
    + **828 documents in cleaned output mentioning candidate units**
    + **1,043 non-candidate formations (in PBDB)**
    + **2,666 documents in cleaned output mentioning non-candidate units**
   
+ Percentage of formations we identified as fossiliferous that were unrecorded in the Paleobiology Database when we ran app: (# of fossiliferous formations in NOT in PBDB in CleanedOutput/ # of fossiliferous formations in CleanedOutput):
    + **Number of formations we found to be fossiliferous which were not in PBDB: 604**
    + **Number of formations we confirmed to be fossiliferous which WERE in PBDB: 1,043**
    + **Total number of formations we were able to identify as fossiliferous through our application processes: 1,647**
    + **Division: 604 / 1,647 = 36.673%**

+ Number of candidate units, non-candidate units (in PBDB), and documents in [NoTraceOutput](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R) (output with sentences including "trace fossil" or "ichno" removed):
    + **588 candidate formations**
    + **782 documents in cleaned output mentioning candidate units**
    + **1,028 non-candidate formations (in PBDB)**
    + **2,563 documents mentioning non-candidate units**
    
+ Number of candidate units, non-candidate units (in PBDB), and documents in [NoMicroOutput](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R) (output with sentences including "microfossil" or "spore" removed):
    + **600 candidate formations**
    + **826 documents in cleaned output mentioning candidate units**
    + **1,040 non-candidate formations (in PBDB)**
    + **2,653 documents mentioning non-candidate units**
    
+ Number of candidate units, non-candidate units (in PBDB), and documents in [NoMicroNoTraceOutput](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R):
    + **584 candidate formations**
    + **780 documents in cleaned output mentioning candidate units**
    + **1,025 non-candidate formations (in PBDB)**
    + **2,550 documents mentioning non-candidate units**
    

    
