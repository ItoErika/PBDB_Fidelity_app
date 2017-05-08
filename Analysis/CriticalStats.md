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
+ Number of candidate units, non-candidate units (in PBDB), and documents in [cleaned output](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R) (output with sentences with words or phrases likely to cause reading errors [e.g. "overlain", "underlain", "correlative", etc.] removed):
    + **610 candidate formations**
    + **853 documents in cleaned output mentioning candidate units**
    + **1,047 non-candidate formations (in PBDB)**
    + **2,722 documents in cleaned output mentioning non-candidate units**
    
+ Number of candidate units, non-candidate units (in PBDB), and documents in [output without trace fossils](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R) (output with sentences including "trace fossil" or "ichno" removed):
    + **594 candidate formations**
    + **807 documents in cleaned output mentioning candidate units**
    + **1,032 non-candidate formations (in PBDB)**
    + **2,617 documents in cleaned output mentioning non-candidate units**
    
+ Number of candidate units, non-candidate units (in PBDB), and documents in [output without microfossils](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R) (output with sentences including "microfossil" or "spore" removed):
    + **606 candidate formations**
    + **851 documents in cleaned output mentioning candidate units**
    + **1,044 non-candidate formations (in PBDB)**
    + **2,709 documents in cleaned output mentioning non-candidate units**
    
+ Number of candidate units, non-candidate units (in PBDB), and documents in [output without trace fossils or microfossils](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R):
    + **590 candidate formations**
    + **805 documents in cleaned output mentioning candidate units**
    + **1,029 non-candidate formations (in PBDB)**
    + **2,604 documents in cleaned output mentioning non-candidate units**
    
    
+ Number of candidate units, non-candidate units (in PBDB), and documents in final output (subset to documents with mention of genus or species taxonomic names). [Click here](https://github.com/ItoErika/PBDB_Fidelity_app/blob/master/Analysis/PBDB_Tuples.R) to see script.
    + ** candidate formations**
    + ** documents in cleaned output mentioning candidate units**
    + ** non-candidate formations (in PBDB)**
    + ** documents in cleaned output mentioning non-candidate units**
