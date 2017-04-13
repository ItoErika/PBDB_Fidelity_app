See [Stats Page](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Analysis/Stats.R) for script.

+ Run Date:\
**2017-04-12 = DELTA**
+ Number of documents in total corpus on run date:\
**3,163,729 documents fetched**
+ Number of initial documents matched to Macrostrat:\
**THETA = 100,173**
+ Number of documents the application was run on:\
**77,679**
+ How many fossil occurrences are in PBDB in North America at app run date (From PBDB API):\
**ALPHA = 489,776**
+ How many North American fossil occurrences are NOT matched to Macrostrat (sum Macrostrat occurrences, sum all North American PBDB occurrences, take difference):
    + North American PBDB Occurrences: **489,776**
    + Total Macrostrat occurrences: **464,655**
    + Difference: **GAMMA = 25,121**
+ Total number of sedimentary, Phanerozoic formations in Macrostrat:\
**EPSILON = 4,682**
+ How many  Phanerozoic sedimentary rock **formations** in Macrostrat have PBDB fossil occurrences:\
**BETA = 2,021**
+ Number of Macrostrat, sedimentary, Phanerozoic formations that do NOT have PBDB fossil occurrences (EPSILON-BETA=**ZETA**, perform in-app check with length(....)):\
**ZETA = 2,661**
+ How much data is cut down from geolocation check:\
**Went from 4,837 to 4,544 documents (293 documents removed)**\
**Went from 763 to 735 candidate units (28 candidate units removed)**
+ Number of candidate units, non-candidate units (in PBDB), and documents in initial output:
    + **735 candidate units**
    + **1,191 documents in initial output mentioning candidate units**
    + **1,120 non-candidate units (in PBDB)**
    + **3,869 documents in initial output mentioning non-candidate units**
+ Number of candidate units, non-candidate units (in PBDB), and documents in [cleaned output](https://github.com/ItoErika/PBDB_Fidelity_app/edit/master/Output_Cleaning.R):
    + **618 candidate formations**
    + **935 documents in cleaned output mentioning candidate units**
    + **1,032 non-candidate formations (in PBDB)**
    + **3,368 documents in cleaned output mentioning non-candidate units**
+ Number of candidate units, non-candidate units (in PBDB), and documents in final output (subset to documents with mention of genus or species taxonomic names). [Click here](https://github.com/ItoErika/PBDB_Fidelity_app/blob/master/Analysis/PBDB_Tuples.R) to see script.
    
