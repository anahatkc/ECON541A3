# ECON541A3
ECON 541 Research Paper

The code can be run as is. Two datafiles are loaded on which give us the main dataset. 

"wmn_nonres" gives us female autonomy variables for each wave with the wave1 variables written as X.VARNAME and the wave2 being just VARNAME. 

Column MIG_STAT provides information if the woman belongs to a house that had a non-resident in wave 1 only (MIG_STAT == 1), in wave 2 only (MIG_STAT == 2), in both waves (MIG_STAT == 3) or in neither wave (MIG_STAT == 0).

Variables related to the non-resident following, once again with variables related to wave 1 being labelled X.VARNAME and those related to wave 2 being VARNAME. If a non-resident migrated only in wave 2 then all X.VARNAMEs for that row will not have data. 

Files can be linked across datasets by creating a concatenated variable 'ID' that contains in this order STATEID, DISTID, PSUID, HHID, HHSPLIT and PERSONID. Dummy variables are created to help with analysis.

The folder contains the ready to go wmn_nonres files that have already been cleaned and the R file can be run as is. 
