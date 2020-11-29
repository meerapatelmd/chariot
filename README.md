# chariot <img src="man/figures/logo.png" align="right" alt="" width="120" />  

This package queries a local Postgres instance using files manually downloaded from Athena at athena.ohdsi.org. The package spawned out of my own homegrown functions I've written to help me map clinical concepts. As a result, much of the functions are hard-coded to specifically query Athena exports that are populated in the public schema of a Postgre database called "Athena". Over time, I hope to be able to modify the parameters to make it more open-source for anyone else. There still may be some blockers should you install your own Postgres with the same exact parameters as mine. 

# Installation    
  
```   
devtools::install_github("meerapatelmd/chariot")
```
  

## Requirements     
A Postgres instance of the OMOP Vocabularies can be instantiated by either using the [setupAthena R Package](https://github.com/meerapatelmd/setupAthena) or can be manually instantiated by running scripts at [OHDSI/CommonDataModel repo PostgreSQL folder]( https://github.com/OHDSI/CommonDataModel/tree/master/PostgreSQL) directly importing the csvs into your client.     



