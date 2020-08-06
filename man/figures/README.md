## Chariot  
This package queries a local Postgres instance using files manually downloaded from Athena at athena.ohdsi.org. The package spawned out of my own homegrown functions I've written to help me map clinical concepts. As a result, much of the functions are hard-coded to specifically query Athena exports that are populated in the public schema of a Postgre database called "Athena". Over time, I hope to be able to modify the parameters to make it more open-source for anyone else. There still may be some blockers should you install your own Postgres with the same exact parameters as mine. 

## Requirements 
1. Local Postgres. I installed mine using the Postgres.app (https://postgresapp.com/).
2. Bundled csv download from athena.ohdsi.org  

## Procedure  
Run the scripts in this repo according to the README https://github.com/OHDSI/CommonDataModel/tree/master/PostgreSQL. At step 2, the csv can be loaded through the Import option in Postgres.app into the appropriate table. Note that you will have many empty CDM tables since you will only have the vocabulary tables at hand (as opposed to all the clinical data from your institutional database). I usually drop these tables at the end.
