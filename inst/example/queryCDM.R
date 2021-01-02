library(pg13)
library(tidyverse)

conn <- local_connect("polyester")

queryCDM(sql_statement = "SELECT * FROM omop_cdm.drug_exposure ORDER BY RANDOM() LIMIT 5;",
         write_schema = "omop_cdm",
         vocab_schema = "omop_cdm",
         conn = conn)

dc(conn = conn)
