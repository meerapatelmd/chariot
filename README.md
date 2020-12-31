
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chariot <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

<!-- badges: end -->

Query, analyze, and explore a Postgres instance of the OMOP Vocabularies
for a multitude of use cases, including standardizing data to the OMOP
Vocabularies, automatically annotating data pulled from the OMOP CDM
with the concept attributes, and calculating cumulative drug dosage from
data found in the Drug Exposures table.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("meerapatelmd/chariot")
```

## Related Packages

  - [setupAthena](https://meerapatelmd.github.io/setupAthena): sets up a
    Postgres instance of the Athena vocabularies downloaded from
    athena.ohdsi.org.  
  - [amphora](https://meerapatelmd.github.io/amphora): plots concept
    ancestries to assist in knowledge management projects such as data
    standardization.  
  - [metaorite](https://meerapatelmd.github.io/metaorite): the UMLS
    Metathesaurus equivalent of this package

## Concept Objects

This package was inititally written with the objective of exploring the
vocabularies for concept mapping. To avoid the challenge of using solely
the Concept Id in the R environment when exploring a certain concept, an
S4 `Concept` class is introduced, where `Concept` objects store all the
concept attributes found in the Concept Table as slots. This is a basic
example which shows you how to solve a common problem:

``` r
library(chariot)
aspirin <- get_concept(concept_id = 1112807,
                       vocab_schema = "omop_vocabulary",
                       conn = conn)
class(aspirin)
#> [1] "concept"
#> attr(,"package")
#> [1] "chariot"
```

``` r
aspirin
#> An object of class "concept"
#> Slot "concept_id":
#> [1] 1112807
#> 
#> Slot "concept_name":
#> [1] "aspirin"
#> 
#> Slot "concept_synonym_names":
#> [1] ""
#> 
#> Slot "domain_id":
#> [1] "Drug"
#> 
#> Slot "vocabulary_id":
#> [1] "RxNorm"
#> 
#> Slot "concept_class_id":
#> [1] "Ingredient"
#> 
#> Slot "standard_concept":
#> [1] "S"
#> 
#> Slot "concept_code":
#> [1] "1191"
#> 
#> Slot "valid_start_date":
#> [1] "1970-01-01"
#> 
#> Slot "valid_end_date":
#> [1] "2099-12-31"
#> 
#> Slot "invalid_reason":
#> [1] NA
```

`Concept` class objects keep the user informed of all the attributes of
a concept during a session using the `chariot` package. For example, I
can look up the RxNorm Ingredients belonging to the ATC 5th Concept
Class `lisinopril` using the Concept Id `21601787` alone.

``` r
resultset <-
lookup_atc_class_ingredients(conn = conn, 
                             vocab_schema = "omop_vocabulary",
                             atc_concept_obj = 21601787)
```

``` r
resultset
#> # A tibble: 1 x 10
#>   concept_id concept_name domain_id vocabulary_id concept_class_id
#>        <dbl> <chr>        <chr>     <chr>         <chr>           
#> 1    1308216 lisinopril   Drug      RxNorm        Ingredient      
#> # … with 5 more variables: standard_concept <chr>, concept_code <chr>,
#> #   valid_start_date <date>, valid_end_date <date>, invalid_reason <chr>
```

However, upon viewing the output, the only knowledge known based on the
arguments is the `21601787` concept id. When using the `Concept` class,
this information is not lost.

``` r
lisinopril_class_concept <- get_concept(conn = conn,
                                        concept_id = 21601787, 
                                        vocab_schema = "omop_vocabulary")
resultset <-
lookup_atc_class_ingredients(conn = conn, 
                             vocab_schema = "omop_vocabulary",
                             atc_concept_obj = lisinopril_class_concept)
```

``` r
resultset
#> # A tibble: 1 x 10
#>   concept_id concept_name domain_id vocabulary_id concept_class_id
#>        <dbl> <chr>        <chr>     <chr>         <chr>           
#> 1    1308216 lisinopril   Drug      RxNorm        Ingredient      
#> # … with 5 more variables: standard_concept <chr>, concept_code <chr>,
#> #   valid_start_date <date>, valid_end_date <date>, invalid_reason <chr>
```

The resultset can be referenced back to the `lisinopril_class_concept`
object that has retained all the concept attributes:

``` r
lisinopril_class_concept
#> An object of class "concept"
#> Slot "concept_id":
#> [1] 21601787
#> 
#> Slot "concept_name":
#> [1] "lisinopril; oral"
#> 
#> Slot "concept_synonym_names":
#> [1] "lisinopril"
#> 
#> Slot "domain_id":
#> [1] "Drug"
#> 
#> Slot "vocabulary_id":
#> [1] "ATC"
#> 
#> Slot "concept_class_id":
#> [1] "ATC 5th"
#> 
#> Slot "standard_concept":
#> [1] "C"
#> 
#> Slot "concept_code":
#> [1] "C09AA03"
#> 
#> Slot "valid_start_date":
#> [1] "1970-01-01"
#> 
#> Slot "valid_end_date":
#> [1] "2099-12-31"
#> 
#> Slot "invalid_reason":
#> [1] NA
```
