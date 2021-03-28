# chariot 1.2.0 (2021-03-28)  
* fixed bugs in `strip` and `label` formatting functions  
* added vignettes for HemOnc functions (`ho_*`), pivoting Concept Relationship and 
Concept Ancestor resultsets, and manipulating strings for mapping purposes.  
* added Vocabulary Level functions (`vocab_*`)  
* added functions that retrieve test data  (`get_test_*`)  
* added scraping of CDM GitHub wiki on package load  
  

# chariot 1.1.0 (2021-01-09)  
* add Drug Strength Staging (`ds_*`) functions to prep and join data for drug exposure 
calculations   


# chariot 1.0.0 (2021-01-02)  
* remove all deprecated functions, including `pivot`, `plot`, `render`, 
`set_parameters`  
* remove `single_concept_ancestry` functions in favor of 
`print_concept_ancestry()` 


# chariot 0.3.0 (2020-12-28)  
* add `maps_to_concept_names` slot to `concept` S4 class object for 
`get_name_cluster` function 
* add `join_on_` and `join_for_*` functions  
* add `queryCDM` function to query a CDM table with automatic iterative joins 
with the Concept Table
* deprecate collapsibleTree plotting functions in favor of 
[amphora R package](https://meerapatelmd.github.io/amphora)    


# chariot 0.2.0 (2020-11-28)
* optimize hemonc functions
* streamline collapsibleTree plotting functions  

# chariot 1.2.0 (2020-08-04)
* optimize filter functions
* streamline concept.tree() into umbrella and bottleneck style plots
* add Sys.sleep feature to querying
* add documentation for leftJoin functions  

# chariot 1.1.1  
* updated all left join functions with pg13 package dependencies
* optimized merge functions
* add first version of concept.tree() function that renders a collapsibleTree

# chariot 0.1.1
* makeLabel function to return <NA> instead of "NA NA" for missing concept_id and concept_name combinations  
