#' Get any synonyms in chemidplus
#' @description Helpful with investigational drugs
#' @import httr
#' @import rvest
#' @import stringr
#' @import police
#' @import centipede
#' @export

annotate_chemidplus_synonyms <-
    function(phrase,
             type = "contains") {
        
        require(httr)
        require(rvest)
        
        #Remove all spaces
        phrase <- stringr::str_remove_all(phrase, "\\s")
        
        url <- paste0("https://chem.nlm.nih.gov/chemidplus/name/", type, "/",  phrase)
        url = police::try_catch_error_as_null(url(url, "rb"))
        
        #print("714")
        if (!is.null(url)) {
            
            #print("718")
            resp <- police::try_catch_error_as_null(read_html(url))
            Sys.sleep(1)
            close(url)
            
            if (!is.null(resp)) {
                # If there aren't any #names headers, it is likely that the query resulted in multiple search results and needs to be tied to an RN number
                qa <-
                    resp %>%
                    html_nodes("#names")
                
                # If there are 0 html_names #names, checking to see if it landed on a multiple results page
                if (length(qa) == 0) {
                    #print("733")
                    
                    multiple_results <-
                        resp %>%
                        html_nodes("div") %>%
                        html_text()
                    
                    if (any(grepl("^1$", multiple_results))) {
                        # Example: Tamoxifen [INN:BAN]10540-29-1
                        first_drug_name <- multiple_results[grep("^1$", multiple_results)+1][1]
                        print(first_drug_name)
                        
                        # Getting the numeric rn to re-query ie 10540-29-1 to get https://chem.nlm.nih.gov/chemidplus/rn/10540-29-1
                        first_drug_rn <- stringr::str_replace_all(first_drug_name, "(^.*?[^[0-9]]{1})([0-9]{2,}[-]{1}[0-9]{2,}[-]{1}[0-9]{1,}$)", "\\2")
                        print(first_drug_rn)
                        #print("746")
                        
                        url <- paste0("https://chem.nlm.nih.gov/chemidplus/rn/", first_drug_rn)
                        url <- police::try_catch_error_as_null(url(url, "rb"))
                        
                        resp <- police::try_catch_error_as_null(read_html(url))
                        
                        Sys.sleep(1)
                        
                        if (!is.null(url)) {close(url)}
                        
                    }
                }
            }
        }
        
        if (!is.null(resp)) {
            
            resp <-
                resp %>%
                html_nodes("#names") %>%
                html_text() %>%
                strsplit(split = "\n") %>%
                unlist() %>%
                stringr::str_remove_all("Systematic Name|Names and Synonyms|Results Name|Name of Substance|MeSH Heading|Synonyms|[^ -~]") %>%
                trimws("both") %>%
                centipede::no_blank() %>%
                unique()
            
            return(resp)
            
        } else {
            return("NULL response")
        }
        Sys.sleep(3)
        closeAllConnections()
    }