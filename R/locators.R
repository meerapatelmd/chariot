#' Locates the vocabulary table and search type where the phrase is first found
#' @description Useful if a simple search isn't returning results.
#' @export

locatePhrase <-
    function(phrase,
             split,
             schema) {


                results <-  queryPhrase(schema = schema,
                                        phrase = phrase,
                                        split = split)

                possibleLocations <-
                c('exact', 'string', 'like', 'synonym_exact', 'synonym_string', 'synonym_like')


                for (possibleLocation in possibleLocations) {
                        if (nrow(results[[possibleLocation]])) {
                                return(possibleLocation)
                        }
                }

    }
