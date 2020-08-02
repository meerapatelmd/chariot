#
#
#
#
#
#
# queryPhrase <-
#         function(schema,
#                  phrase,
#                  caseInsensitive,
#                  type = c("exact", "string", "like"),
#                  synonym = FALSE) {
#
#
#                         if (length(type) != 1) {
#
#                                 stop("'type' must be length 1 of c('exact', 'string', 'like')")
#
#                         }
#
#                         if (!(type %in% c("exact", "string", "like"))) {
#
#
#                                 stop("'type' must be length 1 and one of c('exact', 'string', 'like')")
#
#                         }
#
#
#                         if (type == "exact") {
#
#                                 if (synonym) {
#
#                                         queryPhraseExactSynonym(schema = schema,
#                                                                 caseInsensitive = caseInsensitive,
#                                                                 phrase = phrase,
#
#                                 }
#
#
#
#
#                         }
#
#
#
#
#
#
#         }
