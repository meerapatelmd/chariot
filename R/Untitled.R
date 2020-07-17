# hemonc_components <-
#     chariot::query_athena("SELECT DISTINCT concept_name FROM concept WHERE concept_class_id = 'Component' AND vocabulary_id = 'HemOnc'")
# 
# hemonc_components <- hemonc_components$concept_name
# while (length(hemonc_components) > 0) {
#         
#         hemonc_component <- hemonc_components[1]
#         
#         search_chemidplus_url(phrase = hemonc_component,
#                               conn = conn)
#         
#         Sys.sleep(10)
#         
#         hemonc_components <- hemonc_components[-1]
#     
# }
