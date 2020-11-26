# Single Component Lookup
component <- "trastuzumab"

grep_hemonc_regimens(conn = conn,
                     components = component)

grep_hemonc_regimens(conn = conn,
                     components = component,
                     component_count = 3)

grep_hemonc_regimens(conn = conn,
                     components = c("Lapatinib", "Trastuzumab"))

grep_hemonc_regimens(conn = conn,
                     components = c("Lapatinib", "Trastuzumab"),
                     component_count = 2)

grep_hemonc_regimens(conn = conn,
                     components = c("Lapatinib", "Trastuzumab"),
                     component_count = 3)

grep_hemonc_regimens(conn = conn,
                     components = c("Lapatinib", "Trastuzumab"),
                     component_count = 1)

grep_hemonc_regimens(conn = conn,
                     components = c("Lapatinib", "Trastuzu3mab"),
                     component_count = 1)
