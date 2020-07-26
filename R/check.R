fns <- list.files("R", full.names = TRUE)
while (length(fns)) {
        fn <- fns[1]

        input_lines <- readr::read_lines(fn)

        if (any(grepl("write_sql_for_anc", input_lines))) {
                print(fn)
                secretary::press_enter()
        }


        fns <- fns[-1]
}
