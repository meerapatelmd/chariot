job_resume <- read_gsheet(resume_drbl$drive_resource[[1]]$webViewLink)


output <-
        resume$`PROFESSIONAL EXPERIENCE` %>%
        mutate(Value = sprintf("- %s", Value)) %>%
        rubix::grouped_paste(Company, Class, paste_col = Value, collapse = "\n") %>%
        rubix::grouped_paste(Company, paste_col = Value, collapse = "\n") %>%
        sort_jobs()

for (i in seq_along(output$Company)) {
        header <- output$Company[i]
        value <- output$Value[i]

        secretary::typewrite(secretary::enbold(output$Company[i]), timepunched = F)
        cat(output$Value[i])
        cat("\n\n")
        secretary::press_enter()
}
