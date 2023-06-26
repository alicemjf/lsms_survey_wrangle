source_folder <- file.path(".", "scripts")
output_folder <- file.path('.', "outputs", "markdown")

rmarkdown::render(input = paste0(source_folder, "/", "03_summaryRMD.rmd"),
                  output_format = "html_document",
                  output_file = paste0("data_summary_",Sys.Date(),".html"),
                  output_dir = output_folder)
str(hh_summary)


