load_all()
library(lubridate)
rm(list=ls())

area <- "dwm" # c("brine", "channel", "dwm", "sfwcrft", "twb2")

start_date <- mdy("10-01-2016") # date to start reporting period
end_date <- start_date %m+% months(1) %m-% days(1)
report_date <- format(Sys.Date(), "%m-%d-%Y")
file_name <- paste0("~/code/owensReports/output/", area, "_", 
                    month(start_date, label=TRUE), year(start_date))

# render HTML file from markdown document
if (area %in% c("brine", "dwm", "channel")){
    rmarkdown::render(paste0("scripts/misc_report.Rmd"), 
                      output_file=paste0(file_name, ".html"))
} else{
    rmarkdown::render(paste0("scripts/", area, "_report.Rmd"), 
                      output_file=paste0(file_name, ".html"))
}
# convert HTML to PDF 
convert_command <- paste0("wkhtmltopdf  --page-size letter ", 
                          file_name, ".html ", file_name, ".pdf") 
system(convert_command)
