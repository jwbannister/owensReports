load_all()
library(lubridate)
rm(list=ls())

for (i in c("08", "09", "10")){
    start_date <- mdy(paste0(i, "-01-2016")) # date to start reporting period
    duration <- 1 # number of months to report 
    end_date <- start_date %m+% months(duration) %m-% days(1)
    report_date <- format(Sys.Date(), "%m-%d-%Y")
    file_name <- paste0("~/code/ssReports/output/SS_report_", 
                        month(start_date, label=TRUE), year(start_date))

    # render HTML file from markdown document
    rmarkdown::render(paste0("scripts/SS_report.Rmd"), 
                      output_file=paste0(file_name, ".html"))
    # convert HTML to PDF 
    convert_command <- paste0("wkhtmltopdf ", file_name, ".html ", file_name, ".pdf")
    system(convert_command)
}
