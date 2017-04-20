#!/usr/bin/env Rscript
library(lubridate)
args <- commandArgs(trailingOnly=TRUE)
if (!(args[1] %in% c("brine", "channel", "dwm", "sfwcrft", "twb2", "t1a1"))){
    print("Invalid area string")
    quit()
}
if (is.na(mdy(args[2]))){
    print("Invalid date string")
    quit()
}
area <- args[1]
start_date <- mdy(args[2]) # date to start reporting period
if (is.na(args[3])){
    end_date <- start_date %m+% months(1) %m-% days(1)
} else{
    end_date <- mdy(args[3])
}
report_date <- format(Sys.Date(), "%m-%d-%Y")
file_name <- paste0("~/code/owensReports/output/", area, "_", 
                    month(start_date, label=TRUE), year(start_date))

# render HTML file from markdown document
rmarkdown::render("~/code/owensReports/scripts/report_markdown.Rmd", 
                  output_file=paste0(file_name, ".html"))
# convert HTML to PDF 
convert_command <- paste0("xvfb-run wkhtmltopdf  --page-size letter ",
                          file_name, ".html ", file_name, ".pdf") 
system(convert_command)

# notify on finish
system(paste0("notify-send \"", args[1], " report is finished\""))

# save workspace if needed for debugging
img_fl <- paste0("/tmp/", area, "_report_image.RData")
save.image(file=img_fl)
print(img_fl)
