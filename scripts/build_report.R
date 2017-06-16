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
file_name <- paste0(area, "_", month(start_date, label=TRUE), year(start_date))
fl1 <- tempfile(fileext=".html")
fl2 <- tempfile(fileext=".pdf")

# render HTML file from markdown document
rmarkdown::render("./scripts/report_markdown.Rmd", output_file=fl1)
# convert HTML to PDF 
system(paste0("xvfb-run wkhtmltopdf  --page-size letter ", 
              "--javascript-delay 2000 ", fl1, " ", fl2))
system(paste0("gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 ",
              "-dPDFSETTINGS=/default -dNOPAUSE -dQUIET -dBATCH ",
              "-dDetectDuplicateImages -dCompressFonts=true -r150 ",
              "-sOutputFile=", output_path, file_name, ".pdf ", fl2))

# save workspace if needed for debugging
img_fl <- paste0("/tmp/", area, "_report_image.RData")
save.image(file=img_fl)
print(img_fl)
