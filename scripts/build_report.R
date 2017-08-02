#!/usr/bin/env Rscript
library(lubridate)
cl_args <- commandArgs(trailingOnly=FALSE)
# set working directory to script directory
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", cl_args[grep(file.arg.name, cl_args)])
if (length(script.name)>0) setwd(dirname(script.name))

if (!(cl_args[6] %in% c("brine", "channel", "dwm", "sfwcrft", "twb2", "t1a1"))){
    print("Invalid area string")
    quit()
}
if (is.na(mdy(cl_args[7]))){
    print("Invalid date string")
    quit()
}
area <- cl_args[6]
start_date <- mdy(cl_args[7]) # date to start reporting period
if (is.na(cl_args[8])){
    end_date <- start_date %m+% months(1) %m-% days(1)
} else{
    end_date <- mdy(cl_args[8])
}
report_date <- format(Sys.Date(), "%m-%d-%Y")
file_name <- paste0(area, "_", month(start_date, label=TRUE), year(start_date), 
                    ".pdf")
fl1 <- tempfile(fileext=".html")
fl2 <- tempfile(fileext=".pdf")

# render HTML file from markdown document
rmarkdown::render("report_markdown.Rmd", output_file=fl1)
# convert HTML to PDF 
system(paste0("xvfb-run wkhtmltopdf  --page-size letter ", 
              "--javascript-delay 2000 ", fl1, " ", fl2))
system(paste0("gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 ",
              "-dPDFSETTINGS=/default -dNOPAUSE -dQUIET -dBATCH ",
              "-dDetectDuplicateImages -dCompressFonts=true -r150 ",
              "-sOutputFile=", tempdir(), "/", file_name, " ", fl2))
system(paste0(path.expand(getwd()), "/gdrive upload ", 
              "-p 0B8qHESXOhs-DMk4wcVNnbUIyZTA ", tempdir(), "/", file_name))

# save workspace if needed for debugging
img_fl <- paste0("/tmp/", area, "_report_image.RData")
save.image(file=img_fl)
print(img_fl)

