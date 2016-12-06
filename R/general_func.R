S3_bucket_access <- function(key, file){
    aws_access <- read.table("~/config/credentials/AWS_cred.txt")[2, 1]
    aws_secret <- read.table("~/config/credentials/AWS_cred.txt")[4, 1]
    RS3::S3_connect(aws_access, aws_secret, hostname="s3-us-west-2.amazonaws.com")
    RS3::S3_get_object("saltonimages", key, file)
}

report_header <- function(start_date, end_date, report_date, area){
    index <- data.frame(a = c("brine", "channel", "dwm", "sfwcrft", "twb2", "t1a1"), 
                        b = c("Brine", "Channel", 
                              "Dynamic Water Management", 
                              "Shallow Flood Wetness Curve Refinement Field Test",
                              "Tillage with BACM Backup", 
                              "T1A-1"))
    cat("<img style=\"float: right;\" src=\"logo.png\"> \n")
    cat(" \n# ", index[index$a==area, ]$b, " \n")
    cat(" \n##Summary Period: ", format(start_date, "%m-%d-%Y"), " through ", 
        format(end_date, "%m-%d-%Y"), " \n")
    cat(" \n##Report Date: ", report_date, " \n") 
    cat("<hr class=\"style1\">")
}


ras_clip <- function(ras, shp, fld=1){
          a1_crop <- raster::crop(ras, shp)
          step1 <- raster::rasterize(shp, a1_crop, field=fld)
          a1_crop * step1
}

class_wet <- function(ras, teeter=2080){
  ras[ , ] <- sapply(ras[ , ], function(x) ifelse(x<2080, 1, 0))
  ras
}
