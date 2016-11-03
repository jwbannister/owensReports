S3_bucket_access <- function(key, file){
    aws_access <- read.table("~/config/credentials/AWS_cred.txt")[2, 1]
    aws_secret <- read.table("~/config/credentials/AWS_cred.txt")[4, 1]
    RS3::S3_connect(aws_access, aws_secret, hostname="s3-us-west-2.amazonaws.com")
    RS3::S3_get_object("saltonimages", key, file)
}

report_header <- function(start_date, end_date, report_date, area){
    index <- data.frame(a = c("brine", "channel", "dwm", "sfwcrft", "twb2"), 
                        b = c("Brine", "Channel", 
                              "Dynamic Water Management", 
                              "Shallow Flood Wetness Curve Refinement Field Test",
                              "Tillage with BACM Backup"))
    cat("<img style=\"float: right;\" src=\"logo.png\"> \n")
    cat(" \n# Owens Lake ", index[index$a==area, ]$b, " Areas \n")
    cat(" \n##Summary Period: ", format(start_date, "%m-%d-%Y"), " through ", 
        format(end_date, "%m-%d-%Y"), " \n")
    cat(" \n##Report Date: ", report_date, " \n") 
    cat("<hr class=\"style1\">")
}

point_in_dca <- function(vec_in, poly_df=sfwct_polys){
    for (j in unique(poly_df$objectid)){
      polycheck <- sp::point.in.polygon(vec_in[1], vec_in[2],
                                    dplyr::filter(poly_df, objectid==j)$x, 
                                    dplyr::filter(poly_df, objectid==j)$y)
      if (polycheck==1) return(dplyr::filter(poly_df, objectid==j)$area[1]) 
    }
}
