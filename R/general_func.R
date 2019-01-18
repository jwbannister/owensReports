report_header <- function(start_date, end_date, report_date, area){
    index <- data.frame(a = c("brine", "channel", "dwm", "sfwcrft", "twb2", "t1a1"), 
                        b = c("Brine Areas Monthly Report", 
                              "Channel Areas Monthly Report", 
                              "Dynamic Water Management Areas Monthly Report", 
                              "Shallow Flood Wetness Curve Refinement Field Test",
                              "Tillage with BACM Backup", 
                              "T1A-1 Monthly Report"))
    cat("<img style=\"float: right;\" src=\"../images/logo.png\"> \n")
    cat(" \n# ", index[index$a==area, ]$b, " \n")
    cat(" \n## Summary Period: ", format(start_date, "%m-%d-%Y"), " through ", 
        format(end_date, "%m-%d-%Y"), " \n")
    cat(" \n## Report Date: ", report_date, " \n") 
    cat("<hr class=\"style1\">")
}

parse_dca <- function(x){
    st <- c("", "")
    ints <- as.character(seq(0, 9, 1))
    for (ch in strsplit(x, "")[[1]]){
       if (ch %in% ints) st[1] <- paste0(st[1], ch)
       if (!(ch %in% ints)) st[2] <- paste0(st[2], ch)
    }
    return(st)
}

