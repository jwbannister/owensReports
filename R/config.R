psql_host <- Sys.getenv("PSQL_HOST_OWENS")
psql_password <- Sys.getenv("PSQL_PASSWORD_OWENS")
psql_user <- Sys.getenv("PSQL_USER")
psql_port <- Sys.getenv("PSQL_PORT")

csc_list <- list("brine"=seq(1700, 1799, 1), 
                 "channel"=seq(1300, 1399, 1),
                 "dwm"=seq(1900, 1999, 1), 
                 "t1a1"=seq(1100, 1199, 1), 
                 "twb2"=seq(1600, 1699, 1))
if (start_date < "2016-08-01"){
    csc_list$sfwcrft <- c(seq(1400, 1499, 1), seq(1500, 1599, 1)) 
} else{
    csc_list$sfwcrft <- c(seq(1800, 1899, 1))
}

