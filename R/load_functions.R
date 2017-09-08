
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

load_sandflux <- function(area, start_date, end_date){
    query1 <- paste0("SELECT flux.datetime, idep.deployment AS csc, ",
                     "flux.sensit, flux.dwp_mass, flux.sand_flux, flux.ws_10m, ",
                     "COALESCE(flux.wd_10m, flux.resultant_wd_10m) ",
                     "AS wd_10m, st_y(st_transform(idep.geom, 26911)) ", 
                     "AS northing_utm, ",
                     "st_x(st_transform(idep.geom, 26911)) AS easting_utm, ",
                     "flux.invalid ",
                     "FROM sandcatch.sandflux_5min flux ",
                     "JOIN instruments.deployments idep ",
                     "ON flux.csc_deployment_id=idep.deployment_id ",
                     "WHERE (datetime - '1 second'::interval)::date ",
                     "BETWEEN '", start_date, "'::date ",
                     "AND '", end_date, "'::date ",
                     "AND idep.deployment ",
                     "IN ('", paste0(csc_list[[area]], collapse="', '"), "');")
    flux_df <- query_db("owenslake", query1)
    flux_df
}

load_sites <- function(area, poly_df){
    query1 <- paste0("SELECT DISTINCT i.deployment AS csc, ",
                     "st_y(st_transform(i.geom, 26911)) AS y, ",
                     "st_x(st_transform(i.geom, 26911)) AS x ",
                     "FROM sandcatch.csc_summary s ",
                     "LEFT JOIN instruments.deployments i ",
                     "ON s.csc_deployment_id=i.deployment_id ",
                     "WHERE i.deployment ",
                     "IN ('", paste0(csc_list[[area]], collapse="', '"), "');")
    sites_df <- query_db("owenslake", query1)
    sites_df <- arrange(sites_df, csc)
    sites_df$objectid <- apply(cbind(sites_df$x, sites_df$y), 1, 
                               point_in_dca, poly_df=poly_df)
    sites_df <- sites_df[!duplicated(sites_df), ]
}
