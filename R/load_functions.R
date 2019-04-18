
csc_list <- list("brine"=seq(1700, 1799, 1), 
                 "channel"=seq(1300, 1399, 1),
                 "dwm"=seq(1900, 1999, 1), 
                 "t1a1"=seq(1100, 1199, 1), 
                 "twb2"=seq(1600, 1699, 1))

#load_sites <- function(area, poly_df){
#    query1 <- paste0("SELECT DISTINCT i.deployment AS csc, ",
#                     "st_y(st_transform(i.geom, 26911)) AS y, ",
#                     "st_x(st_transform(i.geom, 26911)) AS x, ",
#                     "i.active ",
#                     "FROM sandcatch.csc_summary s ",
#                     "LEFT JOIN instruments.deployments i ",
#                     "ON s.csc_deployment_id=i.deployment_id ",
#                     "WHERE i.deployment ",
#                     "IN ('", paste0(csc_list[[area]], collapse="', '"), "');")
#    sites_df <- query_db("owenslake", query1)
#    sites_df$objectid <- apply(cbind(sites_df$x, sites_df$y), 1, 
#                               aiRsci::point_in_dca, poly_df=poly_df, return_dca=F)
#    sites_df <- sites_df[!duplicated(sites_df), ]
#    sites_df <- filter(sites_df, objectid!='NULL')
#    sites_df$objectid <- unlist(sites_df$objectid)
#    sites_df
#}
load_sites <- function(area, poly_df, start_date){
    query1 <- paste0("SELECT deployment::text AS csc, x, y ",
                     "FROM info.site_locations_at_date(',", start_date, "') ", 
                     "WHERE deployment ",
                     "IN (", paste0(csc_list[[area]], collapse=", "), ");")
    sites_df <- query_db("owenslake", query1)
    sites_df$objectid <- apply(cbind(sites_df$x, sites_df$y), 1, 
                               aiRsci::point_in_dca, poly_df=poly_df, return_dca=F)
    sites_df <- sites_df[!duplicated(sites_df), ]
    sites_df <- filter(sites_df, objectid!='NULL')
    sites_df$objectid <- unlist(sites_df$objectid)
    sites_df
}

load_site_data <- function(area, start_date, end_date){
    query1 <- 
        paste0("SELECT sens.datetime::timestamp AT TIME ZONE 'America/Los_Angeles' AS datetime, ",
           "csc.csc_deployment_id, csc.sensit_deployment_id, ic.deployment AS csc, ", 
           "sens.sumpc, COALESCE(csc.dwp_mass, csc.district_mass) AS dwp_mass, ",
           "csc.sumpc_total, met.ws_10m, met.wd_10m, ",
           "CASE ", 
               "WHEN csc.sumpc_total > 0 ", 
               "THEN (sens.sumpc / csc.sumpc_total) * ", 
               "(COALESCE(csc.dwp_mass, csc.district_mass) / 1.2) ",
               "ELSE 0 ",
           "END AS sand_flux, ", 
           "flags.field_is_invalid(csc.sensit_deployment_id, 90, sens.datetime) AS invalid ",
           "FROM sensit.sensit_5min sens LEFT JOIN sandcatch.csc_summary csc ",
           "ON csc.sensit_deployment_id = sens.deployment_id ", 
           "AND sens.datetime > csc.start_datetime ", 
           "AND sens.datetime <= csc.collection_datetime ", 
           "JOIN instruments.deployments ic ",
           "ON csc.csc_deployment_id=ic.deployment_id ",
           "LEFT JOIN met.met_5min met ", 
           "ON sens.datetime = met.datetime ", 
           "AND met.deployment_id = csc.met_deployment_id ",
           "WHERE ic.deployment IN ('", 
           paste0(csc_list[[area]], collapse="', '"), "') ", 
           "AND COALESCE(csc.dwp_mass, csc.district_mass) IS NOT NULL ",
           "AND sens.datetime >= '", start_date %m+% minutes(5), "'::timestamp ",
           "AND sens.datetime <= '", end_date %m+% days(1), "'::timestamp;")
    a <- query_db("owenslake", query1)
    attributes(a$datetime)$tzone <- 'America/Los_Angeles'
    return(a)
}

load_collections <- function(area){
    query1 <- paste0("SELECT i.deployment, s.* FROM sandcatch.csc_summary s ", 
                     "JOIN instruments.deployments i ",
                     "ON s.csc_deployment_id=i.deployment_id ",
                     "WHERE ('", start_date-1, "'::date, '", end_date+1, "'::date) ",
                     "OVERLAPS (s.start_datetime::date, ",
                     "s.collection_datetime::date) ", 
                     "AND i.deployment IN ('", 
                     paste0(csc_list[[area]], collapse="', '"), "');") 
    query_db("owenslake", query1)
}

