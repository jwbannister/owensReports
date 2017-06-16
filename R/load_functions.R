

load_sandflux <- function(area, start_date, end_date){
    query1 <- paste0("SELECT flux.datetime, idep.deployment AS csc, ",
                     "flux.sensit, flux.sand_flux, flux.ws_10m, ",
                     "COALESCE(flux.wd_10m, flux.resultant_wd_10m) ",
                     "AS wd_10m, st_y(st_transform(idep.geom, 26911)) ", 
                     "AS northing_utm, ",
                     "st_x(st_transform(idep.geom, 26911)) AS easting_utm, ",
                     "flux.invalid ",
                     "FROM sandcatch.sandflux_5min flux ",
                     "JOIN instruments.deployments idep ",
                     "ON flux.csc_deployment_id=idep.deployment_id ",
#                     "WHERE flux.invalid='FALSE' ",
                     "WHERE (datetime - '1 second'::interval)::date ",
                     "BETWEEN '", start_date, "'::date ",
                     "AND '", end_date, "'::date ",
                     "AND idep.deployment ",
                     "IN ('", paste0(csc_list[[area]], collapse="', '"), "');")
    flux_df <- query_db("owenslake", query1)
    flux_df
}

load_sites <- function(area){
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
                               owensMaps::point_in_dca, poly_df=owens$polygons)

    sites_df <- sites_df %>% 
        left_join(select(owens$data, objectid, dca), by="objectid")
    sites_df <- sites_df[!duplicated(sites_df), ]
}
