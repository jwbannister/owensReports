# calculate sand masses
full_5min <- expand.grid(csc=unique(csc_locs$csc), 
                   datetime=seq(as.POSIXct(paste0(start_date, "00:05:00"), 
                                           tz='America/Los_Angeles'), 
                            as.POSIXct(paste0(end_date %m+% days(1), " 00:00:00"),
                                       tz='America/Los_Angeles'), 
                                       as.difftime(5, units="mins")), 
                         stringsAsFactors=F)
full_5min_csc <- full_5min %>%
    left_join(flux_df, by=c("csc", "datetime"))
full_5min_csc[is.na(full_5min_csc$sand_flux), "sand_flux"] <- 0

full_flux <- full_5min_csc %>%
    left_join(select(csc_collections, deployment, 'coll_mass'=dwp_mass, 
                     start_datetime, collection_datetime), 
              by=c("csc"="deployment")) %>%
    mutate(check=mapply(function(x, y, z) between(x, y, z), 
                        datetime, start_datetime, collection_datetime)) %>%
    filter(check) %>%
    right_join(full_5min, by=c("csc", "datetime"))
full_flux$bad_coll <- sapply(full_flux$coll_mass, function(x) 
                             ifelse(is.na(x) | x<0,  T, F))

last_coll <- select(csc_locs, csc) %>% 
    left_join(csc_collections, by=c("csc"="deployment")) %>% group_by(csc) %>%
    filter(is.na(collection_datetime) | 
           collection_datetime==max(collection_datetime)) %>%
    select(csc, dwp_mass)
mass_comment <- function(x){
    if (is.na(x)) return("No Collection Made")
    if (x==-999) return("Flooded")
    if (x==-888) return("No Sample Available")
    return(NA)
}
last_coll$comment <- sapply(last_coll$dwp_mass, function(x) mass_comment(x))
bad_collections <- full_flux %>% group_by(csc) %>%
    summarize(bad_count=sum(bad_coll), good_count=sum(!bad_coll)) %>%
    filter(bad_count>0) %>%
    left_join(csc_locs, by="csc")
bad_collections$id3 <- as.character(bad_collections$id3)
if (nrow(bad_collections)==0) bad_collections[1, 1:ncol(bad_collections)] <- 0
bad_collections$flag <- sapply(bad_collections$good_count, function(x) 
                               if_else(x==0, "No Data For Month", 
                                       "Partial Data For Month"))
bad_collections <- bad_collections[!duplicated(bad_collections), ]
if (nrow(bad_collections)>0){
    bad_collections$flag <- factor(bad_collections$flag) }
bad_collections <- bad_collections %>% 
    left_join(select(last_coll, -dwp_mass), by="csc")
# remove sites that were not in place for entire month
absent_sites <- filter(bad_collections, 
                       (flag=="No Data For Month" & 
                        comment=="No Collection Made"))$csc
bad_collections <- bad_collections %>% filter(!(csc %in% absent_sites))
if (nrow(bad_collections)>0){
    for (row in 1:nrow(bad_collections)){
        if (bad_collections$flag[row]=='Partial Data For Month'){
            bad_collections$comment[row] <- NA
        }
    }
}

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
csc_mass <- full_flux %>% filter(!invalid | is.na(invalid)) %>% group_by(csc) %>% 
    summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1)) %>%
    left_join(csc_locs, by="csc") %>% ungroup()
csc_mass <- filter(csc_mass, objectid!='NULL')
csc_mass$objectid <- unlist(csc_mass$objectid)
csc_mass$sand.mass <- sapply(csc_mass$sand.mass, 
                             function(x) ifelse(is.na(x), 0, x))
csc_mass <- csc_mass %>% 
    left_join(select(bad_collections, csc, flag, comment), by="csc")
# remove sites from table that were not in place for entire month
csc_mass <- csc_mass %>% filter(!(csc %in% absent_sites))


if (area=="dwm") area_labels <- move_dwm_labels(area_labels)
if (area=="brine") area_labels <- move_brine_labels(area_labels)
if (area=="channel") area_labels <- move_channel_labels(area_labels)
if (area=="t1a1") area_labels <- move_t1a1_labels(area_labels)
if (area=="twb2") area_labels <- move_twb2_labels(area_labels)



