airsci_loc <- Sys.getenv("R_AIRSCI")
suppressMessages(devtools::load_all(airsci_loc))
load_all("~/code/owensReports")
library(tidyverse)
library(lubridate)

area_polys <- pull_onlake_polygons()
date_seq <- seq.Date(as.Date('2016-01-01'), as.Date('2018-01-01'), by="quarter")

i <- 1
start_date <- date_seq[i]
end_date <- date_seq[i+1] %m-% days(1)

# sand flux
flux_df <- load_site_data(area, start_date, end_date)
flux_df[flux_df$sand_flux<=0, ]$sand_flux <- 0
flux_df <- filter(flux_df, !invalid)
daily_flux <- full_flux %>% filter((!invalid | is.na(invalid)) & 
                                   (!bad_coll | is.na(bad_coll))) %>%
    group_by(csc, date=as.Date(datetime %m-% seconds(1),
                               tz='America/Los_Angeles')) %>% 
    summarize(sand.flux=round(sum(sand_flux), 2)) %>%
    ungroup() 


# OLD TEOM DATA QUERY (PRE-OCTOBER 2017). This query used a view on the 
# 1-minute TEOM data. The 1-minute table was deprecated in October 2017 in 
# favor of the 30 minute table. To re-run older reports, this query must be 
# used in the pull_pm10 function. 
#    query1 <- paste0("SELECT deployment, datetime, pm10_std_avg AS pm10_avg, ",
#                     "invalid ",
#                     "FROM teom.avg_1hour_validated ",
#                     "WHERE (datetime-'1 second'::interval)::date ",
#                     "BETWEEN '", date1, "'::date ", 
#                     "AND '", date2, "'::date ",  
#                     "AND deployment IN ", deploys) 

# PM10 data
teom_wctivate owensReports
vim
teom_wind <- pull_teom_wind(start_date, end_date)

# pull data from m-files for T7 teom, keep wind direction and speed
mfile <- pull_mfile(start_date, end_date)

deploys <- c('T29-4N', 'T29-4S', 'T16', 'T11', 'T7', 'T2-1')
teom_pm10 <- pull_pm10_aggregate(start_date, end_date, deploys)

teom <- left_join(teom_wind, filter(teom_pm10, !invalid), 
                  by=c("deployment", "datetime"))
teom <- teom[complete.cases(teom), ] %>% select(-invalid)

df0 <- rbind(teom, mfile)

teom_locs <- pull_locations(deploys)
teom_locs <- pair_teoms(teom_locs)
teom_locs <- assign_wind_angle(teom_locs)

df1 <- left_join(df0, select(teom_locs, deployment, id3, position),
                 by="deployment")







last_coll <- select(flux_df[!duplicated(flux_df$csc), ], csc) %>% 
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
