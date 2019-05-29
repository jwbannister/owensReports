airsci_loc <- Sys.getenv("R_AIRSCI")
suppressMessages(devtools::load_all(airsci_loc))
load_all("~/code/owensReports")
library(tidyverse)
library(lubridate)

avg_pm10 <- function(pm_dat){
    out_list <- vector(mode="list", length=2)
    names(out_list) <- c("avg_pm10", "n")
    out_list[['avg_pm10']] <- round(sum(pm_dat, na.rm=T)/sum(!is.na(pm_dat)), 0)
    out_list[['avg_pm10']] <- sapply(out_list[['avg_pm10']], 
                                     function(x) ifelse(is.na(x), 0, x))
    out_list[['n']] <- sum(!is.na(pm_dat))
    return(out_list)
}

start_date <- as.Date('2019-01-01')
end_date <- as.Date('2019-04-01')
date_seq <- seq.Date(start_date, end_date, by="month")
area <- 'twb2'

csc_collections <- load_collections(area)

for (i in seq(1, length(date_seq)-1, 1)){
    start_date <- date_seq[i]
    end_date <- date_seq[i+1] %m-% days(1)
    print(start_date)

    # sand flux
    flux_df <- load_site_data(area, start_date, end_date)
    flux_df[flux_df$sand_flux<=0, ]$sand_flux <- 0
    daily_flux <- flux_df %>% filter(!invalid | is.na(invalid)) %>%
        group_by(csc, date=as.Date(datetime %m-% seconds(1),
                                   tz='America/Los_Angeles')) %>% 
        summarize(sand.flux=round(sum(sand_flux), 2))
    if (i == 1){
        twb2_flux <- daily_flux
    } else{
        twb2_flux <- rbind(twb2_flux, daily_flux)
    }
}
write.csv(twb2_flux, "~/Desktop/twb2_flux.csv", row.names=F)

for (i in seq(1, length(date_seq)-1, 1)){
    start_date <- date_seq[i]
    end_date <- date_seq[i+1] %m-% days(1)
    print(start_date)

    # PM10 data
    # pull data from m-files for T7 teom, keep wind direction and speed
    mfile <- pull_mfile(start_date, end_date) %>% select(-ws, -wd)

    deploys <- c('T29-4N', 'T29-4S', 'T16', 'T11', 'T2-1')
#    teom <- pull_pm10_aggregate(start_date, end_date, deploys)
    teom <- pull_pm10(start_date, end_date, deploys)
    teom <- filter(teom, !invalid) %>% select(-invalid)
    df0 <- rbind(teom, mfile)
    daily_pm10 <- df0 %>% 
        group_by(deployment, date=as.Date(datetime %m-% seconds(1),
                                   tz='America/Los_Angeles')) %>% 
        do(pm10_24=avg_pm10(.$pm10_avg)[['avg_pm10']]) %>%
        mutate(pm10_24=unlist(pm10_24))

    if (i == 1){
        twb2_pm10 <- daily_pm10
    } else{
        twb2_pm10 <- rbind(twb2_pm10, daily_pm10)
    }
}
write.csv(twb2_pm10, "~/Desktop/twb2_pm10.csv", row.names=F)
save.image("~/twb2_data.RData")



