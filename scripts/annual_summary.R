# This script generates tables and dust roses for inclusion in an annual summary 
# report for T1A1 and Channel areas. Check with M. Schreuder for more details on 
# annual report requirements. 
airsci_loc <- Sys.getenv("R_AIRSCI")
suppressMessages(devtools::load_all(airsci_loc))
load_all(here::here())
library(pander)
library(tidyverse)
library(lubridate)
library(circular)

areas <- c('north_channel', 'south_channel', 't1a1')
start_date <- as.Date('2016-07-01')
end_date <- as.Date('2017-06-30')

for (a in areas){
    print(a)
    if (a=='north_channel'){
        flux_df <- load_site_data('channel', start_date, end_date) %>% 
            filter(!invalid)
        flux_df <- filter(flux_df, between(as.integer(csc), 1300, 1306))
    } else if (a=='south_channel'){
        flux_df <- load_site_data('channel', start_date, end_date) %>% 
            filter(!invalid)
        flux_df <- filter(flux_df, between(as.integer(csc), 1307, 1399))
    } else{
        flux_df <- load_site_data('t1a1', start_date, end_date) %>% 
            filter(!invalid)
    }
    flux_df[flux_df$dwp_mass<=0, ]$sand_flux <- 0
    flux_df$month <- ordered(paste0(month(flux_df$datetime, label=T), "-", 
                                    substr(year(flux_df$datetime), 3, 4)), 
                             levels=c('Jul-16', 'Aug-16', 'Sep-16', 'Oct-16', 
                                      'Nov-16', 'Dec-16', 'Jan-17', 'Feb-17', 
                                      'Mar-17', 'Apr-17', 'May-17', 'Jun-17'))
    flux_df$date <- as.Date(flux_df$datetime, tz='America/Los_Angeles')
    flux_df$hour <- hour(flux_df$datetime %m-% seconds(1))
    geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
    mass_month <- flux_df %>% group_by(csc, month) %>%
        summarize(sand_mass=round(sum(sand_flux)*geom_adj, 2)) %>%
        spread(month, sand_mass) 
    max_flux <- flux_df %>% group_by(csc, month, date) %>%
        summarize(sand_flux=round(sum(sand_flux), 2)) %>% 
        group_by(csc, month) %>%
        summarize(max_flux=max(sand_flux)) %>%
        spread(month, max_flux)

    daily_wind <- flux_df %>% group_by(csc, date, hour) %>%
        summarize(hourly_ws = mean(ws_10m, na.rm=T)) %>%
        group_by(csc, date) %>%
        summarize(max_wind = max(hourly_ws))
    daily_wind$ws_class <- cut(daily_wind$max_wind, breaks=c(0, 8, 12, 16, Inf), 
                            labels=c('<8', '8 to 12', '12 to 16', '>16'), 
                            ordered_result=T)

    daily_flux <- flux_df %>% group_by(csc, date) %>%
            summarize(sand_flux=round(sum(sand_flux), 2)) %>%
            left_join(daily_wind, by=c('csc', 'date'))

    ws_flux <- daily_flux %>% group_by(ws_class) %>%
        summarize(post_con_flux = round(quantile(sand_flux, probs=0.99), 2)) %>%
        filter(!is.na(ws_class) & ws_class!='<8')
    if (a=='t1a1'){ 
        pre_con_flux <- c('8 to 12'=1.8, '12 to 16'=4.5, '>16'=21.5)
    } else if (a=='north_channel'){
        pre_con_flux <- c('8 to 12'=2.7, '12 to 16'=26.9, '>16'=63.4)
    } else{
        pre_con_flux <- c('8 to 12'=3.7, '12 to 16'=9.9, '>16'=26.6)
    }

    ws_flux$pre_con_flux <- sapply(ws_flux$ws_class, 
                                   function(x) pre_con_flux[as.character(x)])
    ws_flux <- ws_flux %>% select(ws_class, pre_con_flux, post_con_flux) %>%
        mutate(estimated_ce = round(1 - (post_con_flux/pre_con_flux), 2))

    write.csv(mass_month, row.names=F, 
              file=paste0("~/output/annual/", a, "_monthly_masses.csv"))
    write.csv(max_flux, row.names=F, 
              file=paste0("~/output/annual/", a, "_monthly_max_flux.csv"))
    write.csv(ws_flux, row.names=F, 
              file=paste0("~/output/annual/", a, "_control_eff.csv"))

    hour_flux <- flux_df %>% group_by(csc, date, hour) %>% filter(!is.na(wd_10m)) %>%
        summarize(flux=sum(sand_flux), 
                  wd=mean.circular(circular(wd_10m, units="degrees", 
                                            rotation="clock"), na.rm=T))
    hour_flux$wd <- sapply(hour_flux$wd, function(x) ifelse(x<0, 360-x, x))
    hour_flux$wd <- sapply(hour_flux$wd, function(x) ifelse(x>360, x-180, x))
    title_index <- c('t1a1'='T1A1', 'north_channel'='North Channel', 
                     'south_channel'='South Channel')
    p1 <- plot_rose(hour_flux, 'flux', 'wd', valuemin=0.5, valueseq_round=1, 
                    plot.title=paste0(title_index[a]), 
                    legend.title="g/cm2/hr")
    png(file=paste0("~/output/annual/", a, "_sandflux_plot.png"), height=6, width=6, 
        units="in", res=300)
    print(p1)
    dev.off()
}
    


