load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
load_all("~/code/Roses")
library(tidyverse)

# pull data from portable teoms, keep wind direction and speed
teom_wind <- pull_teom_wind(start_date, end_date)
teom_pm10 <- pull_pm10(start_date, end_date, deploys)
teom <- left_join(teom_wind, teom_pm10, by=c("deployment", "datetime"))

# pull data from m-files for T7 teom, keep wind direction and speed
mfile <- pull_mfile_wind(start_date %m-% months(5), 
                         end_date %m-% months(5))
mfile$datetime <- mfile$datetime %m+% months(5)

df0 <- rbind(teom_met, mfile)

deploys <- as.character(unique(df0$deployment))
teom_locs <- pull_locations(deploys)


teom_data <- inner_join(df0, teom_locs, by="deployment") %>%
  left_join(pm10_df, by=c("datetime", "deployment")) %>%
  mutate(pm10 = ifelse(is.na(pm10.avg.x), pm10.avg.y, pm10.avg.x)) %>%
  select(-pm10.avg.x, -pm10.avg.y) 

flagged_data <- find_missing(teom_data)

twb2_dcas <- list("North" = c("T29-3", "T29-4"),
                  "Central" = c("T12-1", "T16"),
                  "South" = c("T3SW", "T3SE", "T2-2", "T2-3", 
                                        "T2-4", "T5-4"))

teom_locs <- pair_teoms(teom_locs)
teom_locs <- assign_wind_angle(teom_locs)
# df1 == all available hourly data from teoms
df1 <- inner_join(teom_data, dplyr::select(teom_locs, deployment.id, 
                                           dca.group, position),
                  by="deployment.id")
# filter by direction - keep only hours for which wind is blowing within +/- 
# 11.25 deg of line between teom pairs.
events <- define_event(df1, teom_locs)
joined_events <- inner_join(filter(events, tag=="UW"),
                            filter(events, tag=="DW"), 
                            by=c("datetime", "dca.group"))
clean_events <- joined_events %>% 
  group_by(data.id.x) %>%
  dplyr::select(datetime, dca.group,
                teom.uw=deployment.x, ws.uw=ws.x, wd.uw=wd.x, 
                pm10.uw=pm10.x, 
                teom.dw=deployment.y, ws.dw=ws.y, wd.dw=wd.y, 
                pm10.dw=pm10.y) %>%
 mutate(ws.avg.mps=mean(c(ws.uw, ws.dw))) %>%
  filter(ws.uw!=0,  ws.dw!=0) %>% 
  select(-data.id.x) %>% 
  ungroup() %>% arrange(datetime) 
clean_events$day <- format(clean_events$datetime, "%m-%d-%y")
clean_events <- clean_events[complete.cases(clean_events), ]

# summarize pm10 differnce between matched teom pairs by day.
clean_events2 <- clean_events %>% group_by(day, dca.group) %>%
  mutate(delta=pm10.dw - pm10.uw) %>%
  do(., max.delta.ws=mean(.[.$delta==max(.$delta), ]$ws.uw, 
                          .[.$delta==max(.$delta), ]$ws.dw)) %>%
  ungroup() 
clean_events2$max.delta.ws <- unlist(clean_events2$max.delta.ws)
clean_events <- inner_join(clean_events, clean_events2, 
                           by=c("day", "dca.group"))
daily_summary <- clean_events %>% group_by(day, dca.group) %>%
  summarize(daily.pm10.uw=sum(pm10.uw)/24, daily.pm10.dw=sum(pm10.dw)/24,
            max.delta.ws=unique(max.delta.ws)) %>%
mutate(pm10.delta=daily.pm10.dw - daily.pm10.uw) %>%
ungroup() %>% select(day, dca.group, daily.pm10.uw, daily.pm10.dw, pm10.delta,
                     max.delta.ws)
daily_summary[ , 3:6] <- round(daily_summary[ , 3:6], 2)
daily_summary$status <- sapply(daily_summary$pm10.delta, 
                                function(x) ifelse(x<50, "OK", 
                                                   ifelse(x>100, "Re-Flood", 
                                                          "Maintain")))

n.events <- nrow(clean_events)
n.days <- length(unique(daily_summary$day))
highest.10 <- daily_summary %>% arrange(desc(pm10.delta)) %>%
  filter(pm10.delta>10)

pairs_plots <- vector(mode="list", length=length(twb2_dcas))
names(pairs_plots) <- names(twb2_dcas)
for (i in 1:length(pairs_plots)){
  sub_locs <- filter(teom_locs, dca.group==names(twb2_dcas)[i])
  extent_buffer <- expand.grid(data.frame(x=extendrange(r=sub_locs$x, f=0.2),
                                          y=extendrange(r=sub_locs$y, f=0.2)))
  sub_df <- filter(df1, deployment==sub_locs$deployment[i])
  p1 <- plot_dca_background(twb2_dcas[[i]], owens_areas$dca$polygons, 
                            owens_areas$dca$labels, extent_buffer)
  pairs_plots[[i]] <- teom_pair_plot(teom_locs=sub_locs, 
                                     df1=df1[complete.cases(df1), ], 
                                     background=p1, start_date, end_date)
}

