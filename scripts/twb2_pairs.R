load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
load_all("~/code/Roses")
library(tidyverse)

twb2 <- group_twb2_areas()

# pull data from portable teoms, keep wind direction and speed
teom_wind <- pull_teom_wind(start_date, end_date)

# pull data from m-files for T7 teom, keep wind direction and speed
mfile <- pull_mfile(start_date, end_date)

deploys <- as.character(c(unique(teom_wind$deployment), 
                          unique(mfile$deployment)))
teom_pm10 <- pull_pm10(start_date, end_date, deploys)

teom <- left_join(teom_wind, filter(teom_pm10, !invalid), 
                  by=c("deployment", "datetime"))
teom <- teom[complete.cases(teom), ] %>% select(-invalid)

df0 <- rbind(teom, mfile)

teom_locs <- pull_locations(deploys)
teom_locs <- pair_teoms(teom_locs)
teom_locs <- assign_wind_angle(teom_locs)

df1 <- left_join(df0, select(teom_locs, deployment, dca.group, position),
                 by="deployment")
# filter by direction - keep only hours for which wind is blowing within +/- 
# 11.25 deg of line between teom pairs.
events <- define_event(df1, teom_locs)
joined_events <- inner_join(filter(events, tag=="UW"),
                            filter(events, tag=="DW"), 
                            by=c("datetime", "dca.group"))
clean_events <- joined_events %>% 
  select(datetime, dca.group, teom.uw=deployment.x, ws.uw=ws.x, wd.uw=wd.x, 
                pm10.uw=pm10_avg.x, teom.dw=deployment.y, ws.dw=ws.y, 
                wd.dw=wd.y, pm10.dw=pm10_avg.y) %>%
  mutate(ws.avg.mps=mean(c(ws.uw, ws.dw))) %>%
  filter(ws.uw!=0,  ws.dw!=0) %>% 
  arrange(datetime) 
clean_events$date <- format(clean_events$datetime, "%m-%d-%y")
clean_events <- clean_events[complete.cases(clean_events), ]

# summarize pm10 differnce between matched teom pairs by day.
clean_events2 <- clean_events %>% group_by(date, dca.group) %>%
  mutate(delta=pm10.dw - pm10.uw) %>%
  do(., max.delta.ws=mean(c(.[.$delta==max(.$delta), ]$ws.uw, 
                          .[.$delta==max(.$delta), ]$ws.dw))) %>%
  ungroup() 
clean_events2$max.delta.ws <- unlist(clean_events2$max.delta.ws)
clean_events <- inner_join(clean_events, clean_events2, 
                           by=c("date", "dca.group"))
daily_summary <- clean_events %>% group_by(date, dca.group) %>%
  summarize(daily.pm10.uw=sum(pm10.uw)/24, daily.pm10.dw=sum(pm10.dw)/24,
            max.delta.ws=unique(max.delta.ws)) %>%
mutate(pm10.delta=daily.pm10.dw - daily.pm10.uw) %>%
ungroup() %>% select(date, dca.group, daily.pm10.uw, daily.pm10.dw, pm10.delta,
                     max.delta.ws)
daily_summary[ , 3:6] <- round(daily_summary[ , 3:6], 2)
daily_summary$status <- sapply(daily_summary$pm10.delta, 
                                function(x) ifelse(x<50, "OK", 
                                                   ifelse(x>100, "Re-Flood", 
                                                          "Maintain")))

n.events <- nrow(clean_events)
n.days <- length(unique(daily_summary$date))
highest.10 <- daily_summary %>% arrange(desc(pm10.delta)) 

teom_groups <- unique(teom_locs$dca.group)
pair_grobs <- vector(mode="list", length=length(teom_groups))
names(pair_grobs) <- teom_groups
for (i in teom_groups){
    print(i)
    sub_locs <- filter(teom_locs, dca.group==i)
    extent_buffer <- expand.grid(data.frame(x=extendrange(r=sub_locs$x, f=0.2),
                                            y=extendrange(r=sub_locs$y, f=0.2)))
    sub_df <- filter(df1, dca.group==i)
    tmp_polys <- filter(twb2$polygons, group==i) %>%
        select(x, y, id=objectid)
    tmp_labels <- filter(twb2$labels, group==i) %>%
        select(x, y, id=dca)
    p1 <- plot_dca_background(tmp_polys, tmp_labels, extent_buffer)
    p2 <- teom_pair_plot(teom_locs=sub_locs, df1=df1[complete.cases(df1), ], 
                         background=p1, start_date, end_date)
    fl <- tempfile()
    png(filename=fl, width=8, height=8, units="in", res=300)
    print(p2)
    dev.off()
    pair_plot <- png::readPNG(fl)
    pair_grobs[[i]] <- grid::rasterGrob(pair_plot, interpolate=TRUE)
}

