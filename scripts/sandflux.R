load_all()
load("~/code/owensMaps/data/map_data.RData")
library(tidyverse)
library(lubridate)

# add met station locations if needed for plot display
met_loc <- NULL
#met_loc <- data.frame(group=c('North', 'South'), deployment=c('1552', '1150'), 
#                        x=c(415077.3, 409413.6), y=c(4041263.2, 4019839.6))

expand_daily <- expand.grid(csc=unique(daily_flux$csc), 
                            date=seq(start_date, end_date, "days"), 
                            stringsAsFactors=FALSE)
full_daily <- expand_daily %>%
    left_join(select(daily_flux, -x, -y), by=c("csc", "date")) %>%
    left_join(csc_locs, by="csc")
full_daily[is.na(full_daily$sand.flux), "sand.flux"] <- 0

if (area=='sfwcrft') flux_ce <- calc_flux_ce_sfwcrft(full_daily)

max_daily <- full_daily %>% group_by(csc) %>%
    summarize(max.daily.flux = max(sand.flux), x=unique(x), y=unique(y),
              id2=unique(id2), id3=unique(id3), id1=unique(id1)) 

# One-off adjustments specific to report
# change file location defined in sandmass script
if (file.exists(change_file)) source(change_file)

flux_grobs <- vector(mode="list", length=length(unique(max_daily$id3)))
names(flux_grobs) <- unique(max_daily$id3)
for (i in names(flux_grobs)){
    print(i)
        tmp_bad <- filter(bad_collections, id3==i & flag=="No Data For Month")
        tmp_partial <- filter(bad_collections, 
                              id3==i & flag=="Partial Data For Month")
        tmp_polys <- filter(area_polys, id3==i)
        tmp_labels <- filter(area_labels, id3==i)
        tmp_flux <- filter(max_daily, id3==i)
    if (is.null(met_loc)){
        met_pts <- c()
    } else{
        met_pts <- filter(met_loc, id3==i)
    }
    background <- plot_dca_background(tmp_polys, tmp_labels,
                                      external_points=met_pts) +
        geom_point(data=tmp_partial, mapping=aes(x=x, y=y), size=8, 
                   color="black")
    legend_flux='Max. Daily Flux\n(g/cm^2/day)'
    top_flux <- if_else(area=='twb2', 1, 10)
    p1 <- plot_csc_site(background, tmp_flux, i, legend_title=legend_flux, 
                        value_index=2, value_max=top_flux, plot_title="Daily Flux") +
        scale_shape_manual(name=NULL, values=c(21)) +
        geom_point(data=tmp_bad, mapping=aes(x=x, y=y, size=flag), 
                   color="black") +
        scale_size_manual(name=NULL, values=c(4)) +
        guides(color=guide_colorbar(order=1), shape=guide_legend(order=2), 
               size=guide_legend(order=3))
     if (!is.null(met_pts)){
         p1 <- p1 + geom_point(data=met_pts, 
                               aes(shape=deployment, x=x, y=y), 
                               color="blue", size=6) +
                scale_shape_manual(name='Met Station', values=c(17), 
                                   labels=c('1552')) 
     }
    fl <- tempfile()
    png(filename=fl, width=8, height=8, units="in", res=300)
    print(p1)
    dev.off()
    flux_plot <- png::readPNG(fl)
    flux_grobs[[i]] <- grid::rasterGrob(flux_plot, interpolate=TRUE)
}