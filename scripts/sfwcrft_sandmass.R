load_all()
load("~/code/owensMaps/data/map_data.RData")
library(tidyverse)
library(lubridate)

if (start_date < "2016-08-01"){
    sfwcrft <- sfwcrft1
} else{
    sfwcrft <- sfwcrft2
}

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
csc_mass <- flux_df %>% group_by(csc) %>% 
    summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1), 
              x=unique(easting_utm), y=unique(northing_utm)) %>% ungroup()
csc_mass$objectid <- apply(cbind(csc_mass$x, csc_mass$y), 1, 
                           owensMaps::point_in_dca, poly_df=sfwcrft$polygons)
csc_mass <- csc_mass %>% 
    left_join(select(sfwcrft$data, objectid, dca, treatment), by="objectid")
mass_ce <- calc_mass_ce_sfwcrft(csc_mass)

mass_grobs <- vector(mode="list", length=length(unique(csc_mass$dca)))
names(mass_grobs) <- unique(csc_mass$dca)
for (i in unique(csc_mass$dca)){
    print(i)
    tmp_polys <- filter(sfwcrft$polygons, dca==i) %>%
        select(x, y, id=treatment)
    tmp_labels <- filter(sfwcrft$labels, dca==i) %>%
        select(x, y, id=treatment)
    background <- plot_dca_background(tmp_polys, tmp_labels)
    legend_mass='Total Mass (g)'
    p1 <- plot_csc_site_nolabel(background, csc_mass, i, 
                                legend_title=legend_mass, 
                                value_index=2, value_max=200,
                                plot_title="Monthly Mass")
    fl <- tempfile()
    png(filename=fl, width=8, height=8, units="in", res=300)
    print(p1)
    dev.off()
    mass_plot <- png::readPNG(fl)
    mass_grobs[[i]] <- grid::rasterGrob(mass_plot, interpolate=TRUE)
}
