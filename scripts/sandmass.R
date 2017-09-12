
expand_flux <- expand.grid(csc=unique(flux_df$csc), 
                   datetime=seq(as.POSIXct(paste0(start_date, "00:00:00")), 
                            as.POSIXct(paste0(end_date, " 00:00:00")), 
                                       as.difftime(5, units="mins"), 
                   stringsAsFactors=FALSE))
full_flux <- expand_flux %>%
    left_join(flux_df, by=c("csc", "datetime"))
full_flux[is.na(full_flux$sand_flux), "sand_flux"] <- 0

full_flux <- sqldf::sqldf(paste0("SELECT f.*, c.dwp_mass AS coll_mass ", 
                           "FROM full_flux f ", 
                           "LEFT JOIN csc_collections c ", 
                           "ON f.csc=c.deployment ", 
                           "AND f.datetime BETWEEN c.start_datetime ", 
                           "AND c.collection_datetime"))
full_flux$bad_coll <- sapply(full_flux$coll_mass, 
                             function(x) ifelse(x<0, T, F))

bad_collections <- full_flux %>% group_by(csc) %>%
    summarize(bad_count=sum(bad_coll), good_count=sum(!bad_coll)) %>%
    filter(bad_count>0) %>%
    left_join(csc_locs, by="csc")
bad_collections$flag <- sapply(bad_collections$good_count, function(x) 
                               if_else(x==0, "No Data For Month", 
                                       "Partial Data For Month"))
bad_collections <- bad_collections[!duplicated(bad_collections), ]
if (nrow(bad_collections)>0){
    bad_collections$flag <- factor(bad_collections$flag) }
if (nrow(bad_collections)==0) bad_collections[1, 1:ncol(bad_collections)] <- 0

geom_adj <- 1.2 #sandcatch geometry adjustment for sandflux calculation
csc_mass <- full_flux %>% filter(!invalid & !bad_coll) %>% group_by(csc) %>% 
    summarize(sand.mass=round(sum(sand_flux)*geom_adj, 1)) %>%
    left_join(csc_locs, by="csc") %>% ungroup()
csc_mass <- filter(csc_mass, objectid!='NULL')
csc_mass$objectid <- unlist(csc_mass$objectid)
csc_mass$sand.mass <- sapply(csc_mass$sand.mass, 
                             function(x) ifelse(is.na(x), 0, x))
if (area=='sfwcrft') mass_ce <- calc_mass_ce_sfwcrft(csc_mass)

if (!(area %in% c('twb2', 'sfwcrft'))){
    if (area=="dwm") area_labels <- move_dwm_labels(area_labels)
    if (area=="brine") area_labels <- move_brine_labels(area_labels)
    if (area=="channel") area_labels <- move_channel_labels(area_labels)
    if (area=="t1a1") area_labels <- move_t1a1_labels(area_labels)
}

if (area=='sfwcrft'){
    mass_grobs <- vector(mode="list", length=length(unique(csc_mass$id2)))
    names(mass_grobs) <- unique(csc_mass$id2)
    for (i in unique(csc_mass$id2)){
        print(i)
        tmp_bad <- filter(bad_collections, id2==i & flag=="No Data For Month")
        tmp_partial <- filter(bad_collections, 
                              id2==i & flag=="Partial Data For Month")
        tmp_polys <- filter(area_polys, id2==i)
        tmp_labels <- filter(area_labels, id2==i)
        plot_range_x <- range(tmp_polys$x)
        plot_range_y <- range(tmp_polys$y)
        extent <- data.frame(x=extendrange(r=plot_range_x, f=0.3),
                             y=extendrange(r=plot_range_y, f=0.3))
        background <- photo_background(extent$x[1], extent$x[2], 
                                       extent$y[1], extent$y[2], 
                                       zone="11N") +
        geom_path(data=tmp_polys, mapping=aes(x=x, y=y, group=objectid), 
                  color="black") +
        geom_text(data=tmp_labels, aes(x=x, y=y, label=id1), color="black") 
        legend_mass='Total Mass (g)'
        tmp_mass <- filter(csc_mass, id2==i)
        p1 <- plot_csc_site(background, tmp_mass, i, 
                            legend_title=legend_mass, 
                            value_index=2, value_max=200,
                            plot_title="Monthly Mass") +
geom_point(data=tmp_partial, mapping=aes(x=x, y=y, shape=flag), 
           color="black", size=6) +
scale_shape_manual(name=NULL, values=c(21)) +
geom_point(data=tmp_bad, mapping=aes(x=x, y=y, size=flag), 
           color="black") +
scale_size_manual(name=NULL, values=c(4)) +
guides(color=guide_colorbar(order=1), shape=guide_legend(order=2), 
       size=guide_legend(order=3))
fl <- tempfile()
png(filename=fl, width=8, height=8, units="in", res=300)
print(p1)
dev.off()
mass_plot <- png::readPNG(fl)
mass_grobs[[i]] <- grid::rasterGrob(mass_plot, interpolate=TRUE)
    }
}


