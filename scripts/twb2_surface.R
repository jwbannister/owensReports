yr.mo <- paste0(substr(year(start_date), 3, 4), "_", 
                sprintf("%02i", month(start_date)))

query1 <- paste0("SELECT site, rs_avg, rh_avg, rs_rh, ",
                 "clods, yr_mo, area ", 
                 "FROM field_data.twb2_qa_survey;")
all_data <-query_db("owenslake", query1)
cross_walk <- data.frame(area=c("T2-2", "T3-SW", "T3-SE", "T2-3", "T2-4", 
                                       "T3-NE", "T16", "T24-Add", "T12"), 
                         id2=c("T2-2", "T3SW", "T3SE", "T2-3", "T2-4", 
                               "T3NE", "T16", "T24 Addition", "T12-1"))
df2 <- all_data %>% 
    filter(area %in% cross_walk$area) %>%
    left_join(cross_walk, by="area") %>%
       left_join(select(area_polys, id2, id3), by="id2") %>%
       distinct()
df2$index_date <- sapply(df2$yr_mo, 
                         function(x) paste0("20", substr(x, 1, 2), "-", 
                                            substr(x, 4, 5), "-01"))
df2$index_date <- as.Date(df2$index_date)

all_sites <- data.frame(site=unique(df2$site)) %>% 
    left_join(select(df2, site, id2), by="site") %>%
    distinct()

df3 <- df2 %>% select(-id3) %>% 
    filter(month(index_date)==month(start_date) & 
           year(index_date)==year(start_date)) %>%
        right_join(all_sites, by=c("site", "id2")) %>%
        left_join(select(area_polys, id2, id3), by="id2") %>%
        distinct() %>% arrange(site)
df3$yr_mo <- rep(yr.mo, nrow(df3))
df3$index_date <- sapply(df3$yr_mo, 
                         function(x) paste0("20", substr(x, 1, 2), "-", 
                                            substr(x, 4, 5), "-01"))
df3$index_date <- as.Date(df3$index_date)

plot_end <- as.Date(paste0(year(start_date), "-", month(start_date), "-01"))
plot_start <- plot_end %m-% years(1)
month_seq <- seq(plot_start, plot_end, "month")
full_seq <- expand.grid(id2=unique(df2$id2), index_date=month_seq, 
                        stringsAsFactors=F) %>%
    left_join(distinct(select(df2, id2, id3)), by="id2")

if (nrow(df3)>0){
    surface_df <- vector(mode='list', length=length(report_index))
    names(surface_df) <- report_index
    surface_grobs <- vector(mode='list', length=length(report_index))
    names(surface_grobs) <- report_index
    for (i in report_index){
        surface_df[[i]] <- filter(df3, id3==i & 
                                  month(index_date)==month(start_date) &
                                  year(index_date)==year(start_date))
        plot_data <- df2 %>% 
            filter(!(is.na(rs_avg) & is.na(rh_avg) & is.na(rs_rh) & 
                     is.na(clods))) %>%
            filter(id3==i & index_date>=start_date %m-% years(1)) %>%
            group_by(id2, id3, index_date) %>%
            summarize(rs=mean(rs_avg, na.rm=T), rh=mean(rh_avg, na.rm=T), 
                      rs_rh1=mean(rs_rh, na.rm=T), clods1=mean(clods, na.rm=T)) %>%
            arrange(id2, index_date) %>% ungroup() 
        plot_full <- full_seq %>%
            filter(id3==i & index_date>=start_date %m-% years(1)) %>%
            left_join(plot_data, by=c("id2", "id3", "index_date")) %>%
            arrange(id2, index_date)
        na_plot <- data.frame(id2=c(), index_date=c(), rs=c(), rh=c(), 
                              rs_rh1=c(), clods1=c())
        for (l in unique(plot_full$id2)){
            tmp <- filter(plot_full, id2==l)
            na_run <- which(is.na(tmp$rs))
            for (k in 1:nrow(tmp)){
                na_start <- 
                    data.frame(index_date=tmp$index_date[min(na_run)-1],
                               rs=tmp$rs[min(na_run)-1], 
                               rh=tmp$rh[min(na_run)-1], 
                               rs_rh1=tmp$rs_rh1[min(na_run)-1], 
                               clods1=tmp$clods1[min(na_run)-1]) 
                if (length(which(diff(na_run)>1))>0){
                    na_end <- data.frame(id2=c(), index_date=c(), rs=c(), rh=c(), 
                                          rs_rh1=c(), clods1=c())
                    for (m in which(diff(na_run)>1)){
                    tmp1 <- 
                        data.frame(index_date=tmp$index_date[na_run[m]+1],
                                   rs=tmp$rs[na_run[m]+1], 
                                   rh=tmp$rh[na_run[m]+1], 
                                   rs_rh1=tmp$rs_rh1[na_run[m]+1], 
                                   clods1=tmp$clods1[na_run[m]+1]) 
                    na_end <- rbind(na_end, tmp1)
                    }
                } else{
                na_end <- 
                    data.frame(index_date=tmp$index_date[max(na_run)+1],
                               rs=tmp$rs[max(na_run)+1], 
                               rh=tmp$rh[max(na_run)+1], 
                               rs_rh1=tmp$rs_rh1[max(na_run)+1], 
                               clods1=tmp$clods1[max(na_run)+1]) 
                }
                id2_na_plot <- rbind(na_start, na_end) %>% 
                    cbind(data.frame(id2=rep(l, 2)))
            }
            na_plot <- rbind(na_plot, id2_na_plot)
        }
        comply_lines <- data.frame(x=rep(min(plot_data$index_date), 3), 
                                   rh=c(29, 35, 41), rs_rh=c(12.5, 11, 9.5), 
                                   clods=c(55, NA, 65), 
                                   label=c("Reflood", "Maintain", "Compliance"))
        label_data <- plot_data %>% group_by(id2) %>%
            filter(index_date<=plot_end) %>%
            filter(index_date==max(index_date))
        fl <- tempfile()
        png(filename=fl, width=8, height=8, units="in", res=300)
        print(
              ggplot(plot_full, aes(x=index_date, y=rh)) + 
                  geom_path(aes(color=id2)) +
                  geom_point(aes(color=id2)) +
                  geom_path(data=na_plot, mapping=aes(group=id2), color='grey') +
                  ggrepel::geom_label_repel(data=label_data, 
                                            mapping=aes(x=index_date, y=rh, 
                                                        label=id2), 
                                            min.segment.length=unit(0, "lines"), 
                                            nudge_x=5) +
              ylab("Ridge Height (cm)") + xlab("") + 
              xlim(c(plot_start, plot_end)) +
              geom_hline(yintercept=40, color="grey", linetype="longdash") +
              geom_hline(yintercept=30, color="grey", linetype="longdash") +
              geom_label(data=comply_lines, mapping=aes(x=x, y=rh, label=label, 
                                                        fill=label), 
                         hjust=0, alpha=0.5) +
              scale_fill_manual(guide="none", values=c("dodgerblue", "goldenrod", 
                                                       "firebrick")) +
              ggtitle("Average Ridge Height") +
              theme(plot.background=element_blank(), 
                    legend.position="none")
              )
        dev.off()
        tmp_plot <- png::readPNG(fl)
        surface_grobs[[i]]$rh_plot <- grid::rasterGrob(tmp_plot, interpolate=TRUE)
        png(filename=fl, width=8, height=8, units="in", res=300)
        print(
              ggplot(plot_full, aes(x=index_date, y=rs_rh1)) + 
                  geom_path(aes(color=id2)) +
                  geom_point(aes(color=id2)) +
                  geom_path(data=na_plot, mapping=aes(group=id2), color='grey') +
                  ggrepel::geom_label_repel(data=label_data, 
                                            mapping=aes(x=index_date, y=rs_rh1, 
                                                        label=id2), 
                                            min.segment.length=unit(0, "lines"), 
                                            nudge_x=5) +
              ylab("RS/RH Ratio") + xlab("") +
              xlim(c(plot_start, plot_end)) +
              geom_hline(yintercept=10, color="grey", linetype="longdash") +
              geom_hline(yintercept=12, color="grey", linetype="longdash") +
              geom_label(data=comply_lines, mapping=aes(x=x, y=rs_rh, label=label, 
                                                        fill=label), 
                         hjust=0, alpha=0.5) +
              scale_fill_manual(guide="none", values=c("dodgerblue", "goldenrod", 
                                                       "firebrick")) +
              ggtitle("Average Ridge Spacing / Ridge Height Ratio") +
              theme(plot.background=element_blank(), 
                    legend.position="none")
              )
        dev.off()
        tmp_plot <- png::readPNG(fl)
        surface_grobs[[i]]$rsrh_plot <- grid::rasterGrob(tmp_plot, interpolate=TRUE)
        png(filename=fl, width=8, height=8, units="in", res=300)
        print(
              ggplot(plot_full, aes(x=index_date, y=clods1)) + 
              geom_path(aes(color=id2)) +
              geom_point(aes(color=id2)) +
              geom_path(data=na_plot, mapping=aes(group=id2), color='grey') +
              ggrepel::geom_label_repel(data=label_data, 
                                        mapping=aes(x=index_date, y=clods1, 
                                                    label=id2), 
                                        min.segment.length=unit(0, "lines"), 
                                        nudge_x=5) +
              ylab("Clod Cover (%)") + xlab("") +
              xlim(c(plot_start, plot_end)) +
              geom_hline(yintercept=60, color="grey", linetype="longdash") +
              geom_label(data=comply_lines, mapping=aes(x=x, y=clods, label=label, 
                                                        fill=label), 
                         hjust=0, alpha=0.5) +
              scale_fill_manual(guide="none", values=c("dodgerblue", "goldenrod", 
                                                       "firebrick")) +
              ggtitle("Average Clod Coverage") +
              theme(plot.background=element_blank(), 
                    legend.position="none")
              )
        dev.off()
        tmp_plot <- png::readPNG(fl)
        surface_grobs[[i]]$clods_plot <- grid::rasterGrob(tmp_plot, interpolate=TRUE)
    }
}

