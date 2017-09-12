yr.mo <- paste0(substr(year(start_date), 3, 4), "_", 
                sprintf("%02i", month(start_date)))
query1 <- paste0("SELECT site, area AS area_survey, rs_avg, rh_avg, rs_rh, clods ", 
                 "FROM field_data.twb2_qa_survey ", 
                 "WHERE yr_mo='", yr.mo, "';")
df1 <-query_db("owenslake", query1)
cross_walk <- data.frame(area_survey=c("T2-2", "T3-SW", "T3-SE", "T2-3", "T2-4", 
                                      "T3-NE", "T5-4", "T16", "T24-Add", "T29", 
                                      "T12"), 
                         id2=c("T2-2", "T3SW", "T3SE", "T2-3", "T2-4", 
                                       "T3NE", "T5-4", "T16", "T24 Addition", 
                                       "T29-4", "T12-1"))
df2 <- df1 %>% left_join(cross_walk, by="area_survey")
if (nrow(df2)>0){
    for (i in names(dca_rename)){
        df2[df2$id2==i, ]$id2 <- dca_rename[[i]]
    }
    surface_df <- df2 %>% left_join(select(area_polys, id2, id3), by="id2") %>%
        arrange(site)
    surface_df <- surface_df[!duplicated(surface_df), ]
    surface_df <- surface_df[complete.cases(surface_df), ]
}

