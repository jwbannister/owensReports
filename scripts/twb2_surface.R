yr.mo <- paste0(substr(year(start_date), 3, 4), "_", 
                sprintf("%02i", month(start_date)))
query1 <- paste0("SELECT site, area AS id2, rs_avg, rh_avg, rs_rh, clods ", 
                 "FROM field_data.twb2_qa_survey ", 
                 "WHERE yr_mo='", yr.mo, "';")
df1 <-query_db("owenslake", query1)
if (nrow(df1)>0){
    dca_rename <- c("T12"="T12-1", "T3-SW"="T3SW", "T3-SE"="T3SE", "T3-NE"="T3NE", 
                    "T24-Add"="T24 Addition", "T29"="T29-4")
    for (i in names(dca_rename)){
        df1[df1$id2==i, ]$id2 <- dca_rename[[i]]
    }
    surface_df <- df1 %>% left_join(select(twb2$data, id2, group), by="id2") %>%
        arrange(site)
    surface_df <- surface_df[!duplicated(surface_df), ]
}

