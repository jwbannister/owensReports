group_twb2_areas <- function(df1){
    group_df <- data.frame(id2=c("T12-1", "T16", "T3SW", "T3SE", "T2-2",
                                 "T2-3", "T2-4", "T3NE", "T24 Addition"), 
                           group=c(rep("Central", 2), rep("South", 6), "East"))
    df1 <- left_join(group_df, df1, by="id2") 
    if ("bacm_type" %in% colnames(df1)){
        df1 <- df1 %>% filter(bacm_type=="Tillage With BACM Backup")
    }
    df1$id3 <- df1$group
    df1
}
