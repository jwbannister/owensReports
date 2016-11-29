group_twb2_areas <- function(){
    group_df <- data.frame(dca=c("T29-3", "T29-4", "T12-1", "T16", "T3SW",
                                 "T3SE", "T2-2", "T2-3", "T2-4", "T5-4", 
                                 "T3NE", "T24 Addition"), 
                           group=c(rep("North", 2), rep("Central", 2), 
                                   rep("South", 7), "East"))
    twb2 <- owens
    twb2$data <- left_join(owens$data, group_df, by="dca")
    twb2$polygons <- left_join(owens$polygons, group_df, by="dca")
    twb2$labels <- left_join(owens$labels, group_df, by="dca")
    twb2
}
