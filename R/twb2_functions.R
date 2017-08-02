group_twb2_areas <- function(){
    group_df <- data.frame(dca=c("T29-3", "T29-4", "T12-1", "T16", "T3SW",
                                 "T3SE", "T2-2", "T2-3", "T2-4", "T5-4", 
                                 "T3NE", "T24 Addition"), 
                           group=c(rep("North", 2), rep("Central", 2), 
                                   rep("South", 7), "East"))
    twb2 <- owens
    twb2$data <- left_join(group_df, owens$data, by="dca")
    twb2$polygons <- left_join(group_df, owens$polygons, by="dca")
    twb2$labels <- left_join(group_df, owens$labels, by="dca")
    # move dca labels to avoid conflict with points
    twb2$labels[twb2$labels$dca=="T2-2", ]$x[2] <- 410547
    twb2$labels[twb2$labels$dca=="T2-2", ]$y[2] <- 4020386
    twb2$labels[twb2$labels$dca=="T5-4", ]$x <- 414060
    twb2$labels[twb2$labels$dca=="T5-4", ]$y <- 4021851
    twb2
}
