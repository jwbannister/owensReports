
plot_csc_site_label_nocolor <- function(background, sand_df, area_txt,
                            begin=start_date, ending=end_date, 
                            legend_title="", value_index, value_max, 
                            plot_title=""){
  value.range <- 
      range(sand_df[ , value_index])[2] - range(sand_df[ , value_index])[1]
  x_range <- diff(ggplot_build(background)[[2]]$panel_ranges[[1]]$x.range)
  y_range <- diff(ggplot_build(background)[[2]]$panel_ranges[[1]]$y.range)
  sand_df[ , value_index] <- 
      sapply(sand_df[ , value_index], 
             function(x) ifelse(x>value_max, value_max, x))
  p1 <- background +
    geom_point(data=sand_df, size=4, color="lightblue", 
               mapping=aes_string(x='x', y='y')) +
    geom_label(data=sand_df, mapping=aes(x=x, y=y, label=csc), 
               nudge_x=x_range/25, nudge_y=y_range/45) +
    coord_fixed() +
    ggtitle(plot_title) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(size=12, hjust=0.5),
          legend.position=leg_pos[[area_txt]], 
          legend.background=element_rect(linetype="solid", color="black"), 
          legend.justification=leg_pos[[area_txt]])
  p1
}  

move_brine_labels <- function(df1){
    df1[df1$id2=='T23NE', ]$x <- df1[df1$id2=='T23NE', ]$x - 150
    df1[df1$id2=='T2-4', ]$x <- df1[df1$id2=='T2-4', ]$x + 300
    df1
}

move_dwm_labels <- function(df1){
    df1
}

move_channel_labels <- function(df1){
    df1[df1$id2=="Channel South", ]$x <- 410100 
    df1[df1$id2=="Channel South", ]$y <- 4020800
    df1[df1$id2=="Channel North", ]$x <- 411665
    df1[df1$id2=="Channel North", ]$y <- 4022961
    df1
}

move_t1a1_labels <- function(df1){
    df1[df1$id2=='T1A-1', ]$x <- df1[df1$id2=='T1A-1', ]$x + 150
    df1[df1$id2=='T1A-1', ]$y <- df1[df1$id2=='T1A-1', ]$y - 50
    df1
}

move_twb2_labels <- function(df1){
    df1[df1$dca=="T2-2", ]$x[2] <- 410547
    df1[df1$dca=="T2-2", ]$y[2] <- 4020386
    df1[df1$dca=="T5-4", ]$x <- 414060
    df1[df1$dca=="T5-4", ]$y <- 4021851
    df1
}
