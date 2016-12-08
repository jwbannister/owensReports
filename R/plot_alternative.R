
plot_csc_site_label_nocolor <- function(background, sand_df, area_txt,
                            begin=start_date, ending=end_date, 
                            legend_title="", value_index, value_max, 
                            plot_title=""){
  catches <- sand_df %>% filter(dca==area_txt)
  value.range <- 
      range(catches[ , value_index])[2] - range(catches[ , value_index])[1]
  x_range <- diff(ggplot_build(background)[[2]]$ranges[[1]]$x.range)
  y_range <- diff(ggplot_build(background)[[2]]$ranges[[1]]$y.range)
  catches[ , value_index] <- 
      sapply(catches[ , value_index], 
             function(x) ifelse(x>value_max, value_max, x))
  p1 <- background +
    geom_point(data=catches, size=4, color="lightblue", 
               mapping=aes_string(x='x', y='y')) +
    geom_label(data=catches, mapping=aes(x=x, y=y, label=csc), 
               nudge_x=x_range/25, nudge_y=y_range/45) +
    coord_fixed() +
    ggtitle(plot_title) +
    theme(axis.ticks.x=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_text(size=12),
          legend.position=leg_pos[[area_txt]], 
          legend.background=element_rect(linetype="solid", color="black"), 
          legend.justification=leg_jus[[area_txt]])
  p1
}  
