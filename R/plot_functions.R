
plot_csc_site <- function(background, sand_df, area_txt,
                            begin=start_date, ending=end_date, 
                            legend_title="", value_index, value_max, 
                            plot_title="", labels=TRUE, colorscale=TRUE){
  x_range <- diff(range(ggplot_build(background)[[1]][[1]]$x))
  y_range <- diff(range(ggplot_build(background)[[1]][[1]]$y))
  sand_df[ , value_index] <- 
      sapply(sand_df[ , value_index], 
             function(x) ifelse(x>value_max, value_max, x))
  sand_df$label_x <- sand_df$x
  sand_df$label_y <- sand_df$y
  p1 <- background +
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
    if (colorscale){
        p1 <- p1 + 
            geom_point(data=sand_df, size=4,  
                       mapping=aes_string(x='x', y='y', 
                                          color=names(sand_df)[value_index])) +
            scale_color_gradientn(name=legend_title, 
                                  colors=c("green", "yellow", "red"), 
                                  limits=c(-0.001, value_max+0.001), 
                                  breaks=c(0, value_max/2, value_max), 
                                  labels=c("0", as.character(value_max/2), 
                                           paste0(">", value_max)))
    } else{
        p1 <- p1 + 
            geom_point(data=sand_df, size=4,  
                       mapping=aes_string(x='x', y='y'), 
                       color="blue")
    }
    if (labels){
        p1 <- p1 + 
        ggrepel::geom_label_repel(data=sand_df, mapping=aes(x=x, y=y, label=csc),
                  nudge_x=x_range/25, nudge_y=y_range/45, box.padding=1) 
    }
  p1
}  

# adjust legend positions for plotting
leg_pos <- vector(mode="list", length=0)
leg_pos[['T10-1']] <- c(1, 0)
leg_pos[['T10-1a']] <- c(1, 1)
leg_pos[['T26']] <- c(0, 0)
leg_pos[['T13-1']] <- c(.2, .8)
leg_pos[['T29-2']] <- c(0, 0)
leg_pos[['T1A-4']] <- c(1, 0)
leg_pos[['T2-4']] <- c(1, 0)
leg_pos[['T2-5']] <- c(1, 0)
leg_pos[['T8W']] <- c(1, 0)
leg_pos[['T10-3W']] <- c(1, 0)
leg_pos[['T23NE']] <- c(1, 0)
leg_pos[['T27 Addition']] <- c(0, 0)
leg_pos[['T29-4']] <- c(1, 0)
leg_pos[['T36-3 Addition']] <- c(1, 0)
leg_pos[['T36-3E']] <- c(1, 0)
leg_pos[['T36-2W']] <- c(1, 0)
leg_pos[['T1-1']] <- c(1, 0)
leg_pos[['T5-3']] <- c(1, 0)
leg_pos[['T5-3 Addition']] <- c(1, 0)
leg_pos[['T9']] <- c(1, 0)
leg_pos[['T10-2S']] <- c(1, 0)
leg_pos[['T10-2N']] <- c(1, 0)
leg_pos[['T10-2']] <- c(1, 0)
leg_pos[['T10-3E']] <- c(1, 0)
leg_pos[['T10-3']] <- c(1, 0)
leg_pos[['T18-0']] <- c(1, 0)
leg_pos[['T21W']] <- c(1, 0)
leg_pos[['T25-3']] <- c(1, 0)
leg_pos[['T37-2']] <- c(1, 0)
leg_pos[['North']] <- c(0, 1)
leg_pos[['Central']] <- c(0, 1)
leg_pos[['East']] <- c(0, 0)
leg_pos[['South']] <- c(0, 1)
leg_pos[['Channel North']] <- c(1, 0)
leg_pos[['Channel South']] <- c(1, 0)
leg_pos[['T1A-1']] <- c(1, 0)
leg_pos[['T16']] <- c(1, 0)
leg_pos[['T29-3']] <- c(0, 0)
leg_pos[['T1-1']] <- c(1, 1)
leg_pos[['T17-1']] <- c(1, 1)
leg_pos[['T11']] <- c(1, 1)
leg_pos[['T18S']] <- c(1, 1)
leg_pos[['T23-5']] <- c(1, 1)
leg_pos[['T37-2d']] <- c(1, 1)
leg_pos[['T2-1']] <- c(1, 1)
leg_pos[['T17-2S']] <- c(1, 1)
leg_pos[['T21E']] <- c(1, 1)
leg_pos[['T37-2a']] <- c(1, 1)
leg_pos[['T1A-2']] <- c(1, 1)

csc_label_adjust <- list('twb2'=list('1661'=c(x=100, y=100),
                                 '1647'=c(x=150, y=400),
                                 '1614'=c(x=0, y=300),
                                 '1604'=c(x=-500, y=0)), 
                     'dwm'=NULL, 
                     'channel'=NULL, 
                     'brine'=NULL, 
                     't1a1'=NULL)

move_brine_labels <- function(df1){
    df1[df1$id2=='T23NE', ]$x <- df1[df1$id2=='T23NE', ]$x - 150
    df1[df1$id2=='T2-4', ]$x <- df1[df1$id2=='T2-4', ]$x + 300
    df1
}

move_dwm_labels <- function(df1){
    df1[df1$id2=='T10-2', ]$y <- df1[df1$id2=='T10-2', ]$y + 300
    df1[df1$id2=='T10-1', ]$y <- df1[df1$id2=='T10-1', ]$y + 300
    df1[df1$id2=='T10-1', ]$x <- df1[df1$id2=='T10-1', ]$x - 300
    df1[df1$id2=='T10-1a', ]$y <- df1[df1$id2=='T10-1a', ]$y - 150
    df1
}

move_channel_labels <- function(df1){
    df1[df1$id2=="Channel South", ]$x <- 410100 
    df1[df1$id2=="Channel South", ]$y <- 4020800
    df1[df1$id2=="Channel North", ]$x <- 411665
    df1[df1$id2=="Channel North", ]$y <- 4023161
    df1
}

move_t1a1_labels <- function(df1){
    df1[df1$id2=='T1A-1', ]$x <- df1[df1$id2=='T1A-1', ]$x + 150
    df1[df1$id2=='T1A-1', ]$y <- df1[df1$id2=='T1A-1', ]$y
    df1
}

move_twb2_labels <- function(df1){
    df1[df1$id2=='T2-2', ]$y <- df1[df1$id2=='T2-2', ]$y - 250
    df1[df1$id2=='T2-3', ]$x <- df1[df1$id2=='T2-3', ]$x - 600
    df1[df1$id2=='T2-3', ]$y <- df1[df1$id2=='T2-3', ]$y - 50
    df1[df1$id2=='T2-4', ]$x <- df1[df1$id2=='T2-4', ]$x - 400
    df1[df1$id2=='T2-4', ]$y <- df1[df1$id2=='T2-4', ]$y + 200
    df1[df1$id2=='T3NE', ]$x <- df1[df1$id2=='T3NE', ]$x + 600
    df1[df1$id2=='T3NE', ]$y <- df1[df1$id2=='T3NE', ]$y + 250
    df1
}
