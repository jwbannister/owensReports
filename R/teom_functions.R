#' Pull wind data from portable TEOM stations from database.
#' 
#' Pull wind speed and direction from teoms.teom_summary_data in AirSci 
#' PostgreSQL database. This function pulls only for stations required for the 
#' TwB2 paired TEOM reporting.
#' *Note: PM10 data from this is the analog averaged data transitted via 
#' LoggerNet. The PM10 data from this table should **not** be used for 
#' reporting.*
#' 
#' @param date1, date2 Text string. Date range for which to pull data.
#' @return Data frame.
#' @examples
#' pull_teom_wind("2016-02-01", "2016-03-01")
pull_teom_wind <- function(date1, date2){
    query1 <- paste0("SELECT i.deployment, t.datetime, t.ws_wvc AS ws, ",
                     "t.wd_wvc AS wd ",
                     "FROM teom.teom_analog_1hour t ",
                     "JOIN instruments.deployments i ",
                     "ON t.deployment_id=i.deployment_id ",
                     "WHERE (datetime-'1 second'::interval)::date ",
                     "BETWEEN '", date1, "'::date ", 
                     "AND '", date2, "'::date ",  
                     "AND NOT t.deployment_id = 8")
    wind_df <- owensData::query_owens(query1)
    wind_df$wd <- round(wind_df$wd, 2)
    wind_df
}

pull_mfile <- function(date1, date2){
    query1 <- paste0("SELECT site AS deployment,  datetime, aspd AS ws, ",
                     "dir AS wd, teom AS pm10_avg ",
                     "FROM archive.mfile_data ",
                     "WHERE (datetime-'1 second'::interval)::date ",
                     "BETWEEN '", date1, "'::date ", 
                     "AND '", date2, "'::date ",  
                     "AND site = 'T7'")
    mfile_df <- owensData::query_owens_local(query1)
    mfile_df$wd <- round(mfile_df$wd, 2)
    mfile_df$pm10_avg <- round(mfile_df$pm10_avg, 2)
    mfile_df
}

#' Pull instrument location data.
#' 
#' pull instrument UTM coordinates and deployment description from 
#' instruments.deployments in AirSci PostgreSQL database, base on deployment_id 
#' number. 
#' 
#' @param deploys Numerical. Vector of deployment ids.
#' @return Data frame.
#' @examples
#' pull_locations(c(1719, 1718, 1849))
pull_locations <- function(deploys){
    deploys <- paste0("('", paste(deploys, collapse="', '"), "')")
    query1 <- paste0("SELECT deployment, easting_utm AS x, 
                     northing_utm AS y
                     FROM instruments.deployments 
                     WHERE deployment IN ", deploys)
     station_locs <- owensData::query_owens(query1)
     station_locs
}

#' Pull report-quality PM10 data
#'
#' Pull PM10 data for reporting from AirSci PostgreSQL database.
#' 
#' @param x A number.
#' @param deploys Numerical. Vector of deployment ids.
#' @return Data frame.
#' @examples
#' pull_pm10(c(1719, 1718, 1849))
pull_pm10 <- function(date1, date2, deploys){
    d1 <- paste0(date1, " ", "00:00:00")
    d2 <- paste0(date2 %m+% days(1), " ", "00:00:00")
    deploys <- paste0("('", paste(deploys, collapse="', '"), "')")
    query1 <- paste0("SELECT deployment, datetime, pm10_std_avg AS pm10_avg, ",
                     "invalid ",
                     "FROM teom.avg_1hour_validated ",
                     "WHERE (datetime-'1 second'::interval)::date ",
                     "BETWEEN '", date1, "'::date ", 
                     "AND '", date2, "'::date ",  
                     "AND deployment IN ", deploys) 
    pm10_df <- owensData::query_owens(query1)
#    pm10_df <- filter(pm10_df, pm10_avg > -35)
    pm10_df$pm10_avg <- round(pm10_df$pm10_avg, 2)
    pm10_df
}

#' Identify missing hourly data
#' 
#' @param teom_data Data frame. 
find_missing <- function(teom_data){
  index <- expand.grid(unique(teom_data$datetime), unique(teom_data$deployment)) 
  colnames(index) <- c("datetime", "deployment")
  index$deployment <- as.character(index$deployment)
  df1 <- left_join(index, teom_data, by=c("datetime", "deployment"))
  pm10_missing <- 
    data.frame(hour = filter(df1, is.na(pm10))$datetime,
               site = filter(df1, is.na(pm10))$deployment)  
  wind_missing <- 
    data.frame(hour = filter(df1, is.na(ws) | is.na(wd))$datetime,
             site = filter(df1, is.na(ws) | is.na(wd))$deployment)
  missing_data <- rbind(pm10_missing, wind_missing) %>% arrange(hour, site)
  if (nrow(missing_data)>0){
  missing_data$flag <- "no data"
  flagged_data <- dplyr::left_join(index, missing_data, 
                                   by=c("datetime"="hour",
                                        "deployment"="site"))
  
  return(flagged_data)
  } else {
    print("No missing TEOM data.")
  }
}

#' Summarize missing hourly data
#' 
#' @param teom_data Data frame. 
find_missing_summary <- function(teom_data){
  pm10_missing <- 
    data.frame(day = as.Date(filter(teom_data, is.na(pm10))$datetime),
               site = filter(teom_data, is.na(pm10))$deployment)  %>%
     group_by(day, site) %>% summarize(hours=length(day)) %>% ungroup()
  pm10_missing$flag <- rep("pm10", nrow(pm10_missing))
  wind_missing <- 
    data.frame(day = as.Date(filter(teom_data, is.na(ws) | is.na(wd))$datetime),
             site = filter(teom_data, is.na(ws) | is.na(wd))$deployment)  %>%
     group_by(day, site) %>% summarize(hours=length(day)) %>% ungroup()
  wind_missing$flag <- rep("wind", nrow(wind_missing))
  missing_data <- rbind(pm10_missing, wind_missing) %>% arrange(day, site)
  missing_data
}

#' round a number to closest multiple of a given number.
#' 
#' @param x Numeric.
#' @param base Numeric.
#' @return x rounded to closest multiple of base.
#' @examples
#' > mround(11.3, 5)
#' > 10
mround <- function(x, base){
  base * round(x/base)
}

#' Pair up teoms for upwind/downwind analysis
#' 
#' This function is specific to pairing teoms in TwB2 upwind/downwind analysis.
#' 
#' @import dplyr
#' @param df1 Data frame. Teoms under consideration.
#' @return Input data frame with added column listing relevant DCA for paired 
#' TEOMS. 
pair_teoms <- function(df1){
  df1$dca.group <- rep(NA, nrow(df1))
  for (i in 1:nrow(df1)){
    if (df1$deployment[i] == "T7" | df1$deployment[i] == "T2-1"){
      df1$dca.group[i] <- "South"
    }
    if (df1$deployment[i] == "T11" | df1$deployment[i] == "T16"){
      df1$dca.group[i] <- "Central"
    }
    if (df1$deployment[i] == "T29-4S" | df1$deployment[i] == "T29-4N"){
      df1$dca.group[i] <- "North"
    }
  }
  df1
}

#' Assign upwind/downwind angles for TEOM pair
#' 
#' @import dplyr
#' @param df1 Data frame containing TEOM pairs. Locations or each TEOM in *x* & 
#' *y* columns, matched pairs assigned group in *dca.group* column, each TEOM  
#' has unique identifier in *deployment.id* column, "N" or "S" position in 
#' *position* column. 
#' @return Original data frame with *upwind.angle* and *downwind.angle* columns 
#' addded.
assign_wind_angle <- function(df1){
  df1 <- group_by(df1, dca.group) %>% 
    mutate(position = ifelse(y==max(y), "N", "S")) %>%
    arrange(desc(position)) %>%
    mutate(alpha = atan(diff(x)/diff(y))) %>% 
    mutate(alpha = ifelse(alpha>0, alpha, 2*pi+alpha)) %>%
    mutate(upwind.angle = ifelse(position=="N", alpha, alpha-pi)) %>%
    mutate(upwind.angle = ifelse(upwind.angle>0, upwind.angle, 
                                 2*pi+upwind.angle)) %>%
    select(-alpha) %>%
    ungroup()
  df1$upwind.angle <- circular::deg(df1$upwind.angle) 
  df1$upwind.angle <- round(df1$upwind.angle, 1)
  df1 <- df1 %>% group_by(deployment) %>% 
    mutate(downwind.angle=ifelse(upwind.angle>180, upwind.angle-180,
                                 upwind.angle+180)) %>%
  ungroup()
df1
}

#' Filter hourly data by wind direction.
#' 
#' @import dplyr
#' @param df1 Data frame. Data for teoms under consideration.
#' @param locs Data frame. List of teoms with relevant upwind direction.
#' @return Data frame containing only those hours that are in an upwind 
#' direction for the associated teom. 
define_event <- function(df1, locs){
  return_df <- df1
  return_df$tag <- rep(NA, nrow(return_df))
  return_df <- return_df[numeric(0), ]
  for (i in 1:nrow(locs)){
    a <- filter_by_angle(df1, locs$deployment[i], locs$upwind.angle[i], 
                         "UW")
    b <- filter_by_angle(df1, locs$deployment[i], locs$downwind.angle[i], 
                         "DW")
    return_df <- rbind(return_df, a, b)
  }
  return_df
}

filter_by_angle <- function(df1, id, angle, tag){
  upper.angle <- angle + 11.25
  upper.angle.alt <- upper.angle-360 
  lower.angle <- angle - 11.25
  lower.angle.alt <- lower.angle+360
  if (upper.angle>360){
    if (lower.angle<0){
      events <- dplyr::filter(df1, deployment==id,
                              (wd>lower.angle.alt&wd<360) |
                                (wd>0&wd<upper.angle))
    }else {
      events <- dplyr::filter(df1, deployment==id,
                              (wd>lower.angle&wd<360) |
                                (wd>0&wd<upper.angle.alt))
    }
  }else {
    events <- dplyr::filter(df1, deployment==id,
                            wd>lower.angle&wd<upper.angle)
  }
  events$tag <- rep(tag, nrow(events))
  events
}

#' Plot paired TEOMS with wind roses.
#' 
#' @param teom_locs Data frame. TEOMS with pairs grouping in *dca.group* column.
#' @param df1 Data frame. Hourly PM10 data.
#' @param background GGplot object. Background onwhich to lay roses.
teom_pair_plot <- function(teom_locs, df1, background, start_date, end_date){
  a <- list(grobs=c(), centers=c())
  valueseq <- c(10, 50, 150, 500)
  legend.plot <- df1 %>% filter(dca.group==teom_locs$dca.group[1]) %>%
    plot_rose(., value='pm10_avg', dir='wd', valueseq=valueseq,
              legend.title="PM10")
  legnd <- g_legend(legend.plot)
  for (j in 1:nrow(teom_locs)){
    tmp_df <- filter(df1, deployment==teom_locs$deployment[j])
    p <- plot_rose_image_only(tmp_df, value='pm10_avg', dir='wd', valueseq=valueseq)
    fl <- paste0(tempfile(), ".png")
    png(filename=fl, height=3, width=3, units="in", res=300, bg="transparent")
    print(p)
    dev.off()
    img <- png::readPNG(fl)
    ras <- grid::rasterGrob(img, interpolate=TRUE)
    a$grobs[[j]] <- ras
    a$centers[[j]] <- c(teom_locs$x[j], teom_locs$y[j])
  }
  info <- ggplot_build(background)
  xrange <- info[[2]]$ranges[[1]]$x.range
  yrange <- info[[2]]$ranges[[1]]$y.range
  buffer <- (xrange[2] - xrange[1])/10
  coll.start <- format(as.Date(start_date), "%m/%d")
  coll.end <- format(as.Date(end_date), "%m/%d/%Y")
  plot.title <- paste0("TwB2 ", teom_locs$dca.group[1], 
                       " PM10/Wind Roses (", coll.start, " - ", coll.end, ")")
  p3 <- background + 
    annotation_custom(a$grobs[[1]], xmin=a$centers[[1]][1] - 2*buffer,
                      xmax=a$centers[[1]][1] + 2*buffer, 
                      ymin=a$centers[[1]][2] - 2*buffer,
                      ymax=a$centers[[1]][2] + 2*buffer) +
    annotation_custom(a$grobs[[2]], xmin=a$centers[[2]][1] - 2*buffer,
                      xmax=a$centers[[2]][1] + 2*buffer, 
                      ymin=a$centers[[2]][2] - 2*buffer,
                      ymax=a$centers[[2]][2] + 2*buffer) +
    annotation_custom(legnd, xmin=xrange[2] - 1.6*buffer, xmax=xrange[2],
                      ymin = yrange[1], ymax=yrange[1] + 2.5*buffer) +
#    geom_label(data=teom_locs, mapping=aes(x=x, y=y, label=deployment), 
#               nudge_x=1.0*buffer) +
    ggtitle(plot.title) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          plot.title=element_text(size=12))
  dev.off()
  p3
}

#' strip legend from ggplot object
#' 
#' @param a.gplot ggplot object.
#' @return A grob of the plot legend
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
} 
