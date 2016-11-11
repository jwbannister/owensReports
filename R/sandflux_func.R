
summarize_flux_twb2 <- function(df1, df2){
  df_out <- inner_join(df1, df2, by="csc") %>%
    select(area=dcm, site=csc, sand.mass, max.daily.flux)
  df_out$max.over.pnt5 <- sapply(df_out$max.daily.flux, 
                                 function(x) ifelse(x>0.5, "Yes", "No"))
  df_out$max.over.1 <- sapply(df_out$max.daily.flux, 
                              function(x) ifelse(x>1, "Yes", "No"))
  df_out$action <- rep("OK", nrow(df_out))
  for (i in 1:nrow(df_out)){
    if (is.na(df_out$max.over.pnt5[i])){
      df_out$action[i] <- "NA"
    } else{
    if (df_out$max.over.pnt5[i] == "Yes") df_out$action[i] <- "Maintain"
    if (df_out$max.over.1[i] == "Yes") df_out$action[i] <- "Re-Flood"
    }
  }
  df_out <- as.data.frame(df_out)
  df_out
}

rank_flux_cells <- function(df_in){
  maxes <- df_in %>% group_by(area) %>%
    summarize(t_45=max(t_45, na.rm=TRUE), t_55=max(t_55, na.rm=TRUE), 
              t_65=max(t_65, na.rm=TRUE), t_75=max(t_75, na.rm=TRUE))
  mins <- df_in %>% group_by(area) %>%
    summarize(t_45=min(t_45, na.rm=TRUE), t_55=min(t_55, na.rm=TRUE), 
              t_65=min(t_65, na.rm=TRUE), t_75=min(t_75, na.rm=TRUE))
  maxed <- vector(mode="list", length=4)
  names(maxed) <- colnames(maxes)[2:5]
  minned <- maxed
  clr <- maxed
  colorClass <- function(x){
    a <- 'white'
    if (x==1) a <- 'red'
    if (x==2 | x==3) a <- 'green'
    if (is.na(x)) a <- 'white'
    a
  }
  for (j in names(maxed)){
    for (i in maxes$area){
      d <- filter(df_in, area==i)[ , j]==max(filter(maxes, area==i)[ , j])
      maxed[[j]] <- c(maxed[[j]], d)
    }
    maxed[[j]] <- sapply(maxed[[j]], function(x) x*2)
  }
  for (j in names(minned)){
    for (i in maxes$area){
      d <- filter(df_in, area==i)[ , j]==min(filter(mins, area==i)[ , j])
      minned[[j]] <- c(minned[[j]], d)
    }
    minned[[j]] <- sapply(minned[[j]], function(x) x*1)
  }
  for (i in 1:length(clr)){
    clr[[i]] <- maxed[[i]] + minned[[i]]
    clr[[i]][is.na(clr[[i]])] <- 0
    clr[[i]][clr[[i]]==3] <- 2
  }
  clr
}


