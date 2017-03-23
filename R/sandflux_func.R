
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

calc_flux_ce_sfwcrft <- function(df1){
    df2 <- df1 %>% group_by(dca, treatment, date) %>% 
        summarize(sand=round(mean(sand.flux), 2)) %>% ungroup()
    control <- filter(df2, treatment=="0%") %>%
        select(dca, date, control.sand=sand)
    # calculated control efficiency for treatment areas, only if average daily 
    # flux in control area is greater than 1 gram/cm^2
    control_sum <- df2 %>% 
        inner_join(control, by=c("dca", "date")) %>%
        mutate(control.eff=round(1-(sand/control.sand), 2)*100) %>%
        filter(treatment!='0%') %>%
        filter(control.sand>1)
    if (nrow(control_sum)>0){
    df_a <- control_sum %>% select(-control.eff) %>% 
        spread(treatment, sand)
    names(df_a)[4:5] <- c('sand_55', 'sand_65')
    df_b <- control_sum %>% select(-sand) %>% 
        spread(treatment, control.eff)
    names(df_b)[4:5] <- c('eff_55', 'eff_65')
        df_out <- inner_join(df_a, df_b, by=c('dca', 'date', 'control.sand')) %>%
            arrange(desc(control.sand))
    } else{
        df_out <- control_sum
    }
    df_out
}
