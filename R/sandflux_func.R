clean_flux_sfwct <- function(df_in){
  df_out <- df_in %>% dplyr::select(-sensit_deployment_id, 
                                    -csc_deployment_id, -avgpc, -sumpc, 
                                    -avgpc_total, -sumpc_total, -district_mass,
                                    -dwp_mass, -modeled_mass, -method_used, 
                                    -resultant_wd_10m, csc=deployment, 
                                    y=northing_utm, x=easting_utm, -description)
  df_out$area <- gsub("-", "", df_out$area)
  df_out$area <- substr(df_out$area, 1, 3)
  cscs <- df_out[!duplicated(df_out$csc), ]
  cscs <- dplyr::select(cscs, csc, x, y)
  cscs <- assign_dcm(cscs, sfwct_polys)
  cscs <- inner_join(cscs, select(sfwct_dcm_data, objectid, treatment), 
                     by="objectid")
  cscs <- select(cscs, csc, treatment)
  df_out <- inner_join(df_out, cscs, by="csc")
  df_out
}

clean_flux_twb2 <- function(df_in){
  df_out <- df_in %>% dplyr::select(-sensit_deployment_id, 
                                    -csc_deployment_id, -avgpc, -sumpc, 
                                    -avgpc_total, -sumpc_total, -district_mass,
                                    -dwp_mass, -modeled_mass, -method_used, 
                                    -resultant_wd_10m, csc=deployment, 
                                    y=northing_utm, x=easting_utm, -description)
  df_out$area <- gsub(" ", "-", df_out$area)
  df_out$area <- substr(df_out$area, 1, 5)
  # these sites are not in tilled areas
  df_out <- filter(df_out, !(csc %in% c('1622', '1623', '1624', '1625', '1627')))
  df_out
}

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

summarize_flux_sfwct <- function(df1){
  df1$treatment <- paste0("t_", substr(df1$dcm, 5, nchar(df1$dcm)-1))
  df1$area <- substr(df1$dcm, 1, 3)
  df2 <- df1 %>% group_by(area, treatment, day) %>% 
    summarize(sand=mean(sand.flux)) %>% ungroup()
  df_temp <- summarize_ce(df2) %>% arrange(day, area) %>%
    filter(control.sand>1)
  if (nrow(df_temp)==0) {
    return("No days with control area sand flux > 1")
  } else{
  df_out <- dcast(df_temp, area + day + control.sand ~ treatment,
                  value.var="control.eff") %>%
  filter(!is.na(t_45)) %>% select(-t_0) %>% group_by(area) %>%
  arrange(desc(control.sand))
  }
  df_out
}

#' Summarize SFWCT sand results for control efficiency
#' 
#' @import dplyr
#' @param df_in Data frame of sand mass data.
#' @return Data frame of sumamrized results.
summarize_ce <- function(df_in){
  control <- filter(df_in, treatment=="t_0") %>%
    select(area, day, control.sand=sand)
  control_sum <- df_in %>% 
    inner_join(control, by=c("area", "day")) %>%
    mutate(control.eff=1-(sand/control.sand))
  control_sum$control.eff <- round(control_sum$control.eff, 2)
  control_sum[control_sum$treatment=="t_0", ]$control.eff <- NA
  control_sum
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


