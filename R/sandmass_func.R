calc_mass_ce_sfwcrft <- function(df_in){
  treat_sum <- df_in %>% group_by(id2, id1) %>% 
    summarize(avg.sand.mass=mean(sand.mass)) %>% ungroup()
  control_sum <- treat_sum %>% group_by(id2) %>%
    do(control.mass=filter(., id1=="0%")$avg.sand.mass)
  control_sum[control_sum$id2=="T13-1", 2] <- NA
  control_sum$control.mass <- unlist(control_sum$control.mass)
  # calculated control efficiency for treatment areas, only if average mass in 
  # control area is greater than 10 grams
  treat_sum <- inner_join(treat_sum, control_sum, by="id2") %>%
    mutate(control.eff=ifelse(control.mass<10, NA, 
                              1-(avg.sand.mass/control.mass))) %>%
    dplyr::select(-control.mass)
  treat_sum[treat_sum$id1=="0%", ]$control.eff <- NA
  treat_sum$control.eff <- 
      sapply(treat_sum$control.eff, function(x) 
             ifelse((is.na(x) | x<0), "-", paste0(round(x, 2) * 100, "%")))
  treat_sum$avg.sand.mass <- round(treat_sum$avg.sand.mass, 2)
  treat_sum
}

