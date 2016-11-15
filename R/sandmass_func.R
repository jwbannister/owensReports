calc_mass_ce_sfwcrft <- function(df_in){
  treat_sum <- df_in %>% group_by(dca, treatment) %>% 
    summarize(avg.sand.mass=mean(sand.mass)) %>% ungroup()
  control_sum <- treat_sum %>% group_by(dca) %>%
    do(control.mass=filter(., treatment=="0%")$avg.sand.mass)
  control_sum[control_sum$dca=="T13-1", 2] <- NA
  control_sum$control.mass <- unlist(control_sum$control.mass)
  # calculated control efficiency for treatment areas, only if average mass in 
  # control area is greater than 10 grams
  treat_sum <- inner_join(treat_sum, control_sum, by="dca") %>%
    mutate(control.eff=ifelse(control.mass<10, NA, 
                              1-(avg.sand.mass/control.mass))) %>%
    dplyr::select(-control.mass)
  treat_sum$control.eff <- round(treat_sum$control.eff, 2) * 100
  treat_sum[treat_sum$treatment=="0%", ]$control.eff <- NA
  treat_sum$avg.sand.mass <- round(treat_sum$avg.sand.mass, 2)
  treat_sum
}

