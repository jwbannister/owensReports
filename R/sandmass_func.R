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
    select(-control.mass)
  treat_sum$control.eff <- round(treat_sum$control.eff, 2) * 100
  treat_sum[treat_sum$treatment=="0%", ]$control.eff <- NA
  treat_sum$avg.sand.mass <- round(treat_sum$avg.sand.mass, 2)
  treat_sum
}

calc_flux_ce_sfwcrft <- function(df1){
  df2 <- df1 %>% group_by(dca, treatment, day) %>% 
    summarize(sand=mean(sand.flux)) %>% ungroup()
  control <- filter(df2, treatment=="0%") %>%
    select(dca, day, control.sand=sand)
  # calculated control efficiency for treatment areas, only if average daily 
  # flux in control area is greater than 1 gram/cm^2
  control_sum <- df2 %>% 
    inner_join(control, by=c("dca", "day")) %>%
    mutate(control.eff=round(1-(sand/control.sand), 2)*100) %>%
    filter(control.sand>1) %>%
    select(-sand)
  control_sum[control_sum$treatment=="0%", ]$control.eff <- NA
  df_out <- spread(control_sum, treatment, control.eff) %>%
  filter(!is.na('45%')) %>% select(-4) %>% group_by(dca) %>%
  arrange(desc(control.sand))
  df_out
}
