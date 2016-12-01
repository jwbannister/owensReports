load_all()
load_all("~/code/owensMaps")
library(tidyverse)
library(lubridate)

twb2 <- group_twb2_areas()

df1 <- read.csv("~/code/owensReports/data/surface_data_113016.csv") 
colnames(df1) <- tolower(colnames(df1))
surface_df <- df1 %>% rename(dca=area) %>%
    left_join(select(twb2$data, dca, group), by="dca") %>%
    filter(month==month(start_date) & year==year(start_date))

