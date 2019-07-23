csc_adjust <- function(collections, site_list){
    may_valids <- collections %>% 
        filter(deployment %in% site_list & collection_datetime < as.Date('2019-05-31') & 
               collection_datetime > as.Date('2019-01-01')) %>%
        group_by(deployment) %>% 
        summarize(current_valid=max(collection_datetime))
    tmp <- collections %>% 
        filter(deployment %in% site_list & 
               collection_datetime < as.Date('2019-04-30') &
               collection_datetime > as.Date('2019-01-01')) %>%
        filter(dwp_mass != -999)
    valids <- tmp %>% group_by(deployment) %>%
        summarize(last_valid= max(collection_datetime) + 30) %>%
        left_join(may_valids) %>%
        arrange(deployment)
    bads <- collections %>% 
        filter(deployment %in% site_list & dwp_mass == -999) %>%
        group_by(deployment) %>% summarize(bad_sumpc = sum(sumpc_total))
    new_collections <- collections %>% 
        filter(deployment %in% site_list & collection_datetime < as.Date('2019-05-31') &
               collection_datetime > as.Date('2019-01-01')) %>%
        group_by(deployment) %>% 
        filter(collection_datetime==max(collection_datetime)) %>% ungroup() %>%
        arrange(deployment) %>% 
        left_join(bads) 
    new_collections$sumpc_total <- new_collections$sumpc_total +
        new_collections$bad_sumpc
    new_collections <- select(new_collections, -bad_sumpc)
    new_collections$start_datetime <- valids$last_valid
    return(new_collections)
}

apply_func <- function(flux_line, col){
    tmp <- csc_collections %>% 
        filter(deployment==flux_line[4] &
               start_datetime <= flux_line[1] &
               collection_datetime >= flux_line[1])
    return(tmp[col][1])
}

