cross_walk <- c("T3SW"="T3SW", "T3SE"="T3SE", "T3NE"="T3NE", 
                "T2-2"="T2-2", "T2-3"="T2-3", "T2-4"="T2-4", 
                "T16"="T16", "T12"="T12-1", "T24ADD"="T24 Addition")
gdrive_images <- system(paste0("gdrive list ", 
                               "-q \"trashed = false and ", 
                               "'0B8qHESXOhs-DQTlXa3FKWlpSOHM' in parents and ",
                               "mimeType = 'application/pdf'\" ", 
                               "-m 10000"), intern=T)  
cmnt_fl <- tempfile()
write.table(gdrive_images, file=cmnt_fl, quote=F, row.names=F, col.names=F)
images_list <- read.table(file=cmnt_fl, sep="", header=F, na.strings="", 
                          skip=1, stringsAsFactors=F)
images_list$len <- nchar(images_list$V2)
images_list$index_date <- 
    as.Date(paste0('20', 
                   substr(images_list$V2, 
                          images_list$len-13, images_list$len-12), "-", 
                   substr(images_list$V2, 
                          images_list$len-11, images_list$len-10), "-", 
                   "01"))
images_list$id2 <- 
    cross_walk[substr(images_list$V2, 1, images_list$len-14)]
recent_images <- images_list %>% group_by(id2) %>%
    mutate(delta=end_date - index_date) %>%
    filter(delta==min(delta))
recent_images <- group_twb2_areas(recent_images)
lidar_files <- vector(mode='list', length=length(unique(recent_images$id3)))
names(lidar_files) <- unique(recent_images$id3)
for (i in names(lidar_files)){
    lidar_files[[i]] <- 
        vector(mode='character', length=nrow(filter(recent_images, id3==i)))
    names(lidar_files[[i]]) <- filter(recent_images, id3==i)$id2
    for (j in names(lidar_files[[i]])){
        if (!is.na(filter(recent_images, id2==j)$V2)){
            lidar_files[[i]][j] <- 
                paste0(tempdir(), "/", filter(recent_images, id2==j)$V2)
            system(paste0("gdrive download --force --path ", 
                          tempdir(), " ", filter(recent_images, id2==j)$V1))
            system(paste0("convert -verbose -density 150 ", lidar_files[[i]][j], 
                          " -resize 35% -quality 100 -flatten ",
                          "-sharpen 0x1.0 ",
                          "-rotate 90 ", 
                          "-compress lossless ", 
                          gsub(".pdf", ".jpg", lidar_files[[i]][j])))
#            system(paste0("convert -verbose -density 150 ", lidar_files[[i]][j], 
#                          " -resize 5000x400 -quality 100 -flatten ",
#                          "-sharpen 0x1.0 -compress lossless ", 
#                          gsub(".pdf", ".jpg", lidar_files[[i]][j])))
            lidar_files[[i]][j] <- gsub(".pdf", ".jpg", lidar_files[[i]][j])
        } else{
            lidar_files[[i]][j] <- NA
        }
    }
}

