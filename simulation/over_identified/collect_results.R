library(dplyr)

out<-lapply(list.files("cache/"),function(file.name){
    readRDS(paste0("cache/",file.name))
})%>%bind_rows

saveRDS(out,"results.rds")
