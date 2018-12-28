library(rvest)
options(stringsAsFactors = F)


# get SDTMIG domains ------------------------------------------------------


tables <- read_html("SDTMIG.html") %>%
  html_nodes("table.sdtmig-metadata") %>%
  html_table()
 

catlog <- tables[[1]]
catlog$index <- NA


for (i in 2:length(tables)) {
  domain = tables[[i]][2,4]
  row2write = which(catlog$Dataset == domain)
  if(length(row2write)) catlog$index[row2write] <- i
}

catlog$Name <- paste(catlog$Dataset, catlog$Description)

tables[[1]] <- catlog

saveRDS(tables, file = "sdtm_domain.rds")



# working on SDTM domains -------------------------------------------------


the_table <- tables[[14]]

getDataType <- function(type = c("char")){
  result <- NULL
  for(i in 1:length(type)){
    if(type[i] == "Char") result <- c(result, "Char(200)")
    else if(type[i] == "Num") result <- c(result, "Num(10)")
    else result <- c(result, "/*ERROR*/")
  }
  result
}

the_table %>% 
  mutate(Sql = paste0(pull(.,1), " ", getDataType(pull(., 3)), ' "', pull(., 2), '"')) %>%
  pull(Sql) %>% 
  paste(collapse = ", \n") %>% 
  cat()
