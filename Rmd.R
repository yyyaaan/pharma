library(rvest); library(tidyverse)
options(stringsAsFactors = F)


# supporting functions RUN ------------------------------------------------



getDataType <- function(type = c("char"), lang = "SQL"){
  result <- NULL
  if (lang == "SQL"){
    for(i in 1:length(type)){
      if(type[i] == "Char") result <- c(result, "Char(100)")
      else if(type[i] == "Num") result <- c(result, "   Num(8)")
      else result <- c(result, "/*ERROR*/")
    }
  }
  if (lang == "SAS"){
    for(i in 1:length(type)){
      if(type[i] == "Char") result <- c(result, 'length=$100')
      else if(type[i] == "Num") result <- c(result, 'length=8   ')
      else result <- c(result, "/*ERROR*/")
    }
  }
  result
}

getCode <- function(the_table, lang = "SQL"){
  
  if(lang == "SQL") return(
    the_table %>% 
      mutate(Sql = paste0('\t/*', str_pad(Core, 4) ,'*/  ', 
                          str_pad(`Variable Name`, 14, "right"), 
                          getDataType(Type), 
                          ' "', `Variable Label`, '"')) %>%
      pull(Sql) %>% 
      paste(collapse = ", \n") %>% 
      paste("create table attrib(\n", ., "\n)")
  )
  
  if(lang == "SAS") return(
    the_table %>% 
      mutate(Sas = paste0('\t/*',str_pad(Core, 4) ,'*/  ', 
                          str_pad(`Variable Name`, 14, "right"), 
                          getDataType(Type, "SAS"),
                          '  label="', `Variable Label`, '"')) %>%
      pull(Sas) %>% 
      paste(collapse = "\n") %>% 
      paste('data shell;\n\tattrib\n', . ,'\n\t;\n\tretain _character_ "" _numeric_ . ; stop;\nrun;')
  )
}


# get SDTMIG domains ------------------------------------------------------

getInfo_no_run <- function(){
  
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
  catlog$index[56] <- 58; catlog$index[57] <- 60; catlog$index[58] <- 59
  
  
  catlog$Name <- paste(catlog$Dataset, catlog$Description)
  
  
  tables[[1]] <- catlog
  
  saveRDS(tables, file = "sdtm_domain.rds")
  
  
  
  #   ===   ===   ===

  
  adam_extract <- adamSTR[2931:21613]
  adam_extract[length(adam_extract)]
  
  adam <- read_html(adam_extract)
  adam %>% html_nodes("p")
  


  
  


  xpath <- sprintf("./p[count(preceding-sibling::h2)=%d]", seq_along(headlines)-1)
  
  adam %>% html_nodes(xpath = "./p[count(preceding-sibling::h2)=9]") %>% html_text()
  
}



# get fancy list for domains ----------------------------------------------

getCatlogList_no_run <- function(){
  catlog %>% 
    select(Class, Name) %>% mutate(id = 1:nrow(catlog)) %>% 
    spread(key = "Class", value = "Name") %>% select(-id) %>%
    as.list() %>% lapply(function(x) x[!is.na(x)])
}




# playground --------------------------------------------------------------

playit_not_run <- function(){
  
  tables <- readRDS("sdtm_domain.rds")
  
  the_table <- tables[[14]]
  
  the_table %>% getCode("SAS") %>% cat()
  
  # Count
  the_table %>%
    count(Core) %>%
    mutate(text = paste(Core, n)) %>%
    pull(text) %>%
    paste(collapse = "; ")
  
  "AE Adverse Events" %>% word(2:5) %>% na.omit() %>% paste(collapse = "+")

}


