# Codelist ----------------------------------------------------------------

# cl <- read_html("SDTMCL.html") %>% html_nodes("#contents") %>% html_node("table") %>% html_table(fill = T)
# saveRDS(cl[[1]], "codelist.rds")

cl <- readRDS("codelist.rds")
cl$cl_code <- cl$`NCI Code`
cl$cl_name <- cl$`Name(CDISC Submission Value)`
for (i in 1:nrow(cl)) {
  if (cl$OID[i] == "" || cl$OID[i] == "Back to top") cl$cl_code[i] <- cl$cl_name[i] <- ""
}




# UTILITY: SDTM and ADAM vars ---------------------------------------------


varlist <- function(){
  library(tidyverse)
  options(stringsAsFactors = F)
  sdtm_domain <- readRDS("sdtm_domain.rds")
  
  base <- sdtm_domain[[1]]
  sdtmvars <- data.frame()
  for(i in 2:61){
    cur <- sdtm_domain[[i]]
    cur$domain <- base$Dataset[i-1]
    sdtmvars <- rbind(sdtmvars, cur)
  }
  
  # merge with ADAM vars
  adam <- read.csv2("adamvar.csv")
  adamvars <- data.frame(col1 = adam[,1], col2 = adam[,2], col3 = adam[,3], col4 = adam[,4],
                         col5 = "", col6 = "", col7=adam[,5], col8 = "ADAM")
  colnames(adamvars) <- colnames(sdtmvars)
  allvars <- rbind(sdtmvars, adamvars)
  
  # distinct and sort
  allvars %>% 
    distinct(`Variable Name`, .keep_all = TRUE) %>%
    arrange(`Variable Name`)  -> allvars
  
  for (i in 1:nrow(allvars)) {
    for (j in 1:ncol(allvars)){
      if( is.na(allvars[i,j]) || allvars[i,j] == "") allvars[i,j] <- "x"
      #    stdmvars[i,j] <- stdmvars[i,j] %>% str_replace_all(",", ".")
    }
  }
  
  
  write_delim(allvars[,-6], "vars", delim = "|")  
}


