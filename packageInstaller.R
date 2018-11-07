list.of.packages <- c("shiny","RNeo4j","data.table","rjson","visNetwork","DT")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)