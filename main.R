source("packageInstaller.R")

library(shiny)
library(RNeo4j)
library(data.table)
library(rjson)
library(visNetwork)
library(DT)
library(shinythemes)
library(shinyjs)
library(vcfR)

config <- fromJSON(file="config.json")
credentials <- fromJSON(file="credentials.json")
options(shiny.maxRequestSize=(config$maxRequestSize*1024^2))




list <- 1:10000

