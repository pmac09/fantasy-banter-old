options(stringsAsFactors = FALSE)

library(tidyverse)
library(httr)
library(data.table)

path <- './references/functions'

func_list <- list.files(path)
func_list <- func_list[!grepl("load.R", func_list)]

for (func in func_list){
  source(paste0(path,'/',func))
}

rm('path','func_list','func')