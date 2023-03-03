library(readxl)
library(data.table)
library(stringr)
library(stringr)
library(dplyr)
library(tidyr)


verbatim = fread("data/silver/verbatim_consolides.csv",encoding="UTF-8",colClasses = "character")
print(nrow(verbatim))
verbatim = setDT(separate_rows(verbatim,text,sep="(\\.\r\n)|(\\.\n)|(\\. )|(\r\n)|(\n)"))
print(nrow(verbatim))
verbatim[, ordre := (1:.N), by=.(id,question)]
verbatim = verbatim[text!=""]
verbatim = verbatim[nchar(text)>5]
print(nrow(verbatim))

fwrite(verbatim,"data/silver/verbatim_consolides_separated.csv")
