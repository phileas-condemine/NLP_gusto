library(readxl)
library(data.table)
library(stringr)


files = list.files("data/bronze/",full.names = T)

excel_sheets("data/bronze/ENQUÊTE COVEA - SARAN.xlsx")

read_excel("data/bronze/ENQUÊTE COVEA - WACKEN STRASBOURG.xlsx",sheet="Question 20",skip = 5)

q20 = rbindlist(lapply(files,function(file){
  dt = read_excel(file,sheet="Question 20",skip = 5)
  setDT(dt)
  dt$file = file
  dt
}))

q26 = rbindlist(lapply(files,function(file){
  dt = read_excel(file,sheet="Question 26",skip = 5)
  setDT(dt)
  dt$file = file
  dt
}))

q20$question = "q20"
q26$question = "q26"

verbatim = rbindlist(list(
  q20,q26
))


verbatim[,site := str_remove(file,"data/bronze/ENQUÊTE COVEA - ")]
verbatim[,site := str_remove(site,".xlsx$")]

verbatim[,.N,by=.(question,site)]

verbatim = verbatim[,.(id = as.character(`Respondent ID`),date = `Response Date`,text = Responses,question, site)]


fwrite(verbatim,"data/silver/verbatim_consolides.csv")
fwrite(verbatim[,.(id,question,text)],"data/silver/id_q_verbatim_consolides.csv")




