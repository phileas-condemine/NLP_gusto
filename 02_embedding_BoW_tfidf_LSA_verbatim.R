library(data.table)
library(stringr)
library(text2vec)
library(ggplot2)
library(plotly)
source("utils.R",encoding="UTF-8")




verbatim = fread("data/silver/verbatim_consolides_separated.csv",encoding="UTF-8",colClasses = "character")
# verbatim = fread("data/silver/verbatim_consolides.csv",encoding="UTF-8",colClasses = "character")
verbatim = verbatim[question=="q20"]
verbatim = verbatim[,text := tolower(text)]
verbatim = verbatim[,.SD[1],by=text]
verbatim[,.N,by=text][N>1]
verbatim = verbatim[text!=""]
verbatim = verbatim[nchar(text)>10]

verbatim = comptages_de_caracteres(verbatim)
coord = create_text_embedding(verbatim,tok_fun = tok_no_stem_fun)
verbatim = merge(verbatim,coord,by="text")


g <- ggplot(verbatim)+
  aes(x=V1,y=V2,color=site,label=text)+
  geom_point()


ggplotly(g)


