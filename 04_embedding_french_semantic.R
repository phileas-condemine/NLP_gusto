library(data.table)
library(stringr)
library(text2vec)
library(ggplot2)
library(plotly)
library(umap)
library(Rtsne)
source("utils.R",encoding="UTF-8")






coord = fread("data/silver/embedding_verbatim.csv",colClasses = c("id"="character"))
coord_matrix = as.matrix(coord[,-c("id")])
# row.names(coord_matrix) <- coord$id

methodes = c("none","row_L2_norm","col_norm_std")
# methode = "row_L2_norm"
methode = "none"
# methode = "col_norm_std"

if (methode == "none"){
  print("none")
} else if(methode == "row_L2_norm"){
  print("row_L2_norm")
  coord_matrix = coord_matrix/sqrt(rowSums(coord_matrix^2))
} else if (methode == "col_norm_std"){
  print("col_norm_std")
  # row_mean = colSums(coord_matrix)/nrow(coord_matrix)
  row_mean = apply(coord_matrix,2,mean)
  row_sd = apply(coord_matrix,2,sd)
  coord_matrix = t(apply(coord_matrix,1,function(row){(row - row_mean)/row_sd}))
  print(max(abs(colSums(coord_matrix))))
  print(max(abs(colSums(coord_matrix^2)/nrow(coord_matrix))))
}

dimReds = c("raw_tsne","raw_umap","svd100_tsne")
dimRed = "raw_umap"

if(dimRed == "raw_umap"){
  embedding = umap(coord_matrix)
  coord2D = data.table(embedding$layout)
  coord2D$id = coord$id
} else if(dimRed == "raw_tsne"){
  if(any(duplicated(coord_matrix))){
    coord_matrix = cbind(coord_matrix,sample(coord_matrix[,1]))
  }
  embedding = Rtsne(coord_matrix)
  coord2D = data.table(embedding$Y)
  coord2D$id = coord$id
}


verbatim = fread("data/silver/verbatim_consolides.csv",encoding="UTF-8",colClasses = "character")
verbatim = verbatim[question=="q20"]
verbatim = merge(verbatim,coord2D,by="id")


g <- ggplot(verbatim)+aes(x=V1,y=V2,color=site,label=text)+geom_point()
ggsave(g,filename = sprintf("output/embedding_french_semantic___%s___%s.png",methode,dimRed))

ggplotly(g)


