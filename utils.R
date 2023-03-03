
get_export_de_masse_avec_justif_demande_et_fourni = function(path_to_exports_de_masse = "data/export_de_masse_2022/",regex_questionnaire){
  print("func: get_export_de_masse_avec_justif_demande_et_fourni")
  exports_de_masse = rbindlist(lapply(
    list.files(path_to_exports_de_masse,full.names = T),
    function(path_to_ref){
      export_de_masse = setDT(readxl::read_excel(path_to_ref))
      names(export_de_masse) <- std_string(names(export_de_masse))
      unique(export_de_masse[,.(siren,orias,nom_de_l_intermediaire,question,reference,reponse,demandees,fournies)])
    }
  ))
  exports_de_masse_justif_demande = exports_de_masse[demandees >0]
  exports_de_masse_justif_demande[,.N,by=.(fournies > 0,reponse)]
  exports_de_masse_justif_demande_et_fourni = exports_de_masse_justif_demande[fournies>0]
  exports_de_masse_justif_demande_et_fourni_si_et_soc = exports_de_masse_justif_demande_et_fourni[grepl(regex_questionnaire,reference)]
  exports_de_masse_justif_demande_et_fourni_si_et_soc
}

dirname2 = function(x){
  stringr::str_remove(x,"/[^/]*$")
}
basename2 = function(x){
  stringr::str_extract(x,"/[^/]*$")%>%str_remove("^/")
}

std_string = function(nms,encoding="UTF-8"){
  nms <- tolower(nms)
  nms <- iconv(nms,from=encoding,to="ASCII//TRANSLIT")
  nms <- gsub("[^a-z]","_",nms)
  nms <- gsub("_+","_",nms)
  nms <- gsub("_*$","",nms)
  nms
}

prep_verbatim = function(x,...){
  x = prep_fun(x)
  x = lemmar::lemmatize_fr(x,...)
  # x = iconv(x,to="ASCII//TRANSLIT")
  x
}

prep_fun = function(x){
  x = tolower(x)
  x = gsub("[^a-zéèêàùûîôâïöüëç]"," ",x)
  x = gsub("( |^)(l|s|m|n|d|j|c|qu|t) "," ",x)
  x
}

random_n_colors = function(n){
  sapply(1:n,function(i){
    paste0("#",paste(sample(c(0:9,LETTERS[1:6]),6,replace = T),collapse = "")) 
  })
}

get_clean_folder_names = function(path){
  files_paths = list.files(path,recursive = T,full.names = T)
  files_paths = dirname(files_paths)
  files_paths = gsub("/+","/",files_paths)
  files_paths
}






standardize_names = function(nm,max_len = 181){
  nm <- gsub("\\.pdf$","_",nm)
  nm <- gsub(" ","_",nm)
  nm <- gsub("\\.","_",nm)
  nm <- gsub("-","_",nm)
  nm <- gsub("'","_",nm)
  nm <- gsub("°","_",nm)
  nm <- gsub("/","___",nm)
  nm <- gsub("_*$","",nm)
  nm <- iconv(nm,to="UTF-8")
  nm <- iconv(nm,from="UTF-8",to="ASCII//TRANSLIT")
  nm <- tolower(nm)
  nm <- gsub("\\?","_",nm)
  nm <- substr(nm,1,max_len)
  nm
}



tok_no_stem_fun =function(x) {
  tokens = word_tokenizer(x)
  tokens = lapply(tokens, iconv,from="UTF-8",to="ASCII//TRANSLIT")
  tokens = tokens[nchar(tokens)>2]
  tokens
}


tok_fun =function(x) {
  tokens = word_tokenizer(x)
  tokens = lapply(tokens, function(x) SnowballC::wordStem(x, language="fr"))
  tokens = lapply(tokens, iconv,from="UTF-8",to="ASCII//TRANSLIT")
  tokens = tokens[nchar(tokens)>2]
  tokens
}

create_lemmatizer = function(my_text){
  print("func: create_lemmatizer")
  tok_fun = text2vec::word_tokenizer
  it = text2vec::itoken(na.omit(prep_fun(my_text)), 
                        tokenizer = tok_fun, 
                        progressbar = FALSE)
  post_stopwords = c("avoir","être","j'ai","j'ai_être","dan","c'est")
  my_stopwords = c(tm::stopwords("fr"),"a","rien","ras","e",post_stopwords)
  
  vocab = text2vec::create_vocabulary(it,stopwords =  my_stopwords, ngram = c(1L, 1L))
  # vocab = prune_vocabulary(vocab,term_count_min = 1)
  setDT(vocab)
  print("get vocab => lemmatize => stem => get most freq form")
  lemmatizer = vocab[,.(token = term, term_count)]
  lemmar::hash_lemma_fr
  lemmatizer[,lemma := lemmar::lemmatize_words_fr(token)]  
  lemmatizer[,stem := SnowballC::wordStem(lemma,"fr")]
  setorder(lemmatizer,-term_count)
  lemmatizer[lemmatizer[,.SD[1],by=stem],lemma:=i.token,on="stem"]
  print(c(uniqueN(lemmatizer$token),uniqueN(lemmatizer$stem)))
  lemmatizer = lemmatizer[,.(token,lemma)]
  lemmatizer
}

create_text_embedding = function(dt,
                                 my_stopwords = c(tm::stopwords("fr"),"a","rien","ras","e","avoir","être","j'ai","j'ai_être","dan","c'est"),
                                 dimRed2D = "UMAP", #c("UMAP","tSNE")
                                 tok_fun = tok_fun
){
  print("func: create_text_embedding")
  my_text = dt$text
  lemmatizer = create_lemmatizer(my_text)
  it = text2vec::itoken(na.omit(prep_verbatim(my_text,dictionary = lemmatizer)), 
                        tokenizer = tok_fun, 
                        progressbar = FALSE)
  vocab = text2vec::create_vocabulary(it,stopwords =  my_stopwords, ngram = c(1L, 2L))
  vocab = text2vec::prune_vocabulary(vocab,doc_count_min = 10)
  vectorizer = text2vec::vocab_vectorizer(vocab)
  dtm = text2vec::create_dtm(it, vectorizer)
  tfidf = text2vec::TfIdf$new()
  
  n_dim = 64
  coord_OK = F
  iter = 0
  while(!coord_OK & n_dim>2){
    iter = iter + 1
    try({
      n_dim = n_dim / 2
      print(sprintf("run LSA with %s dimensions",n_dim))
      lsa = text2vec::LSA$new(n_topics = n_dim)
      doc_embeddings = text2vec::fit_transform(dtm, tfidf)
      doc_embeddings = text2vec::fit_transform(doc_embeddings, lsa)
      
      if(!"coord"%in%ls()){
        print("valeur par défaut d'embedding LSA")
        coord = data.table(doc_embeddings[,1:2])
      }
      
      doc_embeddings = cbind(doc_embeddings,sample(doc_embeddings[,1])/(1E4)) # add random col to fix duplicates !
      if(dimRed2D == "tSNE"){
        print('embedding tSNE')
        doc_tsne = Rtsne::Rtsne(doc_embeddings,pca = F)
        coord = data.table(doc_tsne$Y)
      } else if(dimRed2D == "UMAP"){
        print('embedding UMAP')
        doc_umap = umap::umap(doc_embeddings)
        coord = data.table(doc_umap$layout)
      }
      coord_OK = T
    })
    
  }
  
  
  # coord$file_path = dt$file_path[-dup]
  # coord$text = dt$text[-dup]
  # coord$file_path = dt$file_path
  coord$text = dt$text
  coord$type = ifelse(coord_OK,paste0("tSNE_",n_dim,"D"),"LSA")
  coord
  
}

compute_local_embedding = function(full_dt){
  print("func: compute_local_embedding")
  
  full_dt_ = full_dt[text!=" "]
  prefix = "Document au format PDF : "
  full_dt_[,text:=paste0(prefix,text)] # permet d'éviter les doc "vides" à cause du vocab pruning
  full_dt_ = full_dt_[,.SD[1],by=.(text)]
  full_dt_[,.N,by=reference]
  
  
  coord_by_reference = rbindlist(pbapply::pblapply(split(full_dt_,full_dt_$reference),function(dt){
    ref = unique(dt$reference)
    print(c(ref,nrow(dt)))
    sub_coord = create_text_embedding(dt)
    sub_coord$reference = ref
    sub_coord
  }))
  
  coord_by_reference[,.(uniqueN(reference)),by=type]
  coord_by_reference$reference = NULL
  coord_corpus_complet = create_text_embedding(full_dt_)
  coord_synthese = rbindlist(list(
    coord_by_reference[grepl("^tSNE",type)],
    merge(coord_corpus_complet,coord_by_reference[!grepl("^tSNE",type),.(text)],by="text")
  ),use.names = T)
  coord_synthese[,text:=stringr::str_remove(text,prefix)]
  full_dt = merge(full_dt,coord_synthese, by = "text")
  full_dt
}




referentiel = rbindlist(lapply(
  list.files("data/export_de_masse_2022/",full.names = T),
  function(path_to_ref){
    export_de_masse = setDT(readxl::read_excel(path_to_ref))
    unique(export_de_masse[,.(libelle = Question,reference = `Référence`)])
  }
))


get_info_files_available = function(path_to_docs = "data/justif"){
  print("func: get_info_files_available")
  files_available = data.table(path = list.files(path_to_docs,full.names = T,recursive = T))
  files_available[,extension := stringr::str_extract(path,"[^.][A-z0-9]+$")]
  files_available[,.N,by=extension][order(-N)]
  files_available[!extension %in% c("pdf","PDF")]
  files_available[,fichier := basename(path)]
  files_available[,reference := stringr::str_extract(fichier,"((EDISo)|(EDISI))_[0-9]+")]
  files_available[referentiel,libelle := i.libelle,on="reference"]
  files_available
}



add_tokens_count = function(dt,count_min = 100){
  print("func: add_tokens_count")
  non_empty_txt = paste0("document ",dt$text)
  it_train = itoken(non_empty_txt, 
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    progressbar = FALSE)
  print("create vocab")
  vocab = create_vocabulary(it_train,stopwords =  c(tm::stopwords("fr"),"a"), ngram = c(1L, 1L))
  vocab = prune_vocabulary(vocab,doc_count_min = count_min)
  vocab
  vectorizer = vocab_vectorizer(vocab)
  print("create dtm")
  dtm_train = create_dtm(it_train, vectorizer,type = "dgTMatrix")
  print("create rowSums")
  word_count = Matrix::rowSums(dtm_train)
  dt$nb_tokens = word_count-1
  dt
}

comptages_de_caracteres = function(dt){
  print("func: comptages_de_caracteres")
  print("nb_alphabetic")
  dt[,nb_alphabetic:=stringr::str_count(tolower(text),"[a-zéèêàùûîôâïöüëç]")]
  print("nb_numbers")
  dt[,nb_numbers:=stringr::str_count(text,"[0-9]")]
  print("nb_alphanum")
  dt[,nb_alphanum:=stringr::str_count(tolower(text),"[a-z0-9éèêàùûîôâïöüëç]")]
  print("nb_not_alphanum")
  dt[,nb_not_alphanum:=stringr::str_count(tolower(text),"[^a-z0-9éèêàùûîôâïöüëç ]")]
  print("nb_euros")
  dt[,nb_euros:=stringr::str_count(text,"€")]
  print("nb_dollar")
  dt[,nb_dollar:=stringr::str_count(text,"\\$")]
  print("nb_pounds")
  dt[,nb_pounds:=stringr::str_count(text,"£")]
  print("nb_char")
  dt[,nb_char := nchar(text)]
  print("add_tokens_count")
  dt = add_tokens_count(dt)
  dt
}


prep_text_for_similarity = function(text){
  text = tolower(text)
  text = gsub("\n"," ",text)
  text = gsub("[^a-z0-9éèêàùûîôâïöüëç]"," ",text)
  text = gsub("(^ +)|( +$)","",text)
  text = gsub(" +"," ",text)
  text
}

score_matched_keywords = function(dt,keywords,id_var = "file_path"){
  print("Nettoyage du texte")
  setnames(dt,id_var,"id_var_")
  dt[,text_clean := tolower(text)]
  dt[,text_clean := gsub("[^a-zéèàâùôîûêöïüëç0-9]"," ",text_clean)]
  dt[,text_clean := gsub("(^ +)|( +$)","",text_clean)]
  dt[,text_clean := gsub("( )+"," ",text_clean)]
  dt[,text_clean := iconv(text_clean,from="UTF-8",to="ASCII//TRANSLIT")]
  
  print("Comptage des mots clefs trouvés")
  dt$score_mots_clefs = lapply(keywords,function(word){
    stringi::stri_detect_fixed(dt$text_clean,word)
  })%>%do.call(what="cbind")%>%rowMeans()
  
  print("Ajout de la liste des mots clefs trouvés")
  dt_mots_clefs_trouves = rbindlist(lapply(keywords,function(word){
    dt[stringi::stri_detect_fixed(text_clean,word),.(id_var_,mot=word)]
  }))
  setorder(dt_mots_clefs_trouves,mot)
  dt_mots_clefs_trouves = dt_mots_clefs_trouves[,.(mots_clefs_trouves = paste(mot, collapse = " + ")),by=id_var_]
  dt = merge(dt,dt_mots_clefs_trouves,by="id_var_",all.x=T)
  dt = dt[,-c("text_clean")]
  setnames(dt,"id_var_",id_var)
  dt
}


my_default_datatable = purrr::partial(DT::datatable,rownames = F,options=list(pageLength=-1))

