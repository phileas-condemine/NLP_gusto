library(data.table)
library(stringr)
library(text2vec)
library(ggplot2)
library(plotly)
library(gistr)
devtools::install_github("cpsievert/LDAvis")
source("utils.R",encoding="UTF-8")


verbatim = fread("data/silver/verbatim_consolides_separated.csv",encoding="UTF-8",colClasses = "character")
# verbatim = fread("data/silver/verbatim_consolides.csv",encoding="UTF-8",colClasses = "character")
verbatim = verbatim[question=="q20"]
verbatim = verbatim[,text := tolower(text)]
verbatim = verbatim[,.SD[1],by=text]
verbatim[,.N,by=text][N>1]
verbatim = verbatim[text!=""]
verbatim = verbatim[nchar(text)>10]

my_stopwords = c(tm::stopwords("fr"),"a","rien","ras","e","avoir","être","j'ai","j'ai_être","dan","c'est")

my_text = verbatim$text
lemmatizer = create_lemmatizer(my_text)
it = text2vec::itoken(na.omit(prep_verbatim(my_text,dictionary = lemmatizer)), 
                      tokenizer = tok_no_stem_fun, 
                      # tokenizer = tok_fun, 
                      progressbar = FALSE)
vocab = text2vec::create_vocabulary(it,stopwords =  my_stopwords, ngram = c(1L, 2L))
vocab = text2vec::prune_vocabulary(vocab,doc_count_min = 10)
vectorizer = text2vec::vocab_vectorizer(vocab)
dtm = text2vec::create_dtm(it, vectorizer)


if(!file.exists("output/model_LDA_q20.RDS")){
  lda_model = LDA$new(n_topics = 15, doc_topic_prior = 0.1, topic_word_prior = 0.01)
  doc_topic_distr =
    lda_model$fit_transform(x = dtm, n_iter = 1000,
                            convergence_tol = 0.001, n_check_convergence = 25,
                            progressbar = FALSE)
  saveRDS(lda_model,"output/model_LDA_q20.RDS")
  saveRDS(doc_topic_distr,"output/doc_topic_distr_LDA_q20.RDS")
} else {
  lda_model = readRDS("output/LDA_q20.RDS")
  doc_topic_distr = readRDS("output/doc_topic_distr_LDA_q20.RDS")
}

lda_model$plot(out.dir = "output/LDA_q20/")
# lda_model$plot()


### Echantillon de verbatim

doc_topic_distr_dt = data.table(doc_topic_distr)
doc_topic_distr_dt$id = verbatim$id
doc_topic_distr_dt = doc_topic_distr_dt[rowSums(doc_topic_distr)>0]
doc_topic_distr_dt = melt(doc_topic_distr_dt,id.vars = "id")
doc_topic_distr_dt[verbatim,text:=i.text,on="id"]
doc_topic_distr_dt[verbatim,site:=i.site,on="id"]

# on prend le topic principal de chaque verbatim
setorder(doc_topic_distr_dt,-value)
verbatim_main_topic = doc_topic_distr_dt[,.SD[1],by=id]
verbatim_main_topic$ordre = sample(1:nrow(verbatim_main_topic))

print("on les met dans l'ordre aléatoire pour ne pas se concentrer sur les value à 1 qui sont parfois moins pertinentes")

# échantillon de 50 verbatim par topic

setorder(verbatim_main_topic,ordre)
echantillon_verbatim_by_topic = verbatim_main_topic[value > .5,.SD[1:50],by=variable][,-c("ordre")]
setorder(echantillon_verbatim_by_topic,variable,-value)
echantillon_verbatim_by_topic[,.N,by=variable]
DT::datatable(echantillon_verbatim_by_topic)


# distribution des sites par topic

distr_site_par_topic = verbatim_main_topic[value>0.5,.(nb_verbatim = .N),by=.(topic = variable,site)]
distr_site_par_topic[,nb_verbatim_site := sum(nb_verbatim),by=site]
distr_site_par_topic[,nb_verbatim_topic := sum(nb_verbatim),by=topic]

distr_site_par_topic[,prop_verbatim_site_in_topic := nb_verbatim/nb_verbatim_site]

g <- ggplot(distr_site_par_topic) + 
  aes(x=topic,y=prop_verbatim_site_in_topic,fill=site) +
  geom_col(position = "dodge")+ggtitle("Distribution des verbatim de chaque site par topic")
ggplotly(g)

table(rowSums(doc_topic_distr))

# # Thématiques LDA
# - produits végétariens, végétal, légumes, accompagnement
# - produits bio, locaux, de saison
# - choix de produits "variété", "diversité", nombre de stands
# - qualité, fraîcheur des produits
# - produits spécifiques 
#   - pizzas
#   - légumes, plats végétariens, accompagnements Problème de "saveur", "assaisonnement", "sauce", "gout", "epices"
#   - steack hachés et frites, "qualité" "dégradée", "remettre", "changer" (passage du steak qualité bouchère au steak semelle) (on voit le terme "surgelé")
#   - fruits, à un prix raisonnable
#   - salades
# - bruit dans la cantine
# - amabilité du personnel
# - "affluence" "fréquentation", "queue", "temps" ou file d'"attente" "caisse" & "stands" en lien avec 
#   - "bruit"
#   - manque de "place"
#   - plat déjà "froid" le temps de trouver une place assise
# - "developper" offre à "emporter" "click & collect" "sandwich"
# - prix en augmentation "trop cher"
# - contraste "chaud" / "froid"
# - "gras" associé à salade, légumes, dessert, viande...
# - restaurant d'entreprise vs "ticket restaurant" et "extérieur"
# - "cuisson" (viande trop cuite ?)
# - considérations générales :"quantités" / "qualité" / "prix" / "bruit" et "monde" avec les termes "trop" "baisse","diminue", "reduites","forte", "moins"
# - manque de choix à partir d'une certaine heure (13h ?) ""souvent" "arrive" "reste" manque" "choix" "diversite" 

# Est-ce qu'il manque qqch qu'on s'attendait à voir apparaître ?


# next steps 
# - construire un plan d'annotation avec définitions, mots clefs et exemple
# - annoter une fraction des documents en prévoyant un "pertinent hors-plan"
# - vérifier qu'il ne manque pas de thématiques
# - annoter suffisamment pour estimer les fréquences
# - si fréquence trop faible, regrouper, modifier le plan ?
# - continuer d'annoter jusqu'à obtenir un volume suffisant pour entraîner un modèle ?

