library(dplyr)
library(quanteda)
library(topicmodels)
library(seededlda)

#setwd("C:/Users/sophi/Documents/Masterarbeit")
speeches <- read.csv("speeches.csv") |>  # 907,644 obs
  filter(positionShort != "Presidium of Parliament", # 522,671 obs
         nchar(speechContent) > 100) #473,255

set.seed(45693)
random_20T <- speeches[sample(nrow(speeches), 20000), ]



speeches_corpus <- corpus(x = random_20T,
                          text_field = "speechContent",
                          meta = list("lastName", "politicianID", "factionID"),
                          docid_field = 'id')

# Tokenize & clean from particular types of words
mytokens <- tokens(x = speeches_corpus, 
                   remove_punct = TRUE, 
                   remove_numbers = TRUE, 
                   remove_symbols = TRUE,
                   remove_url = TRUE,
                   padding = FALSE) # empty spaces removed

# Remove german stop words
mytokens <- tokens_remove(x = mytokens,
                          stopwords("de"),
                          padding = FALSE)
# Make tokens lowercase
mytokens <- tokens_tolower(x = mytokens)

# How does it look now?
mytokens

# Create document term matrix
dtm <- dfm(x = mytokens)
dtm # 196,747 features

# Exclude words with too low frequency
dtm <- dfm_trim(dtm, min_termfreq = 5)

dim(dtm)
dtm # 43,735 features

dict2 <- dictionary(file = "dictionary2.yml") # dictionary.yml -> only immigration, dictionary2.yml more seeded topics!!!
#print(dict)

#########################################################################################
set.seed(45693)
lda_6res <- textmodel_seededlda(dtm, dict2, 
                                 residual = 3, # how to choose this number? -> higher !!!
                                 batch_size = 0.01, # set to 10? -> no, must be between 0 and 1!!
                                 weight = 0.1,
                                 auto_iter = TRUE,
                                 verbose = TRUE)

saveRDS(lda_6res, file = "SeM_6res_newseeds") 

lda_6res_oldseeds <- readRDS("SeM_6res") # SeM_6res contains lda with old seedwords


top15terms_6res_oldseeds <- as.data.frame(terms(lda_6res_oldseeds, n = 15))
#top15terms_6res <- as.data.frame(terms(lda_6res, n = 15))
top15terms_6res_highweights <- as.data.frame(terms(lda_6res, n = 20))

topic_dist_over_doc6res <- as.data.frame(lda_6res$theta)
#which.max(topic_dist_over_doc$immigration)

word_dist_over_topic6res <- as.data.frame(lda_6res$phi)

topic_dist_over_doc6res <- topic_dist_over_doc6res |> 
  arrange(desc(immigration))

topic_dist_over_doc6res$id <- rownames(topic_dist_over_doc6res)

df6res <- merge(topic_dist_over_doc6res, random_20T, by = "id")|> 
  arrange(desc(immigration))


min(df6res$immigration) # 0.000137817
max(df6res$immigration) # 0.4620344
mean(df6res$immigration) # 0.01203692

top5_6res <- as.data.frame(head(df6res, 5))|> 
  select(speechContent, immigration)

bottom5_6res <- as.data.frame(tail(df6res, 5))|> 
  select(speechContent, immigration)

random5_6res <- df6res[sample(nrow(df6res), 5), ]|> 
  select(speechContent, immigration)


read_speeches6res <- rbind(top5_6res, bottom5_6res, random5_6res)

write.csv(read_speeches6res, "read_speeches50res.csv")
#########################################################################################
# set.seed(45693)
# lda_50res <- textmodel_seededlda(dtm, dict2, 
#                                          residual = 50, # how to choose this number? -> higher !!!
#                                          batch_size = 0.01, # set to 10? -> no, must be between 0 and 1!!
#                                          auto_iter = TRUE,
#                                          verbose = TRUE)

#saveRDS(lda_50res, file = "SeM_50res") 
lda_50res <- readRDS("SeM_50res")

top15terms_50res <- as.data.frame(terms(lda_50res, n = 15))

topic_dist_over_doc50res <- as.data.frame(lda_50res$theta)
#which.max(topic_dist_over_doc$immigration)

word_dist_over_topic50res <- as.data.frame(lda_50res$phi)

topic_dist_over_doc50res <- topic_dist_over_doc50res |> 
  arrange(desc(immigration))

topic_dist_over_doc50res$id <- rownames(topic_dist_over_doc50res)

df50res <- merge(topic_dist_over_doc50res, random_20T, by = "id")|> 
  arrange(desc(immigration))


min(df50res$immigration) # 0.000137817
max(df50res$immigration) # 0.4620344
mean(df50res$immigration) # 0.01203692

top5_50res <- as.data.frame(head(df50res, 5))|> 
  select(speechContent, immigration)

bottom5_50res <- as.data.frame(tail(df50res, 5))|> 
  select(speechContent, immigration)

random5_50res <- df50res[sample(nrow(df50res), 5), ]|> 
  select(speechContent, immigration)


read_speeches50res <- rbind(top5_50res, bottom5_50res, random5_50res)

write.csv(read_speeches50res, "read_speeches50res.csv")
#topwords20of30_TM13 <- as.data.frame(terms(lda_seed_13topics, 50)[, 1:20]) # there are 20 words in dictionary for immigration topic
#immigration_words13 <- topwords20of30_TM13$immigration
#divergence(lda_seed_13topics)
#random_20T[random_20T$id==705624, ]

#########################################################################################
set.seed(45693)
lda_100res <- textmodel_seededlda(dtm, dict2, 
                                 residual = 100, # how to choose this number? -> higher !!!
                                 batch_size = 0.01, # set to 10? -> no, must be between 0 and 1!!
                                 auto_iter = TRUE,
                                 verbose = TRUE)

saveRDS(lda_100res, file = "SeM_100res") 

topic_dist_over_doc100res <- as.data.frame(lda_100res$theta)
#which.max(topic_dist_over_doc$immigration)

word_dist_over_topic100res <- as.data.frame(lda_100res$phi)

topic_dist_over_doc100res <- topic_dist_over_doc100res |> 
  arrange(desc(immigration))

topic_dist_over_doc100res$id <- rownames(topic_dist_over_doc100res)

df100res <- merge(topic_dist_over_doc100res, random_20T, by = "id")|> 
  arrange(desc(immigration))


min(df100res$immigration) # 0.0001497454
max(df100res$immigration) # 0.466805
mean(df100res$immigration) # 0.007976062
top5_100res <- as.data.frame(head(df100res, 5))|> 
  select(speechContent, immigration)

bottom5_100res <- as.data.frame(tail(df100res, 5))|> 
  select(speechContent, immigration)

random5_100res <- df100res[sample(nrow(df100res), 5), ]|> 
  select(speechContent, immigration)

read_speeches100res <- rbind(top5_100res, bottom5_100res, random5_100res)
write.csv(read_speeches100res, "read_speeches100res.csv")

#########################################################################################
set.seed(45693)
lda_500res <- textmodel_seededlda(dtm, dict2, 
                                  residual = 500, # how to choose this number? -> higher !!!
                                  batch_size = 0.01, # set to 10? -> no, must be between 0 and 1!!
                                  auto_iter = TRUE,
                                  verbose = TRUE)

saveRDS(lda_500res, file = "SeM_500res") 

topic_dist_over_doc500res <- as.data.frame(lda_500res$theta)
#which.max(topic_dist_over_doc$immigration)

word_dist_over_topic500res <- as.data.frame(lda_500res$phi)

topic_dist_over_doc500res <- topic_dist_over_doc500res |> 
  arrange(desc(immigration))

topic_dist_over_doc500res$id <- rownames(topic_dist_over_doc500res)

df500res <- merge(topic_dist_over_doc500res, random_20T, by = "id")|> 
  arrange(desc(immigration))


min(df500res$immigration) # 0.0001303101
max(df500res$immigration) # 0.3591549
mean(df500res$immigration) # 0.002566482
top5_500res <- as.data.frame(head(df500res, 5))|> 
  select(speechContent, immigration)

bottom5_500res <- as.data.frame(tail(df500res, 5))|> 
  select(speechContent, immigration)

random5_500res <- df500res[sample(nrow(df500res), 5), ] |> 
  select(speechContent, immigration)

read_speeches500res <- rbind(top5_500res, bottom5_500res, random5_500res)
write.csv(read_speeches500res, "read_speeches500res.csv")

#########################################################################################

divergence(lda_50res)
divergence(lda_100res)
divergence(lda_500res)
divergence()

topicdoc::topic_coherence(lda_50res, dtm)

top_terms <- terms(lda_50res, 10)

#########################################################################################
