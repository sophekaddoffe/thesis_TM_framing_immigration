library(dplyr)
library(quanteda)
library(topicmodels)
library(seededlda)

#setwd("C:/Users/sophi/Documents/Masterarbeit")
speeches <- read.csv("speeches.csv") |>  # 907,644 obs
  filter(positionShort != "Presidium of Parliament", # 522,671 obs
         nchar(speechContent) > 100) #473255


speeches_corpus <- corpus(x = speeches,
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
dtm

# Exclude words with too low frequency
dtm <- dfm_trim(dtm, min_termfreq = 5)

dim(dtm)

dict2 <- dictionary(file = "dictionary2.yml") # dictionary.yml -> only immigration, dictionary2.yml more seeded topics!!!
#print(dict)


lda_seed <- textmodel_seededlda(dtm, dict2, 
                                residual = 3, 
                                batch_size = 0.01, # must be between 0 and 1
                                weight = 0.1,
                                auto_iter = TRUE,
                                verbose = TRUE)

lda_seed <- readRDS("final_seeded_model_ALL_DATA")

topic_dist_over_doc_ALL <- as.data.frame(lda_seed$theta)
#which.max(topic_dist_over_doc$immigration)

topic_dist_over_doc_ALL <- topic_dist_over_doc_ALL |> 
  arrange(desc(immigration))


terms(lda_seed, n = 15)

topic_dist_over_doc_ALL$id <- rownames(topic_dist_over_doc_ALL)

df <- merge(topic_dist_over_doc_ALL, speeches, by = "id")|> 
  arrange(desc(immigration))

min(df$immigration) # 7.83024e-05
max(df$immigration) # 0.6679245
mean(df$immigration) # 0.02484453
sd(df$immigration) # 0.04487275

hist(df$immigration,
     xlab = "Immigration topic",
     main = "Frequency histogram")

# Threshold for classifying documents as immigration-rich
threshold <- 0.1  

# Classify documents as immigration-rich based on the threshold
df <- df |> 
  mutate(Immigration_Rich = ifelse(immigration > threshold, TRUE, FALSE)) 


summary(df$Immigration_Rich)
