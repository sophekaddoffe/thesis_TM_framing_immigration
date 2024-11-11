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
lda_3res <- textmodel_seededlda(dtm, dict2, 
                                residual = 3, 
                                batch_size = 0.01, 
                                weight = 0.1, # seed weight
                                auto_iter = TRUE,
                                verbose = TRUE)

topic_dist_over_doc3res <- as.data.frame(lda_3res$theta)
word_dist_over_topic3res <- as.data.frame(lda_3res$phi)

topic_dist_over_doc3res <- topic_dist_over_doc3res |> 
  arrange(desc(immigration))

topic_dist_over_doc3res$id <- rownames(topic_dist_over_doc3res)

df3res <- merge(topic_dist_over_doc3res, random_20T, by = "id")|> 
  arrange(desc(immigration))

terms(lda_3res, n = 15)

min(df3res$immigration) # 0.0001310101
max(df3res$immigration) # 0.6590406
mean(df3res$immigration) # 0.02523941
sd(df3res$immigration) # 0.04136733

hist(df3res$immigration,
     xlab = "Immigration topic",
     main = "Frequency histogram")

# Threshold for classifying documents as immigration-rich
threshold <- 0.1  # 2.5% of topic proportion

# Classify documents as immigration-rich based on the threshold
df3res <- df3res %>%
  mutate(Immigration_Rich = ifelse(immigration > threshold, TRUE, FALSE)) 


summary(df3res$Immigration_Rich)
# threshold 0.025 classifies 4588 documents as immigration-rich, 15412 as non-immigration-rich!!
# threshold 0.05 classifies 2420 documents as immigration-rich, 17580 as not
# threshold 0.1 classifies 905 documents as immigration-rich, reading the one with the lowest topic score: it is clearly about immigration!

#########################################################################################
# KEYWORD APPROACH

speeches_immigr <- random_20T %>% filter(
  grepl('[aA]syl* | [aA]usländer* | [eE]inwander* | [fF]lüchtling |
        [gG]eflücht* | [iI]ntegration* | [mM]igrant* | [mM]igration* | 
        [iI]mmigration* | [iI]mmigrant* | [nN]iederlassungsrecht | [fF]amiliennachzug | 
        [sS]taatsbürgerschaftsgesetz | [bB]amf | BAMF | [rR]efugee* | *[vV]ertriebene* | 
        [sS]chutzsuchende | [hH]eimatvertriebene | [aA]ussiedler | *[zZ]uwanderung*', 
        speechContent)
) # 589 speeches containing the keywords

write.csv(speeches_immigr, "keywords_dataframe.csv")