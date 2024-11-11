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
dtm # 43,745 features

dict <- dictionary(file = "dictionary.yml")
dict2 <- dictionary(file = "dictionary2.yml") # dictionary.yml -> only immigration, dictionary2.yml more seeded topics!!!
#print(dict)

#########################################################################################
set.seed(45693)
lda_seed_13topics <- textmodel_seededlda(dtm, dict2, 
                                residual = 100, # how to choose this number? -> higher !!!
                                batch_size = 0.01, # set to 10? -> no, must be between 0 and 1!!
                                auto_iter = TRUE,
                                verbose = TRUE)

saveRDS(lda_seed_13topics, file = "SeM_13Top_20TRandom") 

topic_dist_over_doc13 <- as.data.frame(lda_seed_13topics$theta)
#which.max(topic_dist_over_doc$immigration)

word_dist_over_topic13 <- as.data.frame(lda_seed_13topics$phi)

topic_dist_over_doc13 <- topic_dist_over_doc13 |> 
  arrange(desc(immigration))

topwords20of30_TM13 <- as.data.frame(terms(lda_seed_13topics, 50)[, 1:20]) # there are 20 words in dictionary for immigration topic

immigration_words13 <- topwords20of30_TM13$immigration

divergence(lda_seed_13topics)

#########################################################################################
set.seed(45693)
lda_seed_1topic <- textmodel_seededlda(dtm, dict, 
                                         residual = 100, # how to choose this number? -> higher !!!
                                         batch_size = 0.01, # set to 10? -> no, must be between 0 and 1!!
                                         auto_iter = TRUE,
                                         verbose = TRUE)

saveRDS(lda_seed_1topic, file = "SeM_1Top_20TRandom") 

topic_dist_over_doc1 <- as.data.frame(lda_seed_1topic$theta)
#which.max(topic_dist_over_doc$immigration)

topic_dist_over_doc1 <- topic_dist_over_doc1 |> 
  arrange(desc(immigration))


topwords20of30_TM1 <- as.data.frame(terms(lda_seed_1topic, 50)[, 1:20])

immigration_words1 <- topwords20of30_TM1$immigration

#########################################################################################
#random_20T[random_20T$id==705624, ]

migration_mentions <- random_20T %>% filter(
  grepl('Migration|Migrant|Immigration|Immigrant|Immigranten', speechContent)
) 

print(intersect(immigration_words1, immigration_words13)) # in top50 words, 37 are identical

mean(topic_dist_over_doc13$immigration)
mean(topic_dist_over_doc1$immigration)



# Assuming df1 and df2 are your data frames

# Subsetting the immigration variable from each data frame
immigration_df1 <- topic_dist_over_doc1$immigration
immigration_df13 <- topic_dist_over_doc13$immigration

# Calculating the absolute difference
immigration_absolute_difference <- abs(immigration_df1 - immigration_df13)

# Calculating the mean difference
mean_difference <- mean(immigration_absolute_difference)

sd(immigration_absolute_difference)
# Print or use the mean_difference as needed
print(mean_difference)

#########################################################################################
# KEYWORD APPROACH

speeches_immigr <- random_20T %>% filter(
  grepl('[aA]syl* | [aA]usländer* | [eE]inwander* | [fF]lüchtling | [gG]eflücht* | [iI]ntegration* | [mM]igrant* | [mM]igration* | [iI]mmigration* | [iI]mmigrant* | [nN]iederlassungsrecht | [fF]amiliennachzug | [sS]taatsbürgerschaftsgesetz | [bB]amf | BAMF | [rR]efugee* | *[vV]ertriebene* | [sS]chutzsuchende | [hH]eimatvertriebene | [aA]ussiedler | *[zZ]uwanderung*', speechContent)
) # 590 speeches containing the keywords

write.csv(speeches_immigr, "keywords_dataframe.csv")



