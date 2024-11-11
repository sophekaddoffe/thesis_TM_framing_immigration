iolibrary(dplyr)
library(quanteda)
library(topicmodels)
library(seededlda)

#setwd("C:/Users/sophi/Documents/Masterarbeit")
speeches <- read.csv("speeches.csv") |>  # 907,644 obs
  filter(positionShort != "Presidium of Parliament", # 522,671 obs
         nchar(speechContent) > 100) #473255

set.seed(6764) #45693 3209 983478 4829
random_10T <- speeches[sample(nrow(speeches), 10000), ]



speeches_corpus <- corpus(x = random_10T,
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

# inspection
mytokens

# Create document term matrix
dtm <- dfm(x = mytokens)
dtm # 32,554 features

# Exclude words with too low frequency
dtm <- dfm_trim(dtm, min_termfreq = 5)

dim(dtm)
dtm # 5,659 features

#dict <- dictionary(file = "dictionary.yml")
dict2 <- dictionary(file = "dictionary2.yml") # dictionary.yml -> only immigration, dictionary2.yml more seeded topics!!!
#print(dict)



#k <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
k <- c(1:10, 15, 20, 30, 35, 40, 45, 50) #, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100
# Define the vector of granularity parameters
min_size <- c(0.01) # , 0.05, 0.1

# Initialize an empty data frame to store the results
results <- data.frame(Residual_Topics = numeric(),
                      #Granularity = numeric(),
                      Divergence_Score = numeric())



# Loop over each number of residual topics
for (i in k) {
  # Fit the topic model
  set.seed(45693)  # Set seed for reproducibility
  lda_model <- textmodel_seededlda(dtm, dict2, 
                                   residual = i,
                                   batch_size = 0.01,
                                   auto_iter = TRUE,
                                   verbose = FALSE)
  
  # Loop over each granularity parameter
  for (size in min_size) {
    # Compute divergence score
    div_score <- divergence(lda_model, min_size = size)
    
    # Append results to the data frame
    results <- rbind(results, data.frame(Residual_Topics = i,
                                         #Granularity = size,
                                         Divergence_Score = div_score))
  }
}


library(ggplot2)

results_filtered <- subset(results, Granularity == 0.01)


# Plot the results
ggplot(results, aes(x = Residual_Topics, y = Divergence_Score)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Topics (k)", y = "Divergence Score") +
  scale_x_continuous(breaks = k) +
  theme_minimal()
