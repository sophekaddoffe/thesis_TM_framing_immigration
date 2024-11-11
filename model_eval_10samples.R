library(seededlda)
library(ggplot2)
library(quanteda)
library(topicmodels)

# Load the speeches data
speeches <- read.csv("speeches.csv") |>  
  filter(positionShort != "Presidium of Parliament",
         nchar(speechContent) > 100)

# Define the number of random samples
num_samples <- 10

# Define the vector of numbers of residual topics
k <- c(1:10, 15, 20, 30, 35, 40, 45, 50)

# Initialize an empty data frame to store the results
results <- data.frame(Residual_Topics = numeric(),
                      Divergence_Score = numeric(),
                      Sample = numeric())

# Loop over each random sample
for (s in 1:num_samples) {
  # Take a random sample of 1000 speeches
  random_1T <- speeches[sample(nrow(speeches), 1000), ]
  
  # Create a corpus from the random sample
  speeches_corpus <- corpus(x = random_1T,
                            text_field = "speechContent",
                            meta = list("lastName", "politicianID", "factionID"),
                            docid_field = 'id')
  
  # Tokenize & clean from particular types of words
  mytokens <- tokens(x = speeches_corpus, 
                     remove_punct = TRUE, 
                     remove_numbers = TRUE, 
                     remove_symbols = TRUE,
                     remove_url = TRUE,
                     padding = FALSE)
  
  # Remove German stop words
  mytokens <- tokens_remove(x = mytokens,
                            stopwords("de"),
                            padding = FALSE)
  
  # Make tokens lowercase
  mytokens <- tokens_tolower(x = mytokens)
  
  # Create document term matrix
  dtm <- dfm(x = mytokens)
  
  # Exclude words with too low frequency
  dtm <- dfm_trim(dtm, min_termfreq = 5)
  
  # seeds
  dict2 <- dictionary(file = "dictionary2.yml")
  
  # Loop over each number of residual topics
  for (i in k) {
    # Fit the topic model
    set.seed(45693)  # Set seed for reproducibility
    lda_model <- textmodel_seededlda(dtm, dict2, 
                                     residual = i,
                                     batch_size = 0.01,
                                     auto_iter = TRUE,
                                     verbose = TRUE) # weight missing here!!! -> 0.1
    
    # Compute divergence score for default granularity (0.01)
    div_score <- divergence(lda_model, min_size = 0.01)
    
    # Append results to the data frame
    results <- rbind(results, data.frame(Residual_Topics = i,
                                         Divergence_Score = div_score,
                                         Sample = s))
  }
}

# Compute the average divergence score for each k based on the 10 samples
avg_results <- aggregate(Divergence_Score ~ Residual_Topics, data = results, FUN = mean)

# Plot the results
ggplot(avg_results, aes(x = Residual_Topics, y = Divergence_Score)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Residual Topics (k)", y = "Average Divergence Score") +
  scale_x_continuous(breaks = k) +
  theme_minimal()
