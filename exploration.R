library(dplyr)
library(quanteda)
library(topicmodels)
library(seededlda)

#setwd("C:/Users/sophi/Documents/Masterarbeit")
speeches <- read.csv("speeches.csv") |>  # 907,644 obs
  filter(positionShort != "Presidium of Parliament", # 522,671 obs
         nchar(speechContent) > 100) #473255


  #filter(politicianId != -1, # don't keep unknown politicians
        # nchar(speechContent) > 200)
  

politicians <- read.csv("politicians.csv") 
# 38,009 politician IDs missing -> find out how many "disctinct politicians" that are, and maybe even keep them and find out how much they talk about immigration

colnames(politicians)[1] = "politicianId" 


factions <- read.csv("factions.csv")

first_20T <- speeches[1:20000, ] 

random_20T <- speeches[sample(nrow(speeches), 20000), ]




migration_mentions <- random_20T %>% filter(
  grepl('Migration|Migrant|Immigration|Immigrant|Immigranten', speechContent)
  ) 

all_speeches_migr <- speeches %>% filter(
  grepl('Migration|Migrant|Immigration|Immigrant|Immigranten', speechContent)
  )

write.csv(all_speeches_migr, "all_speeches_migr.csv")



#########################################################################################
#full_df <- merge(politicians, speeches, by.x = "politicianID", by.y = "politicianID") # not working for some reason
#########################################################################################
seeds <- c("Asylpolitik", "Asyl", "Asylrecht", "Asylant", "Asylanten", "Asylantin", "Asylantinnen", "Asylgesuch", "Asylsuchende",
           "Ausländer", "Ausländerin", "Ausländerinnen", "Ausländergesetz", "Ausländerbehörde",
           "Einwanderung", "Einwanderungsgesetz", "Einwandererfragen", "Einwanderungsminister", "Einwanderungsministerin", "Einwanderer", "Einwandererorganisationen", "Einwanderungspolitik", "Einwanderungskommission", "Einwanderungsbehörde", "Einwanderungspolitik",
           "A-Flüchtlinge", "Ostflüchtlinge", "Flüchtlingsausschuss", "Flüchtlingsverwaltung", "Flüchtlingsfrage",
           "Flüchtlinge", "Geflüchtete", "Flucht", "Fluchtursache", "Fluchtursachen", "Fluchtursachenbekämpfung",
           "Integration", "Integrationsminister", "Integrationsministerin", "Integrationspolitik", 
           "Integrationspolitiker", "Integrationsmaßnahmen", "Integrationsagentur",
           "Migrant", "Migranten", "Migrantin", "Migrantinnen", "Migrationshintergrund", 
           "Migrationsfragen", "Migrationsminister", "Migrationsministerin", "Migrationspolitik", 
           "Migrationsbehörde", "Arbeitsmigration", "Migrationskommission",
           "Immigration", "Immigrant", "Immigranten", "Immigrantin", "Immigrantinnen", 
           "Niederlassungsrecht", "Familiennachzug", "Staatsbürgerschaftsgesetz", "BAMF", "Refugee", "Refugees", "Vertriebene", "Schutzsuchende", "Heimatvertriebene", "Aussiedler", "Zuwanderung"
)

seeds <- as.matrix(seeds)
#########################################################################################

# speeches_corpus <- corpus(x = speeches,
#                        text_field = "speechContent",
#                        meta = list("lastName", "politicianID", "factionID"),
#                        docid_field = 'id')



speeches_corpus <- corpus(x = random_20T,
                          text_field = "speechContent",
                          meta = list("lastName", "politicianID", "factionID"),
                          docid_field = 'id')

speeches_corpus_keywords <- corpus(x = all_speeches_migr,
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

mytokens <- tokens_select(x = mytokens, selection = 'remove',
                          valuetype = 'glob', 
                          pattern = '\n', 
                          padding = FALSE)

mytokens <- tokens_select(x = mytokens, selection = 'remove',
                          valuetype = 'glob', 
                          pattern = '{', 
                          padding = FALSE)

mytokens <- tokens_select(x = mytokens, selection = 'remove',
                          valuetype = 'glob', 
                          pattern = '}', 
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


set.seed(1)
K <- 20 #50
mylda <- LDA(x = dtm, 
             k = K, 
             method = "Gibbs", # for small data sets we can generally use Gibbs
             control = list(iter = 1000, 
                            seed = 1, 
                            verbose = 100))

# Inspect top 10 from each topics
get_terms(mylda, 10)
get_terms(mylda, 10)[, 1:10] # Only first 10 topics



set.seed(1)
K <- 20 #50
seeded_lda <- LDA(x = dtm, 
             k = K, 
             method = "Gibbs", # for small data sets we can generally use Gibbs
             seedwords = seeds,
             control = list(iter = 1000, 
                            seed = 1, 
                            verbose = 100))


dict <- dictionary(file = "dictionary2.yml")
print(dict)


lda_seed <- textmodel_seededlda(dtm, dict, 
                                residual = 10,
                                batch_size = 0.01, auto_iter = TRUE,
                                verbose = TRUE)

# theta gives topic distribution over documents
knitr::kable(terms(lda_seed))

topic_dist_over_doc <- as.data.frame(lda_seed$theta)
which.max(topic_dist_over_doc$immigration)

topic_dist_over_doc[which.max(topic_dist_over_doc$immigration), ]
random_20T[which.max(topic_dist_over_doc$immigration),]


#########################################################################################
#########################################################################################

speeches_corpus_keywords <- corpus(x = all_speeches_migr,
                                   text_field = "speechContent",
                                   meta = list("lastName", "politicianID", "factionID"),
                                   docid_field = 'id')

# Tokenize & clean from particular types of words
mytokens2 <- tokens(x = speeches_corpus_keywords, 
                   remove_punct = TRUE, 
                   remove_numbers = TRUE, 
                   remove_symbols = TRUE,
                   remove_url = TRUE,
                   padding = FALSE) # empty spaces removed

# Remove german stop words
mytokens2 <- tokens_remove(x = mytokens2,
                          stopwords("de"),
                          padding = FALSE)

mytokens2 <- tokens_select(x = mytokens2, selection = 'remove',
                          valuetype = 'glob', 
                          pattern = '\n', 
                          padding = FALSE)

mytokens2 <- tokens_select(x = mytokens2, selection = 'remove',
                          valuetype = 'glob', 
                          pattern = '{', 
                          padding = FALSE)

mytokens2 <- tokens_select(x = mytokens2, selection = 'remove',
                          valuetype = 'glob', 
                          pattern = '}', 
                          padding = FALSE)


# Make tokens lowercase
mytokens2 <- tokens_tolower(x = mytokens2)

# How does it look now?
mytokens2

# Create document term matrix
dtm2 <- dfm(x = mytokens2)
dtm2

# Exclude words with too low frequency
dtm2 <- dfm_trim(dtm2, min_termfreq = 5)

dim(dtm2)


set.seed(1)
K <- 20 #50
mylda_keywords <- LDA(x = dtm2, 
             k = K, 
             method = "Gibbs", # for small data sets we can generally use Gibbs
             control = list(iter = 1000, 
                            seed = 1, 
                            verbose = 100))

# Inspect top 10 from each topics
get_terms(mylda_keywords, 10)
get_terms(mylda_keywords, 10)[, 1:10] # Only first 10 topics



set.seed(1)
K <- 20 #50
seeded_lda_keywords <- LDA(x = dtm2, 
                  k = K, 
                  method = "Gibbs", # for small data sets we can generally use Gibbs
                  seedwords = seeds,
                  control = list(iter = 1000, 
                                 seed = 1, 
                                 verbose = 100))
