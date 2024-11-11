library(dplyr)
library(quanteda)
library(topicmodels)
library(seededlda)
library(tidyr)
library(xtable)
library(broom)
library(reshape2)
library(lubridate)

speeches <- read.csv("speeches.csv") |>  # 907,644 obs
  filter(positionShort != "Presidium of Parliament", # 522,671 obs
         nchar(speechContent) > 100) #473,255

lda <- readRDS("final_seeded_model_ALL_DATA")
 
topic_dist_over_doc_ALL <- as.data.frame(lda$theta) |> 
   arrange(desc(immigration))
 
word_dist_over_topic <- as.data.frame(lda$phi)
write.csv(word_dist_over_topic, "word_distribution_FINAL.csv")
 
top_terms <- as.data.frame(terms(lda, n = 20))
write.csv(top_terms, "top20_terms_FINALMODEL")
 
topic_dist_over_doc_ALL$id <- rownames(topic_dist_over_doc_ALL)
 
df <- merge(topic_dist_over_doc_ALL, speeches, by = "id")|> 
  arrange(desc(immigration))
 
min(df$immigration) # 7.83024e-05
max(df$immigration) # 0.6679245
mean(df$immigration) # 0.02484453
sd(df$immigration) # 0.04487275
 
# hist(df$immigration,
#      xlab = "Immigration topic",
#      main = "Frequency histogram")

# Threshold for classifying documents as immigration-rich
threshold <- 0.1
 
# Classify documents as immigration-rich based on the threshold
df <- df |> 
  mutate(Immigration_Rich = ifelse(immigration > threshold, TRUE, FALSE)) 
 
summary(df$Immigration_Rich)
 
# write.csv(df, "speeches_topics_merged.csv")
#########################################################################################
#df <- read.csv("speeches_topics_merged.csv")

df_immigr_rich <- df |> 
  filter(Immigration_Rich == TRUE)


factions <- read.csv("factions.csv") |> 
  rename(factionId = id)

df_merged <- merge(df_immigr_rich, factions, by = "factionId") 

df_merged <- df_merged |> 
  mutate(date = as.Date(df_merged$date, "%Y-%m-%d")) |> 
  arrange(date)


CMP_data <- read.csv("CMP_full_data.csv") |> 
  filter(country == 41) |> 
  mutate(edate = as.Date(edate, "%d/%m/%Y")) |> 
  arrange(edate)

CMP_data <- CMP_data |> 
  mutate(rile = (per104 + per201 + per203 + per305 + per401 + per402 + per407 +  per414 + per505 + per601 + per603 + per605 + per606)
         - (per103 + per105 + per106 + per107 + per403 + per404 + per406  + per412 + per413 + per504 + per506 + per701 + per202))



find_next_election_date <- function(speech_date) {
   next_election_date <- CMP_data$edate[CMP_data$edate > speech_date][1]
   return(next_election_date)
}

closest_election_dates <- vector("list", nrow(df_merged))

# Loop through each row of df_merged
for (i in 1:nrow(df_merged)) {
  # Get the speech date from the ith row
  speech_date <- df_merged[i, 42]  #speech date is in column 41
  
  # Find the closest election date using the function
  closest_election_dates[[i]] <- find_next_election_date(speech_date)
}

# Add the closest election dates to df_merged
closest_election_dates <- closest_election_dates |>
  do.call(what = c)

df_merged$closest_election_date <- closest_election_dates

# Compute the difference in days between speech date and closest election date
df_merged <- df_merged |> 
  mutate(days_to_closest_election = as.numeric(difftime(closest_election_date, date, units = "days")),
         abbreviation = ifelse(abbreviation == "not found", "none", abbreviation),
         abbreviation = ifelse(abbreviation == "Fraktionslos", "none", abbreviation),
         abbreviation = ifelse(abbreviation == "FU", "none", abbreviation),
         abbreviation = ifelse(abbreviation == "DA", "none", abbreviation),
         abbreviation = ifelse(abbreviation == "NR", "none", abbreviation),
         abbreviation = ifelse(abbreviation == "FVP", "none", abbreviation),
         abbreviation = ifelse(abbreviation == "DIE LINKE.", "LINKE", abbreviation),
         abbreviation = ifelse(abbreviation == "Z", "DZ", abbreviation)
         )

CMP_data <- CMP_data |> 
  arrange(edate) |> 
  select(edate, partyname, partyabbrev, parfam, rile) |> 
  mutate(abbreviation = trimws(partyabbrev)) |> 
  mutate(abbreviation = ifelse(abbreviation == "", "none", abbreviation),
         abbreviation = ifelse(abbreviation == "Greens/90", "Grüne", abbreviation),
         abbreviation = ifelse(abbreviation == "90/Greens", "Grüne", abbreviation),
         abbreviation = ifelse(abbreviation == "L-PDS", "PDS", abbreviation),
         date = edate) |> 
  select(-edate)


trimws(df_merged$abbreviation)
trimws(CMP_data$partyabbrev)
trimws(CMP_data$abbreviation)
 
unique(df_merged$abbreviation)
unique(CMP_data$abbreviation)

# checking if closest election variable works
speech_and_election_dates <- df_merged[c("date", "closest_election_date", "days_to_closest_election")]
random_100 <- speech_and_election_dates[sample(nrow(speech_and_election_dates), 100), ]




# Merge df_merged with CMP_data based on the closest next election date
merged_df <- df_merged |> 
  #mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  left_join(CMP_data, by = c("date", "abbreviation" ))
 


merged_df <- merge(CMP_data, df_merged, by = c("date", "abbreviation" ))

# checking if ideology variable works
party_and_ideology_var <- merged_df[c("date", "abbreviation", "rile")]
random_100ideology <- party_and_ideology_var[sample(nrow(party_and_ideology_var), 100), ]


# df_merged
# CMP_data

df_merged2 <- merge(df_merged, CMP_data, 
           by.x = c("abbreviation", "closest_election_date"), 
           by.y = c("abbreviation" , "date"),
           all.x = TRUE) # von 21,000 auf 13,308 obs! warum ???

write.csv(df_merged2, "speeches_and_CMP.csv")

df_merged <- read.csv("speeches_and_CMP.csv")


incumbency <- read.csv("incumbency.csv", sep = ";") |> 
  mutate(start_date = as.Date(Regierungsbildung, "%d.%m.%Y"),
         end_date = as.Date(Ende.der.Koalition, "%d.%m.%Y")) |> 
  select(-c(Regierungsbildung, Ende.der.Koalition, Wahlperiode)) 

incumbency <- incumbency[-(29:30),] 
incumbency[28, 3] = as.Date("30.09.2021", "%d.%m.%Y")

# library(reshape2)
# dcast(incumbency, end_date~Parteien)



incumbency <- incumbency |> 
  mutate(parties = strsplit(Parteien, ",\\s*")) |> 
  unnest(parties)

party_dummies <- incumbency |> 
  mutate(in_government = 1) |> 
  pivot_wider(names_from = parties, values_from = in_government, values_fill = 0) |> 
  select(-Parteien)


# get_incumbency <- function(date, partyabbrev) {
#   # Filter party_dummies based on date range
#   party_subset <- party_dummies |> 
#     filter(date >= start_date, date < end_date)
#   
#   # Check if partyabbrev exists as a column in party_dummies
#   if (partyabbrev %in% names(party_subset)) {
#     # Return the value of the corresponding column in party_dummies
#     return(party_subset[[partyabbrev]])
#   } else {
#     # If partyabbrev doesn't exist as a column, return NA
#     return(NA)
#   }
# }
#########################################################################################
adjust_end_dates <- function(df) {
  for (i in 1:(nrow(df) - 1)) {
    # Check if the end date of row n is not one day before the start date of row n+1
    if (df$end_date[i] != df$start_date[i + 1] - 1) {
      # Set the end date of row n to one day before the start date of row n+1
      df$end_date[i] <- df$start_date[i + 1] - 1
    }
  }
  return(df)
}

party_dummies <- adjust_end_dates(party_dummies)

# fixing columns so that they match party abbreviations
party_dummies$'CDU/CSU' <- pmax(party_dummies$CDU, party_dummies$CSU, na.rm = TRUE)
party_dummies$'FVP' <- pmax(party_dummies$'DA (FVP)', party_dummies$'DP (FVP)', na.rm = TRUE)

# Remove the individual CDU and CSU and FVP (DP/DA) columns
party_dummies2 <- party_dummies[, !(names(party_dummies) %in% c("CDU", "CSU"))] 
party_dummies2 <- party_dummies2[, !(names(party_dummies2) %in% c("DA (FVP)", "DP (FVP)"))] 

party_dummies2 <- party_dummies2 |> 
  rename("Grüne" = "90/Greens")

#########################################################################################

df_merged <- df_merged %>%
  mutate(abbreviation = case_when(
    partyname == "The Greens" ~ "Grüne",
    partyname == "Pirates" ~ "Pirates",
    TRUE ~ partyabbrev  # Keep the original partyabbreviation for other cases
  ))


#########################################################################################

get_incumbency <- function(date, abbreviation) {
  # Filter party_dummies based on date range
  party_subset <- party_dummies2 %>%
    filter(date >= start_date, date < end_date)
  
  # Check if partyabbrev exists as a column in party_dummies
  if (abbreviation %in% names(party_subset)) {
    # Return the maximum value across all columns corresponding to the party abbreviation
    return(max(party_subset[[abbreviation]]))
  } else {
    # If partyabbrev doesn't exist as a column, return NA
    return(NA)
  }
}

#########################################################################################
## Test on sample df
df_sample <- df_merged[sample(nrow(df_merged), 100), ]

df_sample <- df_sample |> 
  rowwise() |> 
  mutate(incumbency = get_incumbency(date, abbreviation)) |> 
  ungroup()

#########################################################################################
df_sample <- df_sample |> 
  mutate(incumbency = case_when(
    is.na(incumbency) & !is.na(abbreviation) ~ 0,
    TRUE ~ incumbency  # Keep existing incumbency values unchanged
  ))

#########################################################################################
####### now final data frame!!!

df_merged_with_inc <- df_merged |> 
  rowwise() |> 
  mutate(incumbency = get_incumbency(date, abbreviation)) |> 
  ungroup()

#########################################################################################
df_merged_with_inc <- df_merged_with_inc |> 
  mutate(incumbency = case_when(
    is.na(incumbency) & !is.na(abbreviation) ~ 0,
    TRUE ~ incumbency  # Keep existing incumbency values unchanged
  ))


#rows_with_inf <- df_merged_with_inc |> 
  #filter(incumbency == -Inf)

df_merged_with_inc[17327, "incumbency"] <- 0
df_merged_with_inc[2430, "incumbency"] <- 0
df_merged_with_inc[2434, "incumbency"] <- 0
df_merged_with_inc[2450, "incumbency"] <- 0
df_merged_with_inc[12853, "incumbency"] <- 0

write.csv(df_merged_with_inc, "df_w_independent_vars.csv")


#########################################################################################
#########################################################################################

# calculating time variables: time to next election, time since external event
df2 <- read.csv("df_w_independent_vars.csv")
all_events <- read.csv("all_events.csv", sep = ";")

df2$date <- as.Date(df2$date)
df2$closest_election_date <- as.Date(df2$closest_election_date)
all_events$date <- as.Date(all_events$date, format="%d.%m.%Y")

#######################################
# calculating time_until_election var
df2 <- df2 |> 
  mutate(time_until_election = 1 / (as.numeric(closest_election_date - date) + 1))

# why is the max value so low for the time_until_election variable? (= 0.091)
# Compute the difference in days between date and closest_election_date
df2 <- df2 |> 
  mutate(days_until_election = as.numeric(closest_election_date - date))

# Check the summary of the days until election
summary(df2$days_until_election) # minimum is 10 days, which corresponds to my results (If the election is in 10 days, the result would be 1 / (10 + 1) = 0.0909)

#######################################
# calculating time_since_event var

get_time_since_last_event <- function(doc_date) {
  past_events <- all_events %>% filter(date <= doc_date)
  
  if (nrow(past_events) == 0) {
    return(NA)  # No events before the document date
  }
  
  last_event_date <- max(past_events$date)
  return(1 / (as.numeric(doc_date - last_event_date) + 1))
}

df2 <- df2 |> 
  rowwise() |> 
  mutate(time_since_event = get_time_since_last_event(date))

# View the result
head(df2)
summary(df2$time_since_event)
na_rows <- df2 |>  filter(is.na(time_since_event))
head(na_rows)

# Check how many documents have no event before the document date
df2 <- df2 |> 
  mutate(event_exists = sapply(date, function(doc_date) {
    any(all_events$date <= doc_date)
  }))

# Check how many documents have no event before their date
table(df2$event_exists)

first_event_date <- min(all_events$date, na.rm = TRUE)
first_event_date
# Filter rows where time_since_event is NA and check if the document date is before the first event
na_rows <- df2 |>  
  filter(is.na(time_since_event)) |> 
  mutate(is_before_first_event = date < first_event_date)

# Check the results
summary(na_rows$is_before_first_event) # all NAs before first event!

write.csv(df2, "df_w_independent_vars.csv")

##############################
##############################
## LAST IMPORTANT CHANGES

# move Greens/90 and 90/Greens in same category!!

df3 <- read.csv("df_w_independent_vars.csv")

# Replace both "90/Greens" and "Greens/90" with "Greens" in the partyabbrev column
df3$partyabbrev <- ifelse(df3$partyabbrev %in% c("90/Greens", "Greens/90"), "Greens", df3$partyabbrev)

# View the updated data frame to check the changes
table(df3$partyabbrev)

##########
# move NA and "" in same category!!
df3$partyabbrev[df3$partyabbrev == ""] <- "Unknown"
df3$partyabbrev[is.na(df3$partyabbrev)] <- "Unknown"

df3 <- df3 |> 
  mutate(partyabbrev = ifelse(partyname == "Pirates", "Pirates", partyabbrev))

# Optionally, save the updated data frame back to a CSV file
write.csv(df3, "df_w_independent_vars.csv", row.names = FALSE)


