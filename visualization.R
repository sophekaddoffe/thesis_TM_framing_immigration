library(dplyr)
library(quanteda)
library(topicmodels)
library(seededlda)
library(tidyr)
library(data.table)
library(ggplot2)
library(knitr) 
library(kableExtra) 
library(DT)
library(reshape2)
library(pals)
library(SnowballC)
library(ldatuning)
library(flextable)
library(TraMineR)



lda <- readRDS("final_seeded_model_ALL_DATA")
terms(lda, 15)

topic_dist_over_doc_ALL <- as.data.frame(lda$theta)

df <- read.csv("speeches_topics_merged.csv") |> 
  select(-c(session, electoralTerm, firstName, lastName, politicianId, speechContent, factionId, documentUrl, positionShort, positionLong, id, X))

# -> then add date???

#### define english names for topics
topic_names <- c("immigration" = "immigration", "diversitaet" = "diversity", "sprache" = "language", "nationale_identitaet" = "national identity", "religion" = "religion",
                 "arbeitsmarkt" = "labor market", "haushalt" = "budget", "gesundheitspolitik" = "health policy", "wohnen" = "housing", "bildung" = "education",
                 "wirtschaft" = "economy", "diskriminierung" = "discrimination", "familie" = "family", "menschenrechte" = "human rights", "rassismus" = "racism", "politische_parteien" = "political parties", "european_union" = "european union",
                 "kriminalitaet" = "crime", "terrorismus" = "terrorism", "umwelt" = "environment", "sport" = "sport", "landwirtschaft" = "agriculture", "verkehr" = "traffic infrastructure", 
                 "gesetzgebung" = "legislation", "verteidigung" = "defense", "anrede" = "address", "other1" = "other 1", "other2" = "other 2", "other3" = "other 3")

# append decade information for aggregation
df$decade <- paste0(substr(df$date, 0, 3), "0")


# get mean topic proportions per decade
topic_proportion_per_decade <- aggregate(topic_dist_over_doc_ALL, by = list(decade = df$decade), mean)


# set topic names to aggregated columns
#colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames


# reshape data frame
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")
# plot topic proportions per decade as bar plot

library(viridis)  # Load the viridis package for color palettes

# Generate a custom color palette with more distinct colors
n_colors <- 29  # Number of distinct colors
topic_colors <- colorRampPalette(colors = rainbow(n_colors))(n_colors)

# Plot topic proportions per decade as bar plot
ggplot(vizDataFrame, aes(x = decade, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + 
  ylab("Proportion") + 
  scale_fill_manual(values = topic_colors, name = "Topics", labels = topic_names) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Define a custom color palette with 29 distinct colors
custom_palette <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#800000", "#008000", "#000080", "#808000", "#800080", "#7F7F7F", "#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FFFFFF")

# Plot topic proportions per decade as bar plot
ggplot(vizDataFrame, aes(x = decade, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + 
  ylab("Proportion") + 
  scale_fill_manual(values = custom_palette, name = "Topics", labels = topic_names) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#########################################################################################
df_immigr_rich <- df |> 
  filter(Immigration_Rich == TRUE)

theta_immigr_rich <- df |> 
  filter(Immigration_Rich == TRUE) |> 
  select(-c(Immigration_Rich, date, decade))

# append decade information for aggregation
df_immigr_rich$decade <- paste0(substr(df_immigr_rich$date, 0, 3), "0")


# get mean topic proportions per decade
topic_proportion_per_decade2 <- aggregate(theta_immigr_rich, by = list(decade = df_immigr_rich$decade), mean)


# set topic names to aggregated columns
#colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames


# reshape data frame
vizDataFrame2 <- melt(topic_proportion_per_decade2, id.vars = "decade")

# Plot topic proportions per decade as bar plot
ggplot(vizDataFrame2, aes(x = decade, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + 
  ylab("Proportion") + 
  scale_fill_manual(values = custom_palette, name = "Topics", labels = topic_names) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#########################################################################################


#########################################################################################
#########################################################################################
# Add a year column to the dataset
df$year <- format(as.Date(df$date), "%Y")
#######################################

df_immigr_rich$immigration_proportion <- topic_dist_over_doc_ALL$topic_immigration[df$Immigration_Rich == TRUE]

# Calculate the annual average proportion of immigration-related topics
annual_avg_salience <- aggregate(df_immigr_rich$immigration, by = list(year = df_immigr_rich$year), mean)

# Rename columns for clarity
colnames(annual_avg_salience) <- c("year", "average_salience")


# Calculate the start of each decade
decade_breaks <- seq(floor(min(as.numeric(annual_avg_salience$year))/10)*10, 
                     ceiling(max(as.numeric(annual_avg_salience$year))/10)*10, 
                     by = 10)

# Plot the annual average salience with decade breaks on the x-axis
ggplot(annual_avg_salience, aes(x = as.numeric(year), y = average_salience)) + 
  geom_line(color = "blue", size = 1) + 
  geom_point(size = 2) + 
  labs(x = "Year", y = "Annual Average Proportion") + 
  theme_minimal() + 
  scale_x_continuous(breaks = decade_breaks)  # Set x-axis breaks to decades
#########################################################################################
# same plot for all speeches

#df$immigration_proportion <- topic_dist_over_doc_ALL$topic_immigration

# Calculate the annual average proportion of immigration-related topics
annual_avg_salience_all <- aggregate(df$immigration, by = list(year = df$year), mean)

# Rename columns for clarity
colnames(annual_avg_salience_all) <- c("year", "average_salience")


# Calculate the start of each decade
decade_breaks <- seq(floor(min(as.numeric(annual_avg_salience$year))/10)*10, 
                     ceiling(max(as.numeric(annual_avg_salience$year))/10)*10, 
                     by = 10)

# Plot the annual average salience with decade breaks on the x-axis
ggplot(annual_avg_salience_all, aes(x = as.numeric(year), y = average_salience)) + 
  geom_line(color = "blue", size = 1) + 
  geom_point(size = 2) + 
  labs(x = "Year", y = "Annual Average Proportion") + 
  theme_minimal() + 
  scale_x_continuous(breaks = decade_breaks)  # Set x-axis breaks to decades




#########################################################################################
# plotting 5 frames over decades

# Add a year column to the merged dataset
df_merged_with_inc$year <- format(as.Date(df_merged_with_inc$date), "%Y")

# Calculate the average proportions per year for each frame
frame_columns <- c("culture_association", "economy_association", "human_rights_association", "politics_association", "security_association")
annual_avg_frames <- aggregate(df_merged_with_inc[, frame_columns], by = list(year = df_merged_with_inc$year), mean)

# Reshape the data for easier plotting (long format)
vizDataFrames <- melt(annual_avg_frames, id.vars = "year")

# Plot the five frames as line graphs
ggplot(vizDataFrames, aes(x = as.numeric(year), y = value, color = variable, group = variable)) + 
  geom_line(size = 1.5) + 
  geom_point(size = 2) + 
  labs(x = "Year", y = "Average Proportion") + 
  scale_color_manual(values = custom_palette, name = "Frames") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#########################################################################################

# other plots


interpretative_frames <- list(
  culture = c("diversitaet", "sprache", "nationale_identitaet", "religion"),
  economy = c("arbeitsmarkt", "haushalt", "gesundheitspolitik", "wohnen", "bildung", "wirtschaft"),
  human_rights = c("diskriminierung", "menschenrechte", "familie", "rassismus"),
  politics = c("politische_parteien", "european_union"),
  security = c("terrorismus", "kriminalitaet", "verteidigung")
)

# Creating frame associations
for (frame in names(interpretative_frames)) {
  topics_in_frame <- interpretative_frames[[frame]]
  probabilities <- rowSums(df_merged_with_inc[, topics_in_frame], na.rm = TRUE)
  df_merged_with_inc[[paste0(frame, "_association")]] <- probabilities
}

# Select only the frame association columns
frame_columns <- sapply(names(interpretative_frames), function(x) paste0(x, "_association"))
frame_associations <- df_merged_with_inc[, frame_columns]

# Calculate the correlation matrix
correlation_matrix <- cor(frame_associations, use = "pairwise.complete.obs")

library(reshape2)

# Melt the correlation matrix
melted_correlation <- melt(correlation_matrix)

# Remove "_association" from the labels
melted_correlation$Var1 <- gsub("_association", "", melted_correlation$Var1)
melted_correlation$Var2 <- gsub("_association", "", melted_correlation$Var2)

# Plot the heatmap
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  labs(x = "Frames", y = "Frames", title = "Heatmap of Frames") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########################################################################################
# faceted plots
df_independent_vars <- read.csv("df_w_independent_vars.csv")

frame_columns <- c("culture_association", "economy_association", 
                   "human_rights_association", "politics_association", 
                   "security_association") 

mean_frame_by_party <- df_independent_vars |> 
  group_by(partyabbrev) |> 
  summarise(across(all_of(frame_columns), mean, na.rm = TRUE))


# Reshape to long format
vizFrameDataFrame <- melt(mean_frame_by_party, id.vars = "partyabbrev")

# Rename columns for clarity
colnames(vizFrameDataFrame) <- c("partyabbrev", "frame", "mean_proportion")

# Remove "_association" from frame labels
vizFrameDataFrame$frame <- gsub("_association", "", vizFrameDataFrame$frame)

# move "" and NA in category "Unknown"
vizFrameDataFrame$partyabbrev[vizFrameDataFrame$partyabbrev == ""] <- "Unknown"
vizFrameDataFrame$partyabbrev[is.na(vizFrameDataFrame$partyabbrev)] <- "Unknown"

# Create faceted bar plot by frame and party
ggplot(vizFrameDataFrame, aes(x = partyabbrev, y = mean_proportion, fill = frame)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Party", y = "Average Frame Proportion") + # , title = "Frame Proportions by Party Affiliation"
  facet_wrap(~ frame) +  # Facet by frame
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_fill_brewer(palette = "Set3")  # Choose a color palette
