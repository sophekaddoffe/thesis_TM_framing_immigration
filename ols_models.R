library(dplyr)
library(quanteda)
library(topicmodels)
library(seededlda)
library(tidyr)
library(xtable)
library(broom)
library(reshape2)
library(plm)

#########################################################################################


df_merged_with_inc <- read.csv("df_w_independent_vars.csv")

interpretative_frames <- list(
  culture = c("diversitaet", "sprache", "nationale_identitaet", "religion"),
  economy = c("arbeitsmarkt", "haushalt", "gesundheitspolitik", "wohnen", "bildung", "wirtschaft"),
  human_rights = c("diskriminierung", "menschenrechte", "familie", "rassismus"),
  politics = c("politische_parteien", "european_union"),
  security = c("terrorismus", "kriminalitaet", "verteidigung")
)



# Creating continuous variables for each interpretative frame
for (frame in names(interpretative_frames)) {
  topics_in_frame <- interpretative_frames[[frame]]
  probabilities <- rowSums(df_merged_with_inc[, topics_in_frame])
  df_merged_with_inc[[paste0(frame, "_association")]] <- probabilities
}

# get summary statistics for interpretative frame variables
interpretative_frame_vars <- grep("_association$", names(df_merged_with_inc), value = TRUE)  
frame_summary <- summary(df_merged_with_inc[, interpretative_frame_vars])

# View summary statistics
print(frame_summary)
#########################################################################################


# Assuming df_merged_with_inc contains the relevant variables

# Model 1: Culture frame
model_culture <- lm(culture_association ~ incumbency + rile, data = df_merged_with_inc) # + time_until_election + time_since_event

# Model 2: Economy frame
model_economy <- lm(economy_association ~ incumbency + rile, data = df_merged_with_inc)

# Model 3: Human Rights frame
model_human_rights <- lm(human_rights_association ~ incumbency + rile, data = df_merged_with_inc)

# Model 4: Politics frame
model_politics <- lm(politics_association ~ incumbency + rile, data = df_merged_with_inc)

# Model 5: Security frame
model_security <- lm(security_association ~ incumbency + rile, data = df_merged_with_inc)



# Fit the models
models <- list(
  culture = model_culture,
  economy = model_economy,
  human_rights = model_human_rights,
  politics = model_politics,
  security = model_security
)

# Tidy up the model results and extract coefficients, standard errors, p-values, and R-squared
model_results <- lapply(models, function(model) {
  tidy(model)
})

# Combine model results into a single data frame
results_df <- do.call(rbind, model_results)

# Add model names as a column
results_df$model <- rep(names(models), sapply(model_results, nrow))

print(results_df)



# Convert the results dataframe to xtable format
xtable_results <- xtable(results_df)

print(xtable_results, type = "latex")

#########################################################################################

modelsummary::modelsummary(models, stars = TRUE, fmt = 5, output = "latex")

#########################################################################################

#model_test <- lm(security_association ~ time_until_election + time_since_event, data = df_merged_with_inc)
#modelsummary::modelsummary(model_test, stars = TRUE, fmt = 5, output = "latex")

#########################################################################################
#########################################################################################
#########################################################################################

############################## FIXED PARTY EFFECTS ######################################
df_merged_with_inc$partyabbrev[is.na(df_merged_with_inc$partyabbrev) | df_merged_with_inc$partyabbrev == ""] <- "none"


df_merged_with_inc_clean <- df_merged_with_inc |> 
  filter(!is.na(time_until_election) & !is.na(time_since_event) & !is.na(partyabbrev))



fixed_culture <- plm(
  culture_association ~ time_until_election + time_since_event, 
  data = df_merged_with_inc_clean, 
  index = "partyabbrev", 
  model = "within"
)

#summary(fixed_effect_model)

fixed_economy <- plm(
  economy_association ~ time_until_election + time_since_event, 
  data = df_merged_with_inc_clean, 
  index = "partyabbrev", 
  model = "within"
)

fixed_hr <- plm(
  human_rights_association ~ time_until_election + time_since_event, 
  data = df_merged_with_inc_clean, 
  index = "partyabbrev", 
  model = "within"
)

fixed_politics <- plm(
  politics_association ~ time_until_election + time_since_event, 
  data = df_merged_with_inc_clean, 
  index = "partyabbrev", 
  model = "within"
)

fixed_security <- plm(
  security_association ~ time_until_election + time_since_event, 
  data = df_merged_with_inc_clean, 
  index = "partyabbrev", 
  model = "within"
)

# Fit the models
fixed_models <- list(
  culture = fixed_culture,
  economy = fixed_economy,
  human_rights = fixed_hr,
  politics = fixed_politics,
  security = fixed_security
)


modelsummary::modelsummary(fixed_models, stars = TRUE, fmt = 5, output = "latex")
#########################################################################################

#### OLS WITH PARTY DUMMIES

# Ensure `partyabbrev` is a factor so that R treats it as a categorical variable, creating dummy variables automatically
df_merged_with_inc_clean$partyabbrev <- factor(df_merged_with_inc_clean$partyabbrev)

# Fit OLS models with party dummies for each framing variable
ols_culture <- lm(
  culture_association ~ time_until_election + time_since_event + partyabbrev, 
  data = df_merged_with_inc_clean
)

ols_economy <- lm(
  economy_association ~ time_until_election + time_since_event + partyabbrev, 
  data = df_merged_with_inc_clean
)

ols_hr <- lm(
  human_rights_association ~ time_until_election + time_since_event + partyabbrev, 
  data = df_merged_with_inc_clean
)

ols_politics <- lm(
  politics_association ~ time_until_election + time_since_event + partyabbrev, 
  data = df_merged_with_inc_clean
)

ols_security <- lm(
  security_association ~ time_until_election + time_since_event + partyabbrev, 
  data = df_merged_with_inc_clean
)

# Combine models into a list for easier summary and export
ols_models <- list(
  culture = ols_culture,
  economy = ols_economy,
  human_rights = ols_hr,
  politics = ols_politics,
  security = ols_security
)

# Output the models summary in LaTeX format
modelsummary::modelsummary(ols_models, stars = TRUE, fmt = 5, output = "latex")


