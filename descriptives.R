library(dplyr) 
library(kableExtra)

############

# Count the number of speeches per year
speeches_per_year <- speeches |> 
  mutate(year = as.integer(format(as.Date(date), "%Y"))) |>   # Extract year from date
  group_by(year) |>                                         # Group by year
  summarize(count = n())                                      # Count speeches per year


print(speeches_per_year)

mean_speeches <- speeches_per_year %>%
  summarize(
    mean_overall = mean(count),                       # Mean speeches overall
    mean_before_1990 = mean(count[year < 1990]),      # Mean speeches before 1990
    mean_after_1990 = mean(count[year >= 1990])       # Mean speeches after 1990
  )


print(mean_speeches)

###########


df_merged_with_inc <- read.csv("df_w_independent_vars.csv")

summary(df_merged_with_inc$rile)
sd(df_merged_with_inc$rile, na.rm = TRUE)

which.min(df_merged_with_inc$rile) # 9762
df_merged_with_inc[9762,] # die linke in 2015

which.max(df_merged_with_inc$rile) # 530
df_merged_with_inc[530,] # CDU/CSU in 1954

summary(df_merged_with_inc$incumbency)

summary(df_merged_with_inc$culture_association)
sd(df_merged_with_inc$culture_association)

summary(df_merged_with_inc$politics_association)
sd(df_merged_with_inc$politics_association)

summary(df_merged_with_inc$security_association)
sd(df_merged_with_inc$security_association)

summary(df_merged_with_inc$human_rights_association)
sd(df_merged_with_inc$human_rights_association)

summary(df_merged_with_inc$economy_association)
sd(df_merged_with_inc$economy_association)
###########################################################


mean(df_merged_with_inc$immigration)
sd(df_merged_with_inc$immigration)
length(df_merged_with_inc$immigration)

###########################################################


speech_counts <- df_merged_with_inc |> 
  group_by(partyabbrev) |>   # Replace 'party' with the actual name of your party column
  summarise(num_speeches = n())

print(speech_counts)
