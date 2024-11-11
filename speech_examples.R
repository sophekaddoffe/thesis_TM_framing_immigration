# Load dplyr for easier data manipulation
library(dplyr)

# Function to get top speeches for a given frame
get_top_speeches <- function(df, frame_column, n = 5) {
  df |> 
    arrange(desc(!!sym(frame_column))) |>  # Sort by frame association score in descending order
    slice_head(n = n) |>  # Select top n speeches
    select(firstName, lastName, partyabbrev, date, speechContent, !!sym(frame_column)) # Select the speech content and frame score for display
}

# Example usage for each frame
# Repeat for each frame by specifying the *_association column names
top_culture_speeches <- get_top_speeches(df_merged_with_inc, "culture_association", n = 5)
top_economy_speeches <- get_top_speeches(df_merged_with_inc, "economy_association", n = 5)
top_human_rights_speeches <- get_top_speeches(df_merged_with_inc, "human_rights_association", n = 5)
top_politics_speeches <- get_top_speeches(df_merged_with_inc, "politics_association", n = 5)
top_security_speeches <- get_top_speeches(df_merged_with_inc, "security_association", n = 5)


