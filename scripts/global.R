require(shiny)
require(tidyverse)

# ---- GLOBAL SECTION ----
data_path <- "../"

# Helper functions
extract_participant <- function(filename) {
  str_match(basename(filename), "-(P\\d+)-")[, 2]
}
extract_group <- function(filename) {
  str_match(basename(filename), "-(G0[0-9A-Za-z]+)-")[, 2]
}
# File list
sd2_files <- list.files(path = data_path, pattern = "\\.sd2$", full.names = TRUE)

# Only process if files exist
if (length(sd2_files) > 0) {
  files_df <- tibble(
    path = sd2_files,
    filename = basename(sd2_files),
    group = extract_group(sd2_files),
    participant = extract_participant(sd2_files)
  )
  
  all_data <- map_dfr(files_df$path, function(file) {
    df <- read_csv(file, show_col_types = FALSE)
    
    if (nrow(df) == 0) {
      return(NULL)  # skip files with only header
    }
    
    df %>%
      mutate(
        TaskType = as.character(TaskType),
        SRC = as.character(SRC),
        File = basename(file),
        Group = extract_group(file),
        Participant = extract_participant(file)
      )
  })
  
  file_tp_means <- all_data %>%
    group_by(Group, File, Participant) %>%
    summarise(mean_tp = mean(`TP(bps)`, na.rm = TRUE), .groups = "drop")
} else {
  files_df <- tibble(path = character(), filename = character(), group = character(), participant = character())
  all_data <- tibble()
  file_tp_means <- tibble()
}


# Load *.sd1 files
sd1_files <- list.files(path = data_path, pattern = "\\.sd1$", full.names = TRUE)

sd1_data <- map_dfr(sd1_files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  
  if (nrow(df) == 0) return(NULL)
  
  df %>%
    mutate(
      TaskType = as.character(TaskType),
      File = basename(file),
      Group = extract_group(file),
      Participant = extract_participant(file)
    )
})

