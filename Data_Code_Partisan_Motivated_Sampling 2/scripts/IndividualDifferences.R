# Function to load data and preprocess it
load_and_preprocess <- function(filename, numeric_cols, remove_cols = NULL){
  data <- read.csv(filename, header = T, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  if (!is.null(remove_cols)) data <- data[, -remove_cols]
  data_num <- data[, numeric_cols]
  data_num$diff_score <- data_num$In.Est - data_num$Out.Est
  return(data_num)
}

# Function to generate descriptive statistics
descriptive_stats <- function(data, data_name){
  hist_variables <- names(data)
  print(paste('Descriptives for', data_name))
  for (var in hist_variables){
    plot(hist(data[[var]]), main = paste(var, 'in', data_name))
  }
  print(describe(data$age))
  print(table(data$gender))
  print(paste(data_name, 'Correlation Matrix'))
  print(psych::corr.test(data))
}

# Define data files and respective column configurations
files = list(
  "data/Experiment1aIndDiff.csv" = list(numeric_cols = c(8:13, 15: 28)),
  "data/Experiment1bIndDiff.csv" = list(remove_cols = c(1:2), numeric_cols = c(7:11, 14:23, 25:29)),
  "data/Experiment2IndDiff.csv" = list(remove_cols = c(1:2), numeric_cols = c(6:12, 19:23, 25:29)),
  "data/MetaStudy1Data.csv" = list(numeric_cols = c(5:10, 12:24, 26, 27)),
  "data/MetaStudy2Data.csv" = list(numeric_cols = c(5:10, 12:24, 26, 27)),
  "data/MetaStudy3Data.csv" = list(numeric_cols = c(5:10, 12:24, 26, 27))
)

# Load data, preprocess it and generate descriptive statistics
for (file in names(files)){
  config <- files[[file]]
  data <- load_and_preprocess(file, config$numeric_cols, config$remove_cols)
  data_name <- strsplit(file, split = "/")[[1]][[2]]
  data_name <- strsplit(data_name, split = "\\.")[[1]][[1]]
  descriptive_stats(data, data_name)
}


