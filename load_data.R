# Load readr library
library(readr)

#### Loading the data from plot_data folder using readr ####
cat("Loading data...\n")

# List all files in the plot_data directory
files <- list.files("plot_data", pattern = "\\.csv$", full.names = TRUE)

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = length(files), style = 3)

# Read all CSV files
for (i in seq_along(files)) {
  file <- files[i]
  
  # Read quietly
  data <- readr::read_csv(file, show_col_types = FALSE, progress = FALSE)
  
  # Convert to data frame
  data <- as.data.frame(data)
  
  # Set row names and remove the first column
  rownames(data) <- data$...1
  data <- data[ , -1]
  
  # Assign to variable
  assign(gsub("plot_data/", "", gsub(".csv", "", file)), data, envir = .GlobalEnv)
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}

# Close progress bar
close(pb)

cat("\nData loading complete.\n")

# Remove redundant variables
rm(file, data, files, i, pb)

#### Define data set names ####
dataset_names <- c(
  "PDC000125 - normal - Uterine" = "tmt.PDC000125",
  "PDC000125 - Tumor - Uterine" = "tmt.PDC000125_tumor",
  "PDC000127 - normal - Renal" = "tmt.PDC000127",
  "PDC000127 - Tumor - Renal" = "tmt.PDC000127_tumor",
  "PDC000153 - normal - Lung" = "tmt.PDC000153",
  "PDC000153 - Tumor - Lung" = "tmt.PDC000153_tumor",
  "PDC000198 - normal - Hepatic" = "tmt.PDC000198",
  "PDC000198 - Tumor - Hepatic" = "tmt.PDC000198_tumor",
  "PDC000219 - normal - Lung" = "tmt.PDC000219",
  "PDC000219 - Tumor - Lung" = "tmt.PDC000219_tumor",
  "PDC000234 - normal - Lung" = "tmt.PDC000234",
  "PDC000234 - Tumor - Lung" = "tmt.PDC000234_tumor",
  "PDC000234 - Peptides (Only H3.1 + H3.3)" = "tmt.PDC000234_peptides",
  "PDC000270 - normal - Pancreas" = "tmt.PDC000270",
  "PDC000270 - Tumor - Pancreas" = "tmt.PDC000270_tumor"
)
