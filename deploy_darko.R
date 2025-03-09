# File: deploy_fixed.R
library(rsconnect)

# Get list of files to include in deployment (CRITICAL!)
all_files <- list.files("DARKO", recursive = TRUE, full.names = FALSE)

# Verify key files are included
key_files <- c(
  "version_date.rds",
  "survival_data.rds", 
  "current_talent.rds",
  "five_man.rds",
  "historical_talent.csv",
  "app.R"
)

missing_files <- key_files[!key_files %in% all_files]
if (length(missing_files) > 0) {
  stop("Missing required files: ", paste(missing_files, collapse=", "))
}

cat("All required files found, proceeding with deployment...\n")

# Deploy the app with EXPLICIT file list
rsconnect::deployApp(
  appDir = "DARKO",
  appName = "DARKO-NBA-Analytics",
  appFiles = all_files,  # This is critical!
  launch.browser = TRUE,
  forceUpdate = TRUE
)