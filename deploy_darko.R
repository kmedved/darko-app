# File: deploy_resilient.R

# First, modify load_data.R to handle missing files
source("fix_load_data.R")

# Now deploy with all files
library(rsconnect)

# List all files in the DARKO directory
app_files <- list.files("DARKO", recursive = TRUE, full.names = TRUE)

# Print the files being deployed
cat("Files to be deployed:\n")
print(basename(app_files))

rsconnect::deployApp(
  appDir = "DARKO",
  appName = "DARKO",
  launch.browser = TRUE,
  forceUpdate = TRUE
)