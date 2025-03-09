# Create www folder if it doesn't exist (for images)
if (!dir.exists("DARKO/www")) {
  dir.create("DARKO/www")
}

# Copy any image files to the www folder if they exist
img_files <- list.files("DARKO", pattern = "\\.(jpg|jpeg|png|gif)$", full.names = TRUE)
for (img in img_files) {
  file.copy(img, file.path("DARKO/www", basename(img)), overwrite = TRUE)
}

# Move RDS files to the root of DARKO folder where app.R can find them
cat("Moving data files to the correct location...\n")
data_files <- c(
  "version_date.rds",
  "survival_data.rds",
  "current_talent.rds", 
  "five_man.rds",
  "historical_talent.csv"
)

for (file in data_files) {
  if (file.exists(file.path("DARKO", file))) {
    cat("✓ File", file, "is already in the correct location\n")
  } else {
    cat("✗ File", file, "is missing!\n")
  }
}

cat("\nApp structure has been fixed!\n")