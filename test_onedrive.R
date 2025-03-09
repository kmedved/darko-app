library(Microsoft365R)

# Print initial information
cat("Testing OneDrive connection...\n")

# Try explicit authentication
try({
  cat("Attempting authentication with device code...\n")
  od <- Microsoft365R::get_personal_onedrive(auth_type = "device_code")
  cat("Authentication successful!\n")
  
  # List contents of OneDrive root
  cat("Files in OneDrive root:\n")
  root_files <- od$list_files()
  print(head(root_files))
  
  # List shared items
  cat("\nShared items:\n")
  shared <- od$list_shared_items()
  print(shared)
  
  # If shared items exist, attempt to access the first one
  if(length(shared) > 0) {
    cat("\nAttempting to access first shared item...\n")
    first_item <- shared[[1]]
    print(first_item$properties)
    
    # List files in the shared item if it's a folder
    if(!is.null(first_item$list_files)) {
      cat("\nFiles in shared item:\n")
      shared_files <- first_item$list_files()
      print(head(shared_files))
    }
  } else {
    cat("\nNo shared items found!\n")
  }
})