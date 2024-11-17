library(Microsoft365R)
od <- Microsoft365R::get_personal_onedrive()
od$list_shared_items()[[1]]$download(dest = "darko/data", overwrite = TRUE)