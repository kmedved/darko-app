library(rsconnect)

rsconnect::setAccountInfo(
  name = "apanalytics",
  token = Sys.getenv("SHINY_TOKEN"),
  secret = Sys.getenv("SHINY_SECRET")
)

rsconnect::deployApp(appDir = "darko/DARKO",
                    appId = 1586996,
                    launch.browser = FALSE,
                    forceUpdate = TRUE)
