# Make sure that the rsconnect package is installed.
if (("rsconnect" %in%) == FALSE) {
    install.packages("rsconnect")
}

# Set the account info for deployment.
rsconnect::setAccountInfo(name   = Sys.getenv("shinyapps_name"),
                          token  = Sys.getenv("shinyapps_token"),
                          secret = Sys.getenv("shinyapps_secret"))

# Deploy the application.
rsconnect::deployApp()
