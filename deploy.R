# Install the rsconnect package if it's not already installed
install.packages("rsconnect")

# Load the rsconnect package
library(rsconnect)

# Set your shinyapps.io account information
rsconnect::setAccountInfo(name='tfmkfs',
                          token='my_token',
                          secret='my_key')

# Set the working directory to where your app.R is located
setwd("C:/Users/KSerrano/Documents/GitHub/TFM")

# Deploy the application
rsconnect::deployApp("C:/Users/KSerrano/Documents/GitHub/TFM", appName = 'tfmkfs')
