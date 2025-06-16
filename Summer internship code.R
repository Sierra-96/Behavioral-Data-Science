###Activate libraries###

library(httr)
library(jsonlite)
library(dplyr)
library(corrplot)
library(Hmisc)

###Get the data###

Personality <- read.csv("https://raw.githubusercontent.com/karwester/behavioural-finance-task/refs/heads/main/personality.csv")
    #Open data, just read the soruce as csv

supabase_url <- "https://pvgaaikztozwlfhyrqlo.supabase.co"
api_key <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InB2Z2FhaWt6dG96d2xmaHlycWxvIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NDc4NDE2MjUsImV4cCI6MjA2MzQxNzYyNX0.iAqMXnJ_sJuBMtA6FPNCRcYnKw95YkJvY3OhCIZ77vI"

    # Endpoint
url <- paste0(supabase_url, "/rest/v1/assets?select=*")

      # Ask for data using the API key
response <- GET(url,  add_headers(apikey = api_key,Authorization = paste("Bearer", api_key)))

      # Check the data response, and status
if (status_code(response) == 200) {
  Asset_data <- content(response, as = "text", encoding = "UTF-8")
  Asset_data <- fromJSON(Asset_data)
  print(head(Asset_data))         # Show heads and first row of data
} else {
  print(paste("Error:", status_code(response)))
  print(content(response, as = "text"))
}

      #Rename id variable to match the personality x_id variable
Asset_data<-rename(Asset_data,X_id = `_id`)
      
###Combine the two databases###

Combined <- merge(Asset_data,Personality,by="X_id")

###Identify the highest GBP asset value and it's risk tolerance

HAV <- Combined %>%
  filter(asset_currency == "GBP") %>%
  filter(asset_value == max(asset_value))

print(HAV)


###Descriptive statistics###

    #Frecuency tables for the cualitative variables
        #Absolute frecuencies
AAFT <- table(Combined$asset_allocation)
ACFT<-table(Combined$asset_currency)

  print(AAFT)
  print(ACFT)
        #Relative frecuencies
prop.table(AAFT)
prop.table(ACFT)

    #measures of central tendency and dispersion 

Means <- c(mean(Combined$asset_value),mean(Combined$confidence),mean(Combined$risk_tolerance),
           mean(Combined$composure),mean(Combined$impulsivity),mean(Combined$impact_desire))

Medians <- c(median(Combined$asset_value),median(Combined$confidence),median(Combined$risk_tolerance),
             median(Combined$composure),median(Combined$impulsivity),median(Combined$impact_desire))

Standar.D <- c(sd(Combined$asset_value),sd(Combined$confidence),sd(Combined$risk_tolerance),
               sd(Combined$composure),sd(Combined$impulsivity),sd(Combined$impact_desire))

InterQ <- c(IQR(Combined$asset_value),IQR(Combined$confidence),IQR(Combined$risk_tolerance),
            IQR(Combined$composure),IQR(Combined$impulsivity),IQR(Combined$impact_desire))

        #Set the MCT and dispersion in one object
Descriptives <- data.frame(Means,Medians,Standar.D,InterQ)

  print(Descriptives)
        #Normality test for correlation analysis

Normality <- c(shapiro.test(Combined$asset_value),shapiro.test(Combined$confidence),shapiro.test(Combined$risk_tolerance),
               shapiro.test(Combined$composure),shapiro.test(Combined$impulsivity),shapiro.test(Combined$impact_desire))
    print(Normality)
###Correlation analysis on cuantitative variables###

df.correl <- data.frame(Combined$asset_value,Combined$confidence,Combined$risk_tolerance,
                        Combined$composure,Combined$impulsivity,Combined$impact_desire)

        #Pearson correlation as the variables turn out to have a normal distribution
res_Correlation <- rcorr(as.matrix(df.correl),type="pearson") 
Correlation <- res_Correlation$r
p_correlation <- res_Correlation$P

  print("Pearson correlation matrix:")
  print(Correlation)
  print("p values matrix:")
  print(p_correlation)

###Correlation plots###
      #Correlation matrix plot
corrplot(Correlation, method = "number", type = "upper", 
         tl.col = "black",tl.cex = 0.8,number.cex = 0.5, title = "correlation matrix", mar = c(0,0,4,0))
      #Correlation scatter plots
pairs(df.correl)

