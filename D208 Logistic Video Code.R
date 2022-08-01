#Load the data set "churn_clean.csv"
churn_clean <- read.csv('churn_clean.csv')


#Install package "tidyverse" and load library accordingly
install.packages("tidyverse")
library(tidyverse)
library(magrittr)
library(dplyr)

#Packages need for Confusion Matrix 
install.packages("ISLR")
install.packages("caret")
install.packages("InformationValue")
library(caret)
library(InformationValue)
library(ISLR)

#Check for NULL or missing values
colSums(is.na(churn_clean))


#Rename columns (Item1-8) to respective names for more clarity
churn_updated <- churn_clean %>% rename(Timely_response = Item1, Timely_fixes = Item2, Timely_replacements = Item3, Reliability = Item4, Options = Item5, Respectful_response = Item6, Courteous_exchange = Item7, Active_listening = Item8)       


#Change categorical variables to integer
churn_new <- churn_clean$Churn          # Replicating vector
churn_new <- as.character(churn_new)    # Converting factor to character
churn_new[churn_new == "Yes"] <- 1      # Replacing Yes by 1
churn_new[churn_new == "No"] <- 0       # Replacing No by 0
churn_new <- as.integer(churn_new)      # Converting character to numeric

gender_new <- churn_clean$Gender          # Replicating vector
gender_new <- as.character(gender_new)    # Converting factor to character
gender_new[gender_new == "Male"] <- 2      # Replacing Yes by 2
gender_new[gender_new == "Female"] <- 1       # Replacing No by 1
gender_new[gender_new == "Nonbinary"] <- 0      # Replacing No by 0
gender_new <- as.integer(gender_new)      # Converting character to numeric
gender_new  

techie_new <- churn_clean$Techie         # Replicating vector
techie_new <- as.character(techie_new)    # Converting factor to character
techie_new[techie_new == "Yes"] <- 1      # Replacing Yes by 1
techie_new[techie_new == "No"] <- 0      # Replacing No by 0
techie_new <- as.integer(techie_new)      # Converting character to numeric
techie_new  

paperless_billing_new <- churn_clean$PaperlessBilling         # Replicating vector
paperless_billing_new <- as.character(paperless_billing_new)    # Converting factor to character
paperless_billing_new[paperless_billing_new == "Yes"] <- 1      # Replacing Yes by 1
paperless_billing_new[paperless_billing_new == "No"] <- 0      # Replacing No by 0
paperless_billing_new <- as.integer(paperless_billing_new)      # Converting character to numeric
paperless_billing_new  

streaming_movies_new <- churn_clean$StreamingMovies       # Replicating vector
streaming_movies_new <- as.character(streaming_movies_new)    # Converting factor to character
streaming_movies_new[streaming_movies_new == "Yes"] <- 1      # Replacing Yes by 1
streaming_movies_new[streaming_movies_new == "No"] <- 0      # Replacing No by 0
streaming_movies_new <- as.integer(streaming_movies_new)      # Converting character to numeric
streaming_movies_new 

streaming_tv_new <- churn_clean$StreamingTV      # Replicating vector
streaming_tv_new <- as.character(streaming_tv_new)    # Converting factor to character
streaming_tv_new[streaming_tv_new == "Yes"] <- 1      # Replacing Yes by 1
streaming_tv_new[streaming_tv_new == "No"] <- 0      # Replacing No by 0
streaming_tv_new <- as.integer(streaming_tv_new)      # Converting character to numeric
streaming_tv_new 

techsupport_new <- churn_clean$TechSupport      # Replicating vector
techsupport_new <- as.character(techsupport_new)    # Converting factor to character
techsupport_new[techsupport_new == "Yes"] <- 1      # Replacing Yes by 1
techsupport_new[techsupport_new == "No"] <- 0      # Replacing No by 0
techsupport_new <- as.integer(techsupport_new)      # Converting character to numeric
techsupport_new 

device_protection_new <- churn_clean$DeviceProtection      # Replicating vector
device_protection_new <- as.character(device_protection_new)    # Converting factor to character
device_protection_new[device_protection_new == "Yes"] <- 1      # Replacing Yes by 1
device_protection_new[device_protection_new == "No"] <- 0      # Replacing No by 0
device_protection_new <- as.integer(device_protection_new)      # Converting character to numeric
device_protection_new 

online_backup_new <- churn_clean$OnlineBackup    # Replicating vector
online_backup_new <- as.character(online_backup_new)    # Converting factor to character
online_backup_new[online_backup_new == "Yes"] <- 1      # Replacing Yes by 1
online_backup_new[online_backup_new == "No"] <- 0      # Replacing No by 0
online_backup_new <- as.integer(online_backup_new)      # Converting character to numeric
online_backup_new 

online_security_new <- churn_clean$OnlineSecurity   # Replicating vector
online_security_new <- as.character(online_security_new)    # Converting factor to character
online_security_new[online_security_new == "Yes"] <- 1      # Replacing Yes by 1
online_security_new[online_security_new == "No"] <- 0      # Replacing No by 0
online_security_new <- as.integer(online_security_new)      # Converting character to numeric
online_security_new 

multiple_new <- churn_clean$Multiple  # Replicating vector
multiple_new <- as.character(multiple_new)    # Converting factor to character
multiple_new[multiple_new == "Yes"] <- 1      # Replacing Yes by 1
multiple_new[multiple_new == "No"] <- 0      # Replacing No by 0
multiple_new <- as.integer(multiple_new)      # Converting character to numeric
multiple_new 

phone_new <- churn_clean$Phone  # Replicating vector
phone_new <- as.character(phone_new)    # Converting factor to character
phone_new[phone_new == "Yes"] <- 1      # Replacing Yes by 1
phone_new[phone_new == "No"] <- 0      # Replacing No by 0
phone_new <- as.integer(phone_new)      # Converting character to numeric
phone_new 

tablet_new <- churn_clean$Tablet # Replicating vector
tablet_new <- as.character(tablet_new)    # Converting factor to character
tablet_new[tablet_new == "Yes"] <- 1      # Replacing Yes by 1
tablet_new[tablet_new == "No"] <- 0      # Replacing No by 0
tablet_new <- as.integer(tablet_new)      # Converting character to numeric
tablet_new 

port_modem_new <- churn_clean$Port_modem # Replicating vector
port_modem_new <- as.character(port_modem_new)    # Converting factor to character
port_modem_new[port_modem_new == "Yes"] <- 1      # Replacing Yes by 1
port_modem_new[port_modem_new == "No"] <- 0      # Replacing No by 0
port_modem_new <- as.integer(port_modem_new)      # Converting character to numeric
port_modem_new 

contract_new <- churn_clean$Contract          # Replicating vector
contract_new <- as.character(contract_new)    # Converting factor to character
contract_new[contract_new == "Two Year"] <- 2      # Replacing Yes by 2
contract_new[contract_new == "One year"] <- 1       # Replacing No by 1
contract_new[contract_new == "Month-to-month"] <- 0      # Replacing No by 0
contract_new <- as.integer(contract_new)      # Converting character to numeric
contract_new  

internet_new <- churn_clean$InternetService        # Replicating vector
internet_new <- as.character(internet_new)    # Converting factor to character
internet_new[internet_new == "Fiber Optic"] <- 2      # Replacing Yes by 2
internet_new[internet_new == "DSL"] <- 1       # Replacing No by 1
internet_new[internet_new == "None"] <- 0      # Replacing No by 0
internet_new <- as.integer(internet_new)      # Converting character to numeric
internet_new  


#Replace categorical variables with integer version
churn_updated$Churn <- churn_new
churn_updated$Contract <- contract_new
churn_updated$DeviceProtection <- device_protection_new
churn_updated$Gender <- gender_new
churn_updated$InternetService <- internet_new
churn_updated$Multiple <- multiple_new
churn_updated$OnlineSecurity <- online_security_new
churn_updated$OnlineBackup <- online_backup_new
churn_updated$PaperlessBilling <- paperless_billing_new
churn_updated$Phone <- phone_new
churn_updated$Port_modem <- port_modem_new
churn_updated$StreamingTV <- streaming_tv_new
churn_updated$StreamingMovies <- streaming_movies_new
churn_updated$Tablet <- tablet_new
churn_updated$Techie <- techie_new
churn_updated$TechSupport <- techsupport_new


#Remove unnecessary variables that will not be included in logistic regression model
churn_prepared <- subset(churn_updated, select = -c(CaseOrder, Customer_id, Interaction, UID, City, State, County, Zip, Lat, Lng, Population, Area, TimeZone, Job, Marital, PaymentMethod))


#View summary statistics using
summary(churn_prepared)


#Display univariate visualizations for distribution analysis
num_customers = 1


#Churn
churn_plot <- ggplot(churn_clean,
                     aes(Churn,num_customers)) +
  geom_bar(stat = "identity")
churn_plot

#Gender
Gender_plot <- ggplot(churn_clean,
                      aes(Gender,num_customers)) +
  geom_bar(stat = "identity")
Gender_plot


#Bandwidth
Bandwidth_plot <- ggplot(churn_clean,
                         aes(Bandwidth_GB_Year)) +
  geom_boxplot()
Bandwidth_plot


#MonthlyCharge
MonthlyCharge_plot <- ggplot(churn_clean,
                             aes(MonthlyCharge)) +
  geom_boxplot()
MonthlyCharge_plot


#Reliability
Reliability_plot <- ggplot(churn_prepared,
                           aes(Reliability)) +
  geom_bar()
Reliability_plot


#Display bivariate visualization for distribution analysis with target variable

#Outage(Bi)
Outage_biplot <- ggplot(churn_clean,
                        aes(Churn, Outage_sec_perweek)) +
  geom_point() 
Outage_biplot

#Income(Bi)
Income_biplot <- ggplot(churn_clean,
                        aes(Churn, Income)) +
  geom_point() +
  ylim(0,300000)
Income_biplot

#Age(Bi)
Age_biplot <- ggplot(churn_clean,
                     aes(Churn, Age)) +
  geom_point() 
Age_biplot

#Table Creation for Biplot
churn_test <- churn_new
other_table <- table(churn_prepared$Gender, churn_test)


#Gender(Bi)
Gender_Churn <- barplot(other_table,
                        main = "Gender v Churn",
                        xlab = "Churn", ylab = "Frequency",
                        col = c("darkgrey", "darkblue", "red"),
                        beside = TRUE) # Grouped bars

Gender_Churn


#Code of Prepared Data Set
library(data.table)

fwrite(churn_prepared, "C:\\Users\\eyerm\\Dropbox\\PC\\Downloads\\churn_prepared.csv")
View(churn_prepared)


#Initial Multiple Logistic Regression Model:
Logistic_Model <- glm(churn_new ~ Children + Age + Income + gender_new + Tenure + Outage_sec_perweek + Email +
                        Contacts + Yearly_equip_failure + techie_new + port_modem_new + tablet_new + 
                        internet_new + phone_new + multiple_new + online_backup_new + online_security_new + 
                        streaming_movies_new + streaming_tv_new + Bandwidth_GB_Year + MonthlyCharge + 
                        paperless_billing_new + contract_new + device_protection_new + techsupport_new + 
                        Timely_response + Timely_fixes + Timely_replacements + Reliability + Options + 
                        Respectful_response + Active_listening + Courteous_exchange, family = "binomial", data = churn_prepared)


summary(Logistic_Model)

pscl::pR2(Logistic_Model)["McFadden"]


#Reduced Logistic Regression Model
Reduced_Logistic_Model <- glm(churn_new ~ Children + Age + Tenure + techie_new + port_modem_new + 
                                internet_new + phone_new + online_backup_new + online_security_new + 
                                streaming_movies_new + streaming_tv_new + Bandwidth_GB_Year + MonthlyCharge + 
                                paperless_billing_new + contract_new + device_protection_new, family = "binomial", data = churn_prepared)


summary(Reduced_Logistic_Model)

pscl::pR2(Reduced_Logistic_Model)["McFadden"]



#Initial Model Confusion Matrix
predicted <- predict(Logistic_Model, churn_prepared, type="response")

confusionMatrix(churn_new, predicted)

sensitivity(churn_new, predicted)

specificity(churn_new, predicted)

misClassError(churn_new, predicted, threshold="optimal")



#Reduced Model Confusion Matrix
predicted_2 <- predict(Reduced_Logistic_Model, churn_prepared, type="response")

confusionMatrix(churn_new, predicted_2)

sensitivity(churn_new, predicted_2)

specificity(churn_new, predicted_2)

misClassError(churn_new, predicted_2, threshold="optimal")

