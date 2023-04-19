library(data.table)
library(tidyverse)

#============================
#importing the excel bank statement data
#============================
dt_read <- read_csv('data/Customer Statement.csv')
View(dt_read)


#============================
#Cleaning the data to generate what is needed
#Deleting unwanted columns
#============================
dt_read <- dt_read[,!names(dt_read) %in% c("Category", "Description")]

#renaming the columns to remove ambiguous characters
names(dt_read) <- c("Date", "Money_In", "Money_out", "Recipient", "Balance")

#transform column headers to lowercase 
names(dt_read)[c(1:5)] <- tolower(names(dt_read)[c(1:5)])

#transform content of Recipient column to lowercase for later string matching
dt_read$recipient = tolower(dt_read$recipient)


#adding a new column/Adding Filters
dt_read <- dt_read %>%
  add_column(DebitCredit = "")  
  dt_read$DebitCredit <- ifelse(is.na(dt_read$money_in), "Debit", "Credit")
  
#============================
#Main Analysis
#============================

#1) Amount spent on subscriptions (namecheap, netflix etc)
#using Regex to get similar lines from the recipient table
  
sub <- c("flutterwave", "raenest", "paystack", "fincra", "vfd", "mono", "raven", "aboki", "chipper")

#the criteria has multiple patterns so we pass a|b| using the collapse keyword into a sub pattern
sub_pattern <- paste(sub, collapse = "|") 

dt_read <- dt_read %>% 
  add_column(Category = "")

dt_read$Category <- grepl(sub_pattern, dt_read$recipient) 
dt_read$Category <- ifelse(dt_read$Category == "TRUE", "Subscription", "")



#getting the total amount spent on various subscriptions/max within the data range

dt_sub <- filter(dt_read, Category == "Subscription" & DebitCredit == "Debit")
View(dt_sub)

amount <- as.numeric(dt_sub$money_out)

dt_sub %>%
  summarise(max(money_out)) #50010
  summarise(sum(money_out)) #535846

#2) Getting the total credits/Debits

credits = sum(dt_read$money_in,na.rm=TRUE) #8334236 - credits

debits = sum(dt_read$money_out,na.rm=TRUE) #8331165 - debits

difference <- credits - debits #3070.54 difference



dt_sub %>% 
  mutate(difference = sum(money_in)-sum(money_out))

write.csv(dt_read, file = "general_table.csv", row.names = FALSE)
write.csv(dt_sub, file = "subscription_table.csv", row.names = FALSE)