

# Install and load the quantmod package
install.packages("quantmod")
library(quantmod)

# Define the stock symbol and specify the start and end dates
stock_symbol <- "MDT"
start_date <- "2022-01-01"
end_date <- Sys.Date()  # Use today's date

# Fetch historical stock data
getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date)

# Print the loaded data
stock <- get(stock_symbol)






# Replace ########## with the actual CIK number in the URL
cik_number <- "0000320193"
api_url <- paste0("https://data.sec.gov/submissions/CIK", cik_number, ".json")
api_url <- "https://data.sec.gov/submissions/CIK0000320193.json"

# Make the API request
response <- GET(api_url)

# Parse the JSON response
api_call_test <- fromJSON(content(response, "text"))

?GET





# Your character vector
my_text <- "Some text before<strong>Question-And-Answer Session</strong>Some text after"

# Split the text based on the specified string
split_text <- strsplit(my_text, "<strong>Question-And-Answer Session</strong>")

# Extract the first part (text before the specified string)
text_before <- split_text[[1]][1]

# Extract the second part (text after the specified string)
text_after <- split_text[[1]][2]

# Print the results
cat("Text Before:", text_before, "\n")
cat("Text After:", text_after, "\n")





test_string <- "partner with these companies openai´s openai's also validates how valuable our audience is to some of the biggest brands. The hot topic right now is AI and machine learning. We've all seen that large language models, such as ChatGPT"


test_string %>% 
  #removePunctuation() %>%
  str_replace_all("´+|[0-9]+|[[:punct:]]|\\(.*\\)", " ") %>% 
  removeWords(stopwords(kind = "en")) %>% 
  removeWords(custom_stopwords) %>% 
  str_replace_all("\\s+", " ") %>% 
  filter_by_length(min_length = 3 , max_length = 20)



data.cleaned %>% 
  filter(id %in% c("4506219")) %>% 
  head(15) %>% 
  pull(content_full)









test_data <- keyword_freq %>% 
  filter(total_mentions > 0) %>% 
  group_by(gics_sector) %>% 
  summarise(n_mentions = n_distinct(company_name)) %>% 
  left_join(comp_per_sect, by = "gics_sector") %>% 
  mutate(mentions_weighted = (n_mentions / total_companies) * 100) %>% 
  mutate(not_mentions_weighted = 100 - mentions_weighted) %>% 
  #filter(gics_sector %in% c("Communication Services", "Energy")) %>% 
  select(gics_sector, mentions_weighted, not_mentions_weighted)

test_data <- keyword_freq %>% 
  group_by(gics_sector) %>% 
  summarise(n_mentions = sum(total_mentions)) %>% 
  select(gics_sector, n_mentions)

test_data <- data.frame(test_data)


# Create a contingency table
contingency_table <- table(test_data$gics_sector, test_data$mentions_weighted)

# Perform the chi-squared test
chi_squared_test <- chisq.test(contingency_table)
fisher_test_result <- fisher.test(contingency_table)

# Print the test result
print(chi_squared_test)
print(fisher_test_result)




keyword_freq %>% 
  filter(total_mentions > 0) %>% 
  filter(gics_sector == "Energy")
