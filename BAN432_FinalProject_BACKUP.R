
# libraries
library(readr) # read the data
library(readxl) # read xlsx file
library(rvest) # parse html
library(tidyverse) # basic data wrangling
library(httr) # API call to edgar api
library(jsonlite) # handle json result returned by API
library(writexl) # write output to excel for in-between analysis
library(quanteda) # KWIC function
library(stopwords) # handling stopwords for data cleaning
library(purrr) # further data wrangling
library(tm) # text mining tools to clean text
library(textstem) # lemmatize texts
library(slam)
library(tidytext)
library(lubridate)
library(stringr)
library(topicmodels)
library(wordcloud)
library(sentimentr)
library(SentimentAnalysis)
library(lexicon)
library(udpipe)
library(stringi)
library(tokenizers)

# load custom functions from functions file
source("BAN432_FinalProject_Functions.R")

# in this file we define additional data objects manually to keep the main file clean
source("BAN432_FinalProject_AdditionalData.R")

# read gics classification file
gics <- read_excel("data_other/gics_classification.xlsx") %>% rename(symbol = Identifier) %>% 
  mutate(symbol = tolower(symbol))

# load additional stopwords to remove in the data cleaning process
custom_stopwords <- read_excel("data_other/stopwords_extended.xlsx")
custom_stopwords <- as.vector(custom_stopwords$stopwords)



#########################################################################


# DATA CLEANING -----

## Load and Inspection ------

# load all file names for the earning calls in a vector
files <- list.files("dataShareWithStudents/dataShareWithStudents/")

# create tibble containing a row for each earning call (list object)
data.raw <- tibble(call = list())

# load all earning calls in dataframe
for (i in 1:length(files)) {
  file <- read_rds(paste0("dataShareWithStudents/dataShareWithStudents/", files[i]))
  data.raw <- data.raw %>%
    add_row(call = list(file))
}

# By inspecting the list for one earning call, we realize:
# the "included"-elements contain useful information about the company
# We realize that some earning calls contain more information than others
# We decide to check for up to 12 "included"-elements
# one important information we are interested in is the industry or sector of the company
data.raw$information2 <- apply(data.raw, 1, get_information, i = 2)
data.raw$information3 <- apply(data.raw, 1, get_information, i = 3)
data.raw$information4 <- apply(data.raw, 1, get_information, i = 4)
data.raw$information5 <- apply(data.raw, 1, get_information, i = 5)
data.raw$information6 <- apply(data.raw, 1, get_information, i = 6)
data.raw$information7 <- apply(data.raw, 1, get_information, i = 7)
data.raw$information8 <- apply(data.raw, 1, get_information, i = 8)
data.raw$information9 <- apply(data.raw, 1, get_information, i = 9)
data.raw$information10 <- apply(data.raw, 1, get_information, i = 10)
data.raw$information11 <- apply(data.raw, 1, get_information, i = 11)
data.raw$information12 <- apply(data.raw, 1, get_information, i = 12)

# We inspect the 12 new columns and see that in most cases,
# element 4 contains industry or sector classification
# (although in a few cases, element 4 contains other information)
data.raw$information4
# We look at the unique values of element 4 and manually extract 9 sectors
data.raw$information8 %>%  unique()
sectors <- c("healthcare",
             "technology",
             "industrial goods",
             "consumer goods",
             "financial",
             "basic materials",
             "services",
             "utilities",
             "conglomerates")

# check, how many of our earning calls do not contain sector information in element 4
data.raw %>% 
  filter(!information4 %in% sectors)

# Make industry column
data.raw$industry <- apply(data.raw, 1, get_industry, sectors = sectors)
data.raw$industry <- ifelse(data.raw$industry %in% sectors, data.raw$industry, NA)

# add a symbol column and a date column
data.raw <- data.raw %>% 
  mutate(symbol = information2) %>% 
  mutate(map_df(call, ~ tibble(id = .x$data$id))) %>% 
  mutate(map_df(call, ~ tibble(date = as.Date(.x$data$attributes$publishOn)))) %>% 
  mutate(year = format(date, "%Y"),
         month = month(date),
         quarter = quarter(date),
         year_month = format(date, "%Y-%m"),
         year_quarter = paste0(year(date), "-Q", quarter(date)))

# we can drop the information columns now
data.raw <- data.raw %>% select(-matches("^information.*"))

# still around 40% that do not contain ANY industry information in any of the
# "included"-elements
data.raw %>% 
  filter(!industry %in% sectors) %>% 
  select(call) %>% 
  head(1)

# so we try to get the industry from the themes and merge it with the approach above
data.raw$theme1 <- apply(data.raw, 1, get_themes, i = 1)
data.raw$theme2 <- apply(data.raw, 1, get_themes, i = 2)
data.raw$theme3 <- apply(data.raw, 1, get_themes, i = 3)
data.raw$theme4 <- apply(data.raw, 1, get_themes, i = 4)
data.raw$theme5 <- apply(data.raw, 1, get_themes, i = 5)
data.raw$theme6 <- apply(data.raw, 1, get_themes, i = 6)
data.raw$theme7 <- apply(data.raw, 1, get_themes, i = 7)
data.raw$theme8 <- apply(data.raw, 1, get_themes, i = 8)

# theme3 mostly contains sector information
data.raw$industry_theme <- ifelse(data.raw$theme3 %in% sectors, data.raw$theme3, NA)

# we can drop the other themes now
data.raw <- data.raw %>% select(-matches("^theme.*"))

# still 3648 earnings calls where neither the included elements nor the themes
# contain sector or industry information
data.raw %>% 
  filter(!is.na(industry) | !is.na(industry_theme))

# join with gics industry classification by stock symbol (dowlnoaded from refinitiv)
data.raw <- data.raw %>% 
  left_join(., gics, by = "symbol") %>% 
  rename(gics_sector = `GICS Sector Name`,
         gics_industry_group = `GICS Industry Group Name`,
         gics_industry = `GICS Industry Name`,
         company_name = `Company Name`) %>% 
  select(-`Country of Exchange`, -`Identifier_Original`)

# from our gics industry dataset, we can assign industries to 7310 of the 7643
# earnings calls. This will be the raw dataset that we work with for the analysis
# we exlcude the ones that we cant find gics industry matches for
data.raw <- data.raw %>% 
  filter(!is.na(gics_industry_group))

# we save it so we can load it the next time
save(data.raw, file = "raw/data_raw.RData")



## Data Cleaning ----
load("raw/data_raw.RData")

# we continue with a subset of all data for quicker computation and tests
set.seed(1234)

data.sample <- data.raw %>% 
  # filter(year == "2023") %>%
  sample_n(2000)

# preprocessing the html content of the earnings calls
#   (1) we extract the text from the html content
#   (2) we split the text into pre QA session and after QA session
data.pre <- data.raw %>%
  # get the raw content with html tags
  mutate(map_df(call, ~ tibble(raw_content = .x$data$attributes$content))) %>% 
  # convert raw content to html so we can apply rvest html functions
  mutate(raw_content_html = map(raw_content, read_html)) %>% 
  # extract only the text from the html file, no tags etc
  mutate(content_full = as.character(map(raw_content_html, html_text))) %>%
  # split into before and after Q and A session (Apply same steps as above)
  mutate(raw_beforeQA = as.character(map(raw_content, split_content, before_after = "b")),
         raw_afterQA = as.character(map(raw_content, split_content, before_after = "a"))) %>%
  mutate(raw_beforeQA_html = map(raw_beforeQA, read_html),
         raw_afterQA_html = map(raw_afterQA, possibly(read_html, otherwise = NA_character_))) %>% 
  mutate(beforeQA_full = as.character(map(raw_beforeQA_html, html_text)),
         afterQA_full = as.character(map(raw_afterQA_html, possibly(html_text, otherwise = NA_character_)))) %>%
  select(symbol,
         company_name,
         id,
         date,
         year,
         month,
         quarter,
         year_month,
         year_quarter,
         gics_sector,
         gics_industry_group,
         gics_industry,
         raw_content,
         content_full,
         beforeQA_full,
         afterQA_full
  )

# save the pre-processed dataset
save(data.pre, file = "processed/data_pre.Rdata")

# load pre-processed dataset
load("processed/data_pre.Rdata")

# apply text cleaning steps defined in the function R-file
data.cleaned <- data.pre %>% 
  mutate(content_cleaned = custom_clean(content_full, custom_stopwords, contractions),
         beforeQA_cleaned = custom_clean(beforeQA_full, custom_stopwords, contractions),
         afterQA_cleaned = custom_clean(afterQA_full, custom_stopwords, contractions))

# further special cleaning steps
# (1) remove company names
companies <- tolower(data.cleaned$company_name) %>% unique() %>% as.vector() %>% 
  removePunctuation() %>% 
  str_split(., pattern = " ") %>%
  unlist() %>% 
  unique()

companies <- companies[nchar(companies) > 2]

data.cleaned$content_cleaned <- data.cleaned$content_cleaned %>% removeWords(companies)
data.cleaned$beforeQA_cleaned <- data.cleaned$beforeQA_cleaned %>% removeWords(companies)
data.cleaned$afterQA_cleaned <- data.cleaned$afterQA_cleaned %>% removeWords(companies)

# save the cleaned dataset
save(data.cleaned, file = "cleaned/data_cleaned.Rdata")

# load cleaned dataset
load("cleaned/data_cleaned.Rdata")


# GENERAL ANALYSIS -----
# Number of earnings calls per GICS industry group
plot_n_per_group <- data.cleaned %>% 
  group_by(gics_industry_group) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(y = gics_industry_group, x = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), hjust = -0.2, color = "black") +
  labs(title = "Number of earnings calls per GICS industry group", x = "Count", y = "Industry Group") +
  theme_minimal()

# Number of earnings calls per GICS sector
plot_n_per_sector <- data.cleaned %>% 
  group_by(gics_sector) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(y = reorder(gics_sector, n), x = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), hjust = -0.2, color = "black", size = 6) +
  labs(title = "Number of earnings calls per GICS sector", x = "", y = "Sector") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

# Number of companies per sector
plot_ncomp_per_sector <- data.cleaned %>% 
  group_by(gics_sector) %>% 
  summarise(n = n_distinct(company_name)) %>% 
  ggplot(aes(y = reorder(gics_sector, n), x = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), hjust = -0.2, color = "black", size = 6) +
  labs(title = "Number of companies per sector", x = "", y = "Sector") +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

png("plots/plot_ncomp_per_sector.png", width = 1200, height = 800)
plot_ncomp_per_sector
dev.off()

# Number of earnings calls per quarter
plot_n_per_q <- data.cleaned %>% 
  group_by(year_quarter) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = year_quarter, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.2, color = "black", size = 6) +
  labs(title = "Number of earnings calls per quarter", x = "", y = "count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))
  
png("plots/plot_calls_per_quarter.png", width = 1200, height = 800)
plot_n_per_q
dev.off()



# KEYWORD ANALYSIS -----

# make DTM of our cleaned data
dtm_full <- data.cleaned$content_cleaned %>% 
  VectorSource() %>% 
  Corpus() %>% 
  DocumentTermMatrix(control = list(bounds = list(global = c(1,4000))))

dtm_full$dimnames$Docs <- data.cleaned$id


## Keyword frequencies ----

# make frequency list of keywords
keyword_freq <- dtm_full %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  select(matches(paste0("\\b", keywords, "\\b"))) %>% 
  mutate(total_mentions = rowSums(.)) %>% 
  cbind(gics_industry_group = data.cleaned$gics_industry_group) %>% 
  cbind(gics_sector = data.cleaned$gics_sector) %>% 
  cbind(date = data.cleaned$date) %>% 
  cbind(id = data.cleaned$id) %>% 
  cbind(company_name = data.cleaned$company_name)

# which keywords are the most common accross industries
keyword_freq %>% 
  select(chatgpt, gpt, openai) %>% 
  summarise_all(sum)

# mentions of AI and chatgpt keywords per industry
plot_key_per_group <- keyword_freq %>% 
  select(-date, -gics_sector, -id, -company_name) %>% 
  group_by(gics_industry_group) %>% 
  summarise_all(sum) %>% 
  mutate(total_n = rowSums(select(., -gics_industry_group))) %>% 
  select(gics_industry_group, total_n) %>% 
  arrange(desc(total_n)) %>% 
  ggplot(aes(x = total_n, y = reorder(gics_industry_group, total_n))) +
  geom_bar(stat = "identity") +
  labs(title = "Mentions of chatgpt per GICS industry group", x = "count", y = "GICS Industry Group") +
  geom_text(aes(label = total_n), hjust = -0.2, color = "black", size = 6) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

png("plots/plot_key_per_group.png", width = 1200, height = 800)
plot_key_per_group
dev.off()

# mentions of AI and chatgpt keywords per sector
plot_key_per_sector <- keyword_freq %>% 
  select(-date, -gics_industry_group, -id, -company_name) %>% 
  group_by(gics_sector) %>% 
  summarise_all(sum) %>% 
  mutate(total_n = rowSums(select(., -gics_sector))) %>% 
  select(gics_sector, total_n) %>% 
  arrange(desc(total_n)) %>% 
  ggplot(aes(x = total_n, y = reorder(gics_sector, total_n))) +
  geom_bar(stat = "identity") +
  labs(title = "Mentions of chatgpt per GICS sector", x = "count", y = "GICS sector") +
  geom_text(aes(label = total_n), hjust = -0.2, color = "black", size = 6) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

png("plots/plot_key_per_sector.png", width = 1200, height = 800)
plot_key_per_sector
dev.off()

# mentions of AI and chatgpt keywords per sector (weighted)
plot_key_weighted <- keyword_freq %>% 
  group_by(gics_sector) %>% 
  summarise(ndocs = n(),
            nocc = sum(total_mentions)) %>% 
  mutate(freq_weighted = (nocc / ndocs) * 100) %>% 
  ggplot(aes(x = freq_weighted, y = reorder(gics_sector, freq_weighted))) +
  geom_bar(stat = "identity") +
  labs(title = "Mentions of chatgpt per GICS sector (weighted)", x = "count", y = "GICS sector") +
  geom_text(aes(label = round(freq_weighted)), hjust = -0.2, color = "black", size = 6) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

png("plots/plot_key_weighted.png", width = 1200, height = 800)
plot_key_weighted
dev.off()

# chatgpt related mentions per company
# we see that microsoft makes up a big share of the mentions
plot_key_per_comp <- keyword_freq %>% 
  group_by(company_name) %>% 
  summarise(sum = sum(total_mentions)) %>% 
  arrange(desc(sum)) %>% 
  head(10) %>% 
  ggplot(aes(x = sum, y = reorder(company_name, sum))) +
  geom_bar(stat = "identity") +
  labs(title = "Mentions of chatgpt per company", x = "", y = "Company Name") +
  geom_text(aes(label = sum), hjust = -0.2, color = "black", size = 6) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

png("plots/plot_key_per_comp.png", width = 1200, height = 800)
plot_key_per_comp
dev.off()

# percentage of companies mentioning chatgpt per sector
comp_per_sect <- keyword_freq %>% 
  group_by(gics_sector) %>% 
  summarise(total_companies = n_distinct(company_name))

plot_key_comp_perc <- keyword_freq %>% 
  filter(total_mentions > 0) %>% 
  group_by(gics_sector) %>% 
  summarise(n_mentions = n_distinct(company_name)) %>% 
  left_join(comp_per_sect, by = "gics_sector") %>% 
  mutate(mentions_weighted = (n_mentions / total_companies) * 100) %>% 
  ggplot(aes(x = mentions_weighted, y = reorder(gics_sector, mentions_weighted))) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of companies per sector mentioning chatGPT",x = "", y = "GICS sector") +
  geom_text(aes(label = round(mentions_weighted, 2)), hjust = -0.2, color = "black", size = 6) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

png("plots/plot_key_comp_perc.png", width = 1200, height = 800)
plot_key_comp_perc
dev.off()


# percentage of companies mentioning chatgpt per sector
# this time for pre release and post release
cutoff_date <- as.Date("2022-11-30")

comp_per_sect_date <- keyword_freq %>% 
  mutate(before_after_release = ifelse(date < cutoff_date, "Before", "After")) %>% 
  group_by(gics_sector, before_after_release) %>% 
  summarise(total_companies = n_distinct(company_name))

plot_key_comp_perc_release <- keyword_freq %>% 
  filter(total_mentions > 0) %>% 
  mutate(before_after_release = ifelse(date < cutoff_date, "Before", "After")) %>% 
  group_by(gics_sector, before_after_release) %>% 
  summarise(n_mentions = n_distinct(company_name)) %>% 
  left_join(comp_per_sect_date, by = c("gics_sector", "before_after_release")) %>% 
  mutate(mentions_weighted = (n_mentions / total_companies) * 100) %>% 
  ggplot(aes(x = mentions_weighted, y = reorder(gics_sector, mentions_weighted), fill = before_after_release)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Before" = "blue", "After" = "red")) +
  facet_wrap(~ before_after_release, scales = "free_y", ncol = 1) +
  labs(title = "Percentage of companies per sector mentioning chatGPT",x = "", y = "GICS sector") +
  geom_text(aes(label = round(mentions_weighted, 2)), hjust = -0.2, color = "black", size = 6) +
  theme_minimal() +
  theme(legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20))

png("plots/plot_key_comp_perc_release.png", width = 1200, height = 800)
plot_key_comp_perc_release
dev.off()


# To asses if companies are actually affected when they talk about ChatGPT,
# we look at how the talk about it by analyzing the surrounding context of the keywords
# we do not work with the dtm, but with the full content here, to create a tokens
# object that we can feed into the KWIC function
toks <- data.cleaned$content_full %>% 
  tolower() %>% 
  str_replace_all("\\bai\\b", " aiaiai ") %>% 
  str_replace_all("´+|[0-9]+|[[:punct:]]|\\(.*\\)", " ") %>%
  str_replace_all("\\s+", " ") %>% 
  quanteda::tokens()

# docnames to identify the documents by document ID
docnames(toks) <- data.cleaned$id

# KWIC, we choose a window of 30 to capture more words surrounding the keywords
kw.env <- kwic(x = toks, 
               pattern = paste0("\\b", keywords, "\\b"),
               valuetype = "regex",
               window = "30",
               case_insensitive =  T
)

# We do not want to see certain words, specifically the keywords, which do
# not give any qualitative insight into if keyword mentions means companies are affected
custom_stop <- c("aiaiai", "openai", "chatgpt")

# All tokens after the keyword, filtering out words that are shorter than 5 characters
# and filtering out stopwords and custom stopwords as defined above
pre_post_freq <- kw.env %>%
  as_tibble() %>%
  select(post) %>%
  mutate(post = removeWords(post, stopwords("en"))) %>% 
  mutate(post = removeWords(post, custom_stop)) %>% 
  mutate(post = filter_by_length(post, 5, 20)) %>% 
  unnest_tokens(input = post,
                output = tokens,
                to_lower = T) %>%
  count(tokens, sort = T)

# All tokens before the keyword, filtering out words that are shorter than 5 characters
# and filtering out stopwords and custom stopwords as defined above
pre_post_freq <- kw.env %>%
  as_tibble() %>%
  select(pre) %>%
  mutate(pre = removeWords(pre, stopwords("en"))) %>% 
  mutate(pre = removeWords(pre, custom_stop)) %>%
  mutate(pre = filter_by_length(pre, 5, 20)) %>% 
  unnest_tokens(input = pre,
                output = tokens,
                to_lower = T) %>%
  count(tokens, sort = T) %>% 
  bind_rows(pre_post_freq) # binding before and after together in one tibble

# Make a wordcloud for a nice visual representation of the KWIC
filename <- "plots/wc_kwic_chatgpt.png"
png(filename, 2000, 2000)
wordcloud(words = pre_post_freq$tokens[1:100],
          freq  = pre_post_freq$n[1:100],
          scale = c(15,3))
dev.off()

# analyse the sentiment of the KWIC: how many positive and negative words?







# SENTIMENT ANALYSIS ----

# inspect only those documents that actually talk about AI
# load all ids of earings calls that contain keywords in vector
ai.call.ids <- dtm_full %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  select(matches(paste0("\\b", keywords_2, "\\b"))) %>% 
  mutate(total_sum = rowSums(.)) %>% 
  cbind(id = data.cleaned$id) %>% 
  filter(total_sum > 8) %>% 
  pull(id)

# construct dtm only for the documents that contain AI keywords only
dtm_ai <- dtm_full[dtm_full$dimnames$Docs %in% ai.call.ids,]

# top most frequent negative terms in AI documents with LM Dictionary
LM.neg <- tibble(word = dtm_ai$dimnames$Terms,
                 freq = col_sums(dtm_ai))

LM.neg <- LM.neg %>% 
  inner_join(as_tibble(DictionaryLM$negative), by = c("word" = "value")) %>% 
  filter(freq > 0) %>% 
  arrange(desc(freq))


# look at topics for Documents that talk about AI
topic_ai <- LDA(dtm_ai,  # document term matrix
                k = 15, # specifify number of topics
                method = "Gibbs",
                control = list(
                  seed = 1234, # eases replication
                  burnin = 100,  # how often sampled before estimation recorded
                  iter = 300,  # number of iterations
                  keep = 1,    # saves additional data per iteration (such as logLik)
                  save = F,     # saves logLiklihood of all iterations
                  verbose = 10  # report progress
                ))

# topics are good reasonable topics about AI, however difficult to get risks from
apply(topic_ai@beta, 1, function(x) head(topic_ai@terms[order(x, decreasing = T)],10))



## Sentiment by sentence ----

# first put all documents that talk about AI in a vector
ai.texts <- data.cleaned %>% 
  filter(id %in% ai.call.ids) %>% 
  pull(content_full) %>%
  tolower() %>% 
  as.vector()

# now compute the sentiment for all sentences from documents that contain ai keywords
ai_sentiment <- sentiment(get_sentences(ai.texts))

# store all sentences as tibble to analyse easier
ai_sentences <- get_sentences(ai.texts) %>% unlist()

# add the sentences as column to sentiment dataframe
ai_sentiment$sentence <- ai_sentences

# filter sentiment dataframe for sentences that contain ai keywords
pattern <- paste(paste0("\\b", keywords_2, "\\b"), collapse = "|")
ai_sentiment_filtered <- ai_sentiment[stri_detect_regex(sentence, pattern, case_insensitive = TRUE)]

# display top negative sentences
neg_uncertain <- ai_sentiment_filtered %>% 
  filter(sentiment > 0) %>% 
  arrange(sentiment) %>% 
  #head(200) %>% 
  pull(sentence)

neg_uncertain[18]


## Topic modelling for sentences that contain AI related words

# trying it with bigrams
ai_sentiment_filtered$sentence_bi  <- ai_sentiment_filtered$sentence %>% 
  custom_clean(., custom_stopwords, contractions) %>% 
  tokenize_ngrams(n = 2, ngram_delim = "_") %>% 
  map(paste, collapse = " ") %>% 
  unlist()

# trying it with trigrams
ai_sentiment_filtered$sentence_tri  <- ai_sentiment_filtered$sentence %>% 
  custom_clean(., custom_stopwords, contractions) %>% 
  tokenize_ngrams(n = 3, ngram_delim = "_") %>% 
  map(paste, collapse = " ") %>% 
  unlist()


# construct dtm for unigrams
dtm_ai_sentences <- ai_sentiment_filtered %>%
  filter(sentiment < 0) %>% 
  #group_by(element_id) %>% 
  summarise(ai_text = paste(sentence, collapse = " ")) %>%
  mutate(ai_text = str_replace_all(ai_text, "\\bai\\b", " aiaiai ")) %>% 
  pull(ai_text) %>% 
  VectorSource() %>% 
  Corpus() %>% 
  DocumentTermMatrix(control = list( 
                      removePunctuation = T,
                      stopwords = T,
                      stemming = F,
                      removeNumbers = T,
                      wordLengths = c(4, 40)))

# construct dtm for bigrams
dtm_ai_sentences_bi <- ai_sentiment_filtered %>%
  filter(sentiment < 0) %>% 
  group_by(element_id) %>% 
  summarise(ai_text = paste(sentence_bi, collapse = " ")) %>%
  mutate(ai_text = str_replace_all(ai_text, "\\bai\\b", " aiaiai ")) %>% 
  pull(ai_text) %>% 
  VectorSource() %>% 
  Corpus() %>% 
  DocumentTermMatrix()

# construct dtm for trigrams
dtm_ai_sentences_tri <- ai_sentiment_filtered %>%
  filter(sentiment < 0) %>% 
  group_by(element_id) %>% 
  summarise(ai_text = paste(sentence_tri, collapse = " ")) %>%
  mutate(ai_text = str_replace_all(ai_text, "\\bai\\b", " aiaiai ")) %>% 
  pull(ai_text) %>% 
  VectorSource() %>% 
  Corpus() %>% 
  DocumentTermMatrix()

# freq list of negative sentences
freq_neg <- tibble(token = dtm_ai_sentences$dimnames$Terms,
                   n = col_sums(dtm_ai_sentences))


# look at topics for sentences that contain ai related terms
topic_ai_sentences <- LDA(dtm_ai_sentences,  # document term matrix
                k = 8, # specifify number of topics
                method = "Gibbs",
                control = list(
                  seed = 1234, # eases replication
                  burnin = 500,  # how often sampled before estimation recorded
                  iter = 1500,  # number of iterations
                  keep = 1,    # saves additional data per iteration (such as logLik)
                  save = F,     # saves logLiklihood of all iterations
                  verbose = 10  # report progress
                ))

# still no negative topics that could be interpreted as a AI related risk
apply(topic_ai_sentences@beta, 1, function(x) head(topic_ai_sentences@terms[order(x, decreasing = T)],10))


# top negative terms in AI sentences
LM.neg.sent <- tibble(word = dtm_ai_sentences$dimnames$Terms,
                      freq = col_sums(dtm_ai_sentences))

LM.neg.sent <- LM.neg.sent %>% 
  inner_join(as_tibble(DictionaryLM$negative), by = c("word" = "value")) %>% 
  filter(freq > 0) %>% 
  arrange(desc(freq))



# analyse frequencies of bigrams
LM.neg.sent.bi <- tibble(bigram = dtm_ai_sentences_bi$dimnames$Terms,
                         freq = col_sums(dtm_ai_sentences_bi))

# must contain negative words from LM dictionary
LM.neg.sent.bi <- LM.neg.sent.bi[apply(sapply(DictionaryLM$negative, grepl, x = LM.neg.sent.bi$bigram), 1, any), ]

# must contain our keywords
LM.neg.sent.bi <- LM.neg.sent.bi[apply(sapply(keywords_2, grepl, x = LM.neg.sent.bi$bigram), 1, any), ]

# which negative words occur
LM.neg.sent.bi <- LM.neg.sent.bi %>% 
  mutate(word = str_split(bigram, "_")) %>% 
  unnest(word) %>% 
  inner_join(as_tibble(DictionaryLM$negative), by = c("word" = "value")) %>% 
  filter(!word == "question") %>% 
  arrange(desc(freq))

LM.neg.bi.topics <- LM.neg.sent.bi %>% 
  group_by(word) %>% 
  summarise(topic = paste(str_replace_all(bigram, "_", " "), collapse = " "),
            freq = sum(freq))

LM.neg.bi.topics <- LM.neg.bi.topics %>% 
  mutate(topic = map(topic, get_topics))


LM.neg.bi.topics %>% 
  arrange(desc(freq)) %>% 
  head(40) %>% 
  pull(topic)



# analyse trigrams
LM.neg.sent.tri <- tibble(trigram = dtm_ai_sentences_tri$dimnames$Terms,
                         freq = col_sums(dtm_ai_sentences_tri))

# must contain negative words from LM dictionary
LM.neg.sent.tri <- LM.neg.sent.tri[apply(sapply(DictionaryLM$negative, grepl, x = LM.neg.sent.tri$trigram), 1, any), ]

# must contain our keywords
LM.neg.sent.tri <- LM.neg.sent.tri[apply(sapply(keywords_2, grepl, x = LM.neg.sent.tri$trigram), 1, any), ]

# which negative words occur
LM.neg.sent.tri <- LM.neg.sent.tri %>% 
  mutate(word = str_split(trigram, "_")) %>% 
  unnest(word) %>% 
  inner_join(as_tibble(DictionaryLM$negative), by = c("word" = "value")) %>% 
  filter(!word == "question") %>% 
  arrange(desc(freq))

negative_words <- LM.neg.sent.tri$word %>% unique()

# extract only sentences that contain these negative words
for (word in negative_words) {
  sentences <- ai_sentiment_filtered[grep(paste0("\\b", word, ".*"), ai_sentiment_filtered$sentence, ignore.case = TRUE)] %>%
    pull(sentence) %>% 
    paste(collapse = " ")
  
  test <- append(test, sentences)
}



dtm_test <- test %>% 
  VectorSource() %>% 
  Corpus() %>% 
  DocumentTermMatrix(control = list( 
    removePunctuation = T,
    stopwords = T,
    stemming = F,
    removeNumbers = T,
    wordLengths = c(4, 20)))

topic_test <- LDA(dtm_test,  # document term matrix
                  k = 12, # specifify number of topics
                  method = "Gibbs",
                  control = list(
                    seed = 1234, # eases replication
                    burnin = 500,  # how often sampled before estimation recorded
                    iter = 2000,  # number of iterations
                    keep = 1,    # saves additional data per iteration (such as logLik)
                    save = F,     # saves logLiklihood of all iterations
                    verbose = 10  # report progress
                          ))

apply(topic_test@beta, 1, function(x) head(topic_test@terms[order(x, decreasing = T)],10))

beta <- exp(topic_test@beta)

paste(head(topic_test@terms[order(beta[12,], decreasing = T)], 8), collapse = ", ")


test_bi  <- test %>% 
  custom_clean(., custom_stopwords, contractions) %>% 
  tokenize_ngrams(n = 2, ngram_delim = "_") %>% 
  map(paste, collapse = " ") %>% 
  unlist()

dtm_test_bi <- test_bi %>% 
  VectorSource() %>% 
  Corpus() %>% 
  DocumentTermMatrix(control = list( 
    #removePunctuation = T,
    #stopwords = T,
    #stemming = F,
    #removeNumbers = T,
    wordLengths = c(4, 40)))


LM.neg.bi.topics <- LM.neg.sent.bi %>% 
  group_by(word) %>% 
  summarise(topic = paste(str_replace_all(bigram, "_", " "), collapse = " "),
            freq = sum(freq))

LM.neg.bi.topics <- LM.neg.bi.topics %>% 
  mutate(topic = map(topic, get_topics))


LM.neg.bi.topics %>% 
  arrange(desc(freq)) %>% 
  head(40) %>% 
  pull(topic)


DictionaryGI

# look at KWIC for "risk"

# look at tf-idf matrix for ai sentences/documents
