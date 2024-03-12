
## Functions used in the main coding file


# function to get information contained in the $included-list
get_information <- function(call_list, i) {
  list <- call_list[[1]]
  tryCatch(
    {
      return(tolower(list$included[[i]]$attributes$name))
    },
    error = function(e) {
      return(NA)
    }
  )
}

# function to get names of themes, which can also contain sector or industry information
get_themes <- function(call_list, i) {
  list <- call_list[[1]]
  tryCatch(
    {
      return(gsub("-", " ", names(list$data$attributes$themes)[i]))
    },
    error = function(e) {
      return(NA)
    }
  )
}

# function to return sector information from all information columns, depending on
# in which element the sector information is found
get_industry <- function(row, sectors) {
  match <- sectors[match(tolower(row), sectors)]
  return(unique(match[!is.na(match)]))
}


# function to split content in executive presentation  and Q&A session
split_content <- function(text, before_after) {
  tryCatch(
    {
      split_text <- strsplit(tolower(text), tolower("<strong>Question-And-Answer Session</strong>"))
      if (before_after == "b") {
        return(split_text[[1]][1])
      } else if (before_after == "a") {
        return(split_text[[1]][2])
      }
    },
    error = function(e) {
      return(NA)
    }
  )
  
}


# function to filter words by number of characters in each text
filter_by_length <- function(text, min_length, max_length) {
  words <- unlist(strsplit(text, " "))
  selected_words <- words[nchar(words) >= min_length & nchar(words) <= max_length]
  paste(selected_words, collapse = " ")
}


# custom cleaning function. We write this function so we only have to change
# cleaning steps in one place in the code, if we want to change it
#   (1) convert to lower
#   (2) replace contractions with extended form
#   (3) replace ai with aiaiai to filter words by length
#   (4) remove punctuation
#   (5) remove numbers
#   (6) remove stopwords
#   (7) remove custom defined stopwords
#   (8) filter words by length
#   (9) remove excess whitespaces
#   (10) Lemmatize stings
custom_clean <- function(text, custom_stopwords, custom_contractions) {
  text %>% 
    tolower() %>% 
    str_replace_all(., custom_contractions) %>%
    str_replace_all("\\bai\\b", " aiaiai ") %>% 
    str_replace_all("´+|[0-9]+|[[:punct:]]|\\(.*\\)", " ") %>% 
    removeNumbers() %>% 
    removeWords(stopwords(kind = "en")) %>% 
    removeWords(custom_stopwords) %>%
    sapply(FUN = filter_by_length, min_length = 3 , max_length = 20) %>% 
    str_replace_all("\\s+", " ") %>% 
    lemmatize_strings()
}

# function to get collapsed unique topic words
get_topics <- function(text) {
  text %>%
    quanteda::tokens() %>%
    unlist() %>%
    unique() %>%
    paste(., collapse = " ") %>% 
    as.character()
}
