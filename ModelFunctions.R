

library(tidyverse)
library(stringr)

# setwd("C:/Data Science/John Hopkins - Coursera/Capstone Project/Capstone Project")


bi_words <- readRDS("bi_words.rds")
tri_words  <- readRDS("tri_words.rds")
quad_words <- readRDS("quad_words.rds")




bigram <- function(input_words){
  num <- length(input_words)
  filter(bi_words, 
         word1==input_words[num]) %>% 
    top_n(3, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "?", return(out))
}


trigram <- function(input_words){
  num <- length(input_words)
  filter(tri_words, 
         word1==input_words[num-1], 
         word2==input_words[num])  %>% 
    top_n(3, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
  num <- length(input_words)
  filter(quad_words, 
         word1==input_words[num-2], 
         word2==input_words[num-1], 
         word3==input_words[num])  %>% 
    top_n(3, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> Output_word
  ifelse(Output_word=="character(0)", trigram(input_words), return(Output_word))
}


ngrams <- function(input){
  # Create a dataframe
  input <- data_frame(text = input)
  # Clean the Inpput
  replace_reg <- "[^[:alpha:][:space:]]*"  
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word count, separate words, lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  # Call the matching functions
  out <- ifelse(input_count==0 , "Enter Some String" , 
                ifelse(input_count == 1, bigram(input_words), 
                       ifelse (input_count == 2, trigram(input_words), quadgram(input_words))))
  
  out <- ifelse(out=="?" , "a" , out)
  # Output
  return(out)
}
