#text analysis
library(stringr)
library(tidyverse)

con <- file("R/session7/Obama_Speech_2-24-09.txt", "r", blocking = FALSE,encoding="UTF-8")
#please read this about encoding:
#http://people.fas.harvard.edu/~izahn/posts/reading-data-with-non-native-encoding-in-r/
text <- readLines(con)
close(con) 
#now clean your text
text2 <-
  text %>% str_to_lower() %>%
  #replace any non character with a blank
  str_replace_all("[^a-z\\s]", " ") %>% 
  #replace multiple balnks with one blank
  str_replace_all("[\\s]+", " ") %>% 
  #split the string into words
  str_split(boundary("word")) %>%
  #in case there are multiple strings in a list reduce this to a vector 
  unlist()

#now we can start analysing the words
#average word length
text2 %>% 
  str_length() %>% mean()
#count number of sentences.
#for this use the original file as we have removed all additional characters
text %>% str_to_lower() %>%
  #replace any non character with a blank
  str_replace_all("[^a-z\\s\\.]", " ") %>% 
  #replace multiple balnks with one blank
  str_replace_all("[\\s]+", " ") %>% 
  #split the string into words
  str_split(boundary("sentence")) %>%
  unlist() %>%
  str_length() %>%
  mean()
# but what about abbreviations like Mr.???
# wird automatisch erkannt von stringr

#Create regular expressions to find all words that:
  #   1. Start with a vowel.
  #   2. That only contain consonants. (Hint: thinking about matching “not”-vowels.)
  #   3. End with ed, but not with eed.
  #   4. End with ing or ise.
text %>% str_to_lower() %>%
  str_view("^[aeiouy]", match=T)
text %>% str_to_lower() %>%
  str_count("^[aeiouy]")
text %>% str_to_lower() %>%
  str_view_all("[ise$]") 

  
#for more exercises see:
#https://github.com/siskavera/r4ds_exercises/blob/master/strings.R




