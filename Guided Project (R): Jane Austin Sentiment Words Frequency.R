# Guided Project Source: https://data-flair.training/blogs/data-science-r-sentiment-analysis-project/

library(janeaustenr) # textual data in the form of books by Jane Austen - austen_books()
library(stringr)
library(tidytext) # get_sentiments("bing"/ "AFINN" / "loughran")
library(dplyr)

tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter[\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
ungroup() %>%
unnest_tokens(word, text)  

positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive") # filtering only positive sentiments

tidy_data %>%
  filter(book == "Emma") %>% # getting the book "Emma"
  semi_join(positive_senti) %>% # getting the words included both in lexicon and book
  count(word, sort= TRUE)


library(tidyr)

bing <- get_sentiments("bing")
Emma_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "Emma", index = linenumber %/% 80, sentiment)  %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

# aes(x-axis, y-axis, legenda ) - aesthetic mapping
ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
# adding layer geom_bar(statistical transformation ("bin" - number of cases; "identity" - map a value to the y aes), )
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)

counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col() +
  coord_flip() + 
  labs(y = 'Sentiment Score')

library(reshape2)
library(wordcloud)
tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word  ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"), max.words = 100)
