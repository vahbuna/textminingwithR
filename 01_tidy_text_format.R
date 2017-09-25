#emily dickenson
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

library(dplyr)

# tibble
text_df <- data_frame(line = 1:4, text = text)

library(tidytext)

#tokenize
text_df %>% unnest_tokens(word, text)

#clean jane austen novels
library(janeaustenr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>% #in dplyr
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

#make data tidy by tokenizing again
library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

#remove stop words
data(stop_words)
tidy_books <- tidy_books %>%anti_join(stop_words)

#word frequency
tidy_books %>% count(word, sort = TRUE) 

# display
library(ggplot2)
tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#gutenbergr 
# tutorial https://ropensci.org/tutorials/gutenbergr_tutorial.html

#word frequencies
library(gutenbergr)
# get  The Time Machine, The War of the Worlds, 
# The Invisible Man, and The Island of Doctor Moreau
hgwells <- gutenberg_download(c(35, 36, 5230, 159)) #need curl

#tokenisation and stop word removal
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#freauencies
tidy_hgwells %>%
  count(word, sort = TRUE)

# get  Jane Eyre, Wuthering Heights, The Tenant of Wildfell Hall,
# Villette, and Agnes Grey

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)

library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion)%>% 
  gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)

#plot
library(scales)
# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

#correlation
cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)