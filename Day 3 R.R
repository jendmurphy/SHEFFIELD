require(tidyverse)

install.packages("tm")
install.packages("tidytext")

# **************************************************************#
# CDT Social Analytics and Visualisation (SMI610)
# June 2018
# Instructor: Paul Clough
#
# Examples from http://handsondatascience.com/ and 
# http://onepager.togaware.com/TextMiningO.pdf
# **************************************************************#

# **************************************************************#
# Part 1: Importing data into R
# **************************************************************#

# set working directory at start of tutorial

######################################################
# using base read functions
######################################################

#  Set working directory
setwd("//stfdata07/home/MI/Mip18jdm/ManW10/Desktop/SHEFFIELD/SHEFFIELD")

#  Read in the csv 
meals <- read.csv("https://data.yorkopendata.org/dataset/14b8a985-fc49-4d3e-947e-b4f12c9bf59b/resource/03b0ae33-f6fe-4431-94f8-4b811c8921ba/download/fsmdetails.csv", header=TRUE, stringsAsFactors=FALSE)

head(meals) # take a peek
class(meals) # what type of object

# List out schools in the dataframe
meals$SchoolName  # which is the same as meals[[1]]

# Pull out all records where SchoolName = ""
meals[meals$SchoolName=="Carr Infant School",]

#####################################################
# Using readtext package
######################################################

install.packages("readtext")    # for data preparation
library(readtext)

txt <- readtext("http://ir.shef.ac.uk/cloughie/download/inaugral.txt") # plaintext
str(txt)

#  THe speech is held in a single cell in the dataframe - this displys the whole speech.
txt$text

pdfDoc <- readtext("http://ir.shef.ac.uk/cloughie/download/text_analysis_in_R.pdf")
pdfDoc$text

wordDoc <- readtext("http://ir.shef.ac.uk/cloughie/download/DoingPhD.doc")
wordDoc$text

# Another option is to download first and then import file 
# Note for a Word doc we need to download as a binary file 
# Note you need permissions on the working directory to save the file

download.file("http://ir.shef.ac.uk/cloughie/download/DoingPhD.doc", mode="wb", destfile="DoingAPhD.doc")

wordDoc <- readtext("DoingAPhD.doc")
wordDoc$text


######################################################
# Scraping text from web pages using rvest
######################################################
# Good example tutorial:
# http://bradleyboehmke.github.io/2015/12/scraping-html-text.html

install.packages("rvest")
library(rvest) 

url<-"https://en.wikipedia.org/wiki/Sheffield"
wikiPage <- read_html(url)
wikiPage

class(wikiPage)
str(wikiPage)

h2Sections <- wikiPage %>%
  html_nodes("h2")

h2Sections[1]
h2Sections[2]
h2Sections[1:2]

#  This command strips out the mark up and returns just the text values
h2Sections %>% # remember pipe is ctrl shift m
  html_text()

#  To get the content of a page, select all of the content marked up using the paragraph tags
pageText <- wikiPage %>%  #  save as pageText
  html_nodes("p") %>% # split out markdown nodes 
  html_text() # split just the text out

#  Each section is in a separate item - its in a list.
pageText[103]

#  STRINGER IS A PACKAGE TO DEAL WITH FILTERING OUT UNWANTED CHARACTERS
install.packages("stringr")
library(stringr) 

# get rid of the number references
x <- pageText %>%
  str_replace_all(pattern = "\\[.*?\\]", replacement = "") 

x[1]


######################################################
# Gathering data from an API
######################################################

install.packages("rtimes")
library(rtimes)

nytAPIKey <- "3717e0bf11df4cfcaa63b23ac0a9ec91" # NYTIMES API Key here - signed up as jennifer.murphy@manchester.ac.uk

# search for news articles with the query "Brexit"
articles <- as_search(q="Brexit", key=nytAPIKey)
class(articles) #  the query returns a list

str(articles)

articles$copyright
articles$meta #  A tibble is a dataframe in tidyverse speak

articles$data$web_url[1]
articles$data$snippet[1]

articles$data$snippet[2]

articles <- as_search(q="Trump", begin_date=20171015, end_date=20171016, key=nytAPIKey)
articles$meta

require(dplyr)
snip<-tbl_df(articles$data$snippet)



##############################
#TRYING OUT SOME OTHER STUFF NEED TO REGISTER FOR TWITTER API#
##############################

'''
install.packages("twitteR")
require(twitteR)

install.packages("wordcloud")
require(wordcloud)

#necessary file for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
consumer_key <- 'your key'
consumer_secret <- 'your secret'
access_token <- 'your access token'
access_secret <- 'your access secret'
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

#the cainfo parameter is necessary only on Windows
r_stats <- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")
#should get 1500
length(r_stats)
#[1] 1500

#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())

#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#clean up
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower)) 
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()))
wordcloud(r_stats_text_corpus)

#alternative steps if you running into problems 
r_stats<- searchTwitter("#Rstats", n=1500, cainfo="cacert.pem")
#save text
r_stats_text <- sapply(r_stats, function(x) x$getText())
#create corpus
r_stats_text_corpus <- Corpus(VectorSource(r_stats_text))

#if you get the below error
#In mclapply(content(x), FUN, ...) :
#  all scheduled cores encountered errors in user code
#add mc.cores=1 into each function

'''
#run this step if you get the error:
#(please break it!)' in 'utf8towcs'
'''
r_stats_text_corpus <- tm_map(r_stats_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1
)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, content_transformer(tolower), mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, removePunctuation, mc.cores=1)
r_stats_text_corpus <- tm_map(r_stats_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
wordcloud(r_stats_text_corpus)

'''

# **************************************************************#
# Using tm for text mining and analysis
# **************************************************************#

# install and load packages
install.packages("tm")
library(tm)


# import texts from directory
dirname <- file.path(".", "texts") # define the path of the document collection
dirname # print path
length(dir(dirname)) # number of files in the folder
dir(dirname) # list of file names in the folder

?Corpus
getSources()

trumpCorpus <- Corpus(DirSource(dirname, encoding="UTF-8"))

# trumpCorpus <-Corpus(DirSource("./texts", encoding="UTF-8")) 

str(trumpCorpus)
summary(trumpCorpus)

# We can inspect the documents and their metadata
inspect(trumpCorpus[1])
trumpCorpus[[1]]
trumpCorpus[[1]]$meta
trumpCorpus[[1]]$content

# By way of another example - the following makes use of a package
# which has compiled all of the Harry Potter books into a text file
# that you can access as a package  
# https://github.com/bradleyboehmke/harrypotter
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)

# harrypotter::chamber_of_secrets

book <- Corpus(VectorSource(philosophers_stone))
book[[1]]$meta
book[[1]]$content

######################################################
# transform and preprocess text
######################################################

getTransformations()

trumpCorpus <- tm_map(trumpCorpus, content_transformer(tolower))
trumpCorpus[[1]]$content

trumpCorpus <- tm_map(trumpCorpus, removeWords, stopwords("english"))
trumpCorpus[[1]]$content

trumpCorpus <- tm_map(trumpCorpus, content_transformer(gsub), pattern="\n\n", replacement=" ")
trumpCorpus[[1]]$content

# book <- tm_map(book, content_transformer(tolower))


######################################################
# Representing text with the document-term matrix
######################################################

trumpDTM <- DocumentTermMatrix(trumpCorpus)
trumpDTM

inspect(trumpDTM)
inspect(trumpDTM[1:2, 1:2])

trumpDTMS <- removeSparseTerms(trumpDTM, 0.05) 
trumpDTMS

trumpDTMS <- removeSparseTerms(trumpDTM, 0.10) 
trumpDTMS

inspect(trumpDTMS[1:2, 1:5])

inspect(trumpDTM[, c("news", "fake", "america", "great")])

trumpFreqTerms <- findFreqTerms(trumpDTM, lowfreq=50)
trumpFreqTerms

# trumpTDM <- TermDocumentMatrix(trumpDTM)

trumpFreqTerms <- colSums(as.matrix(trumpDTM))
head(trumpFreqTerms)

sort(trumpFreqTerms, decreasing = TRUE)

install.packages("tidyverse")
library(tidyverse)

trumpFreqTermsDF <- data.frame(word=names(trumpFreqTerms), freq=trumpFreqTerms)
arrange(trumpFreqTermsDF, desc(freq))

ggplot(subset(trumpFreqTermsDF, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

# create a wordcloud to visualise frequent terms
install.packages("wordcloud")
library(wordcloud)
set.seed(142)   
wordcloud(trumpFreqTermsDF$word, trumpFreqTermsDF$freq, min.freq=25) 

wordcloud(trumpFreqTermsDF$word, trumpFreqTermsDF$freq, min.freq=50) 


######################################################
# compare frequent terms in two documents
######################################################

# look at first doc
vs = VectorSource(trumpCorpus[[1]]) 
corp <- Corpus(vs)
dtm <- DocumentTermMatrix(corp)
ft <- colSums(as.matrix(dtm))
df <- data.frame(word=names(ft), freq=ft)
head(arrange(df, desc(freq)))
set.seed(142)   
wordcloud(df$word, df$freq, min.freq=3) 


# look at final doc
vs = VectorSource(trumpCorpus[[11]]) 
corp <- Corpus(vs)
dtm <- DocumentTermMatrix(corp)
ft <- colSums(as.matrix(dtm))
df <- data.frame(word=names(ft), freq=ft)
head(arrange(df, desc(freq)))
set.seed(142)   
wordcloud(df$word, df$freq, min.freq=4) 


######################################################
# Show occurrences of particular word in documents
######################################################

df_trump <- tidy(trumpDTM)

df_trump %>%
  filter(term=="obama") %>% 
  group_by(document) %>% 
  ggplot(aes(x = document, y = count)) +
  geom_bar(stat = "identity") + 
  ggtitle("Occurrences of 'Obama' in Trump speeches") +
  labs(x="", y="Word frequency") +
  theme(axis.text.x=element_text(angle=45, hjust=1))


# find word associations
findAssocs(trumpDTM, "clinton", corlimit = 0.6)

# demonstrate hierarchical clustering
install.packages("proxy")
library(proxy)

sparsetrumpDTM <- removeSparseTerms(trumpDTM, sparse=0.95)
mat <- as.matrix(sparsetrumpDTM)
docsdissim <- dist(scale(mat))

h <- hclust(docsdissim, method = "ward.D")
plot(h, cex=0.8, hang=0.1)

# demonstrate clustering using k-means
install.packages("fpc")
install.packages("cluster")
library(fpc)   
library(cluster)

kfit <- kmeans(docsdissim, 2)   
clusplot(as.matrix(docsdissim), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

# exercise
# potterDTM <- DocumentTermMatrix(book)
# potterFreqTerms <- findFreqTerms(potterDTM, lowfreq=50)
# head(potterFreqTerms)
# findAssocs(potterDTM, "dumbledore", corlimit = 0.8)
# sort(potterFreqTerms, decreasing = TRUE)

# sparsepotterDTM <- removeSparseTerms(potterDTM, sparse=0.95)
# mat <- as.matrix(sparsepotterDTM)
# docsdissim <- dist(scale(mat))
# h <- hclust(docsdissim, method = "ward.D")
# plot(h, cex=0.8, hang=0.1)



# **************************************************************#
# Part 3: Text analysis using tidytext
# **************************************************************#

library(tidytext)
library(dplyr)
library(janeaustenr)
library(stringr)


######################################################
# Working with tidytext
######################################################

View(austen_books())
austen_books()

austen_books() %>% 
  group_by(book) %>%
  summarise(total_lines = n())

austenTidyBooks <- austen_books() %>% 
  unnest_tokens(word, text) 

austenTidyBooks

austenTidyBooks <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber=row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text)

austenTidyBooks


# Example of reading in more documents

# reading in the inaugral.txt file which is already stored 
# in a data frame - txt
txt <- readtext("http://ir.shef.ac.uk/cloughie/download/inaugral.txt") # plaintext
txt_df <- txt %>%
  unnest_tokens(word, text)

txt_df

# tidying tm corpus
trumpCorpus <- VCorpus(DirSource("./texts", encoding="UTF-8"))
trumpTxt <- tidy(trumpCorpus)
trumpTxt

trumpTxt$id
trumpTxt$text

tidyTrumpTxt <- trumpTxt %>%
  select(text, id) %>%
  group_by(id) %>% 
  unnest_tokens(word, text) %>%
  ungroup() # you can experiment with not including this when you compute count()

tidyTrumpTxt

# tidying tm DTM
trumpDTM <- DocumentTermMatrix(trumpCorpus)
tidyTrumpTDM <- tidy(trumpDTM)
tidyTrumpTDM

######################################################
# Computing word frequencies
######################################################

austenTidyBooks %>%
  count(word, sort=TRUE)

austenTidyBooks %>%
  group_by(book) %>%
  mutate(unique = n_distinct(word)) %>%
  count(book, unique)

# Remove stopwords (with anti-join)
data(stop_words)
austenTidyBooks<-austenTidyBooks %>%
  anti_join(stop_words)

austenTidyBooks %>%
  count(word, sort=TRUE)

austenTidyBooks %>%
  count(word, sort=TRUE) %>%
  filter(n>500) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# example of filtering out words
wordsToFilterOut <- c("miss", "time")

austenTidyBooks %>%
  filter(!word %in% wordsToFilterOut) %>%
  count(word, sort=TRUE)

austenTidyBooks %>% 
  filter(book=="Emma") %>%
  count(word, sort = TRUE) 

austenTidyBooks %>%
  count(book, word, sort=TRUE) %>%
  
  austenTidyBooks %>%
  group_by(book) %>%
  count(word, sort=TRUE)

# Exercise: try the same for Trump speeches
# Try the same for Trump speeches
tidyTrumpTxt <- tidyTrumpTxt %>%
  anti_join(stop_words)

# count word frequency across all documents
tidyTrumpTxt %>%
  count(word, sort=TRUE)

tidyTrumpTxt %>%
  count(id, word, sort=TRUE)


######################################################
# Computing tf-idf
######################################################

austenTFIDF<-austenTidyBooks %>%
  count(book, word, sort=TRUE) %>%
  bind_tf_idf(word, book, n) %>%
  arrange(desc(tf_idf))

austenTFIDF

austenTFIDF %>%
  group_by(book) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

# Exercise: for the Trump speeches

trumpTFIDF <- tidyTrumpTxt %>%
  count(id, word, sort=TRUE) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

trumpTFIDF

trumpTFIDF %>%
  group_by(id) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ id, scales = "free") +
  ylab("tf-idf") +
  coord_flip()


######################################################
# Computing bigrams
######################################################

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_separated

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# new bigram counts:
bigram_counts <- bigrams_united %>% 
  count(bigram, sort=TRUE)

bigram_counts


######################################################
# Visualising word n-grams with ggraph
######################################################

# KJV Bible example
install.packages("gutenbergr")
library(gutenbergr)
library(stringr)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(tidyverse)
library(tidytext)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token="ngrams", n=2) %>%
    separate(bigram, c("word1", "word2"), sep=" ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort=TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type="closed", length=unit(.08, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout="fr") +
    geom_edge_link(aes(edge_alpha=n), show.legend=FALSE, arrow=a) +
    geom_node_point(color="red", size=2) +
    geom_node_text(aes(label=name), repel=TRUE) +
    theme_void() + 
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
}

kjv <- gutenberg_download(10)
kjv

kjv_bigrams <- kjv %>%
  count_bigrams()

kjv_bigrams

kjv_bigrams %>%
  filter(n>20, 
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()


expectations <- gutenberg_download(1400)

expectations_bigrams <- expectations %>%
  count_bigrams()

expectations_bigrams %>%
  filter(n>3,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()



# **************************************************************#
# Part 3: Sentiment analysis
# **************************************************************#

######################################################
# Starting with simple dictionary-based approach
######################################################

sentiments

get_sentiments("bing")

# words in nrc lexicon by sentiment type
get_sentiments("nrc") %>%
  group_by(sentiment) %>%
  summarise(count = n())

nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)
nrc

austenTidyBooks %>%
  filter(book=="Emma") %>%
  summarise(count = n())

austenTidyBooks %>%
  filter(book=="Emma") %>%
  inner_join(get_sentiments("nrc")) %>%
  summarise(count = n())

austenTidyBooks %>%
  filter(book=="Emma") %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  summarise(count = n())

austenTidyBooks %>%
  filter(book=="Emma") %>%
  distinct(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  summarise(count = n_distinct(word))

nrcAnger <- get_sentiments("nrc") %>% 
  filter(sentiment=="anger")

austenTidyBooks %>%
  filter(book=="Emma") %>%
  inner_join(nrcAnger) %>%
  count(word, sort = TRUE)

# visualise sentiment
library(reshape2)
#library(dplyr)
#library(tidyverse)
#library(tidytext)
#library(wordcloud)

austenTidyBooks %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)

tidyTrumpTxt %>%
  inner_join(nrc, by = "word") %>%
  count(id, sentiment, word) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  summarize(words = sum(n))


tidyTrumpTxt %>%
  inner_join(get_sentiments("bing"))


# Syuzhet package example

install.packages("syuzhet")
library(syuzhet)

get_sentiment(trumpTxt$text) %>% as.numeric()

nrc_data <- get_nrc_sentiment(trumpTxt$text)

barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  col=heat.colors(8), 
  main = "Emotions in Trump's tweets", xlab="Percentage"
)


# **********************************************************
# Appendix 1 
# Example: working with the Harry Potter books
# Sources: 
# https://rpubs.com/tsholliger/301914
# https://uc-r.github.io/sentiment_analysis 
#***********************************************************

# re-structure the data to make it easier to work with
titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series <- tibble()

for(i in seq_along(titles)) {
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))

book_words <- series %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

series_words <- book_words %>%
  group_by(book) %>%
  summarise(total = sum(n))

book_words <- left_join(book_words, series_words)

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))),
         book = factor(book, levels = titles)) %>% 
  group_by(book) %>%
  top_n(15, wt = tf_idf) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Highest tf-idf words in the Harry Potter series",
       x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# conduct sentiment analysis
series %>%
  group_by(book) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         book = factor(book, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free_x")


# **********************************************************
# Appendix 2 
# Example: working with the tweets from Trump
#***********************************************************

library(stringr)

load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- trump_tweets_df %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]")) %>%
  select(word)

tweet_words

tweet_words %>%
  count(word, sort = TRUE)


# **********************************************************
# Appendix 3
# Example: work on analysing a specific text - Great Expectations
#***********************************************************

expectations <- gutenberg_download(1400)

tidy_expectations <- expectations %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# simple word count
tidy_expectations %>%
  count(word, sort=TRUE)

library(plotly)
word_count <- 
  tidy_expectations %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 
word_count %>%
  head(10) %>% 
  plot_ly(x=~n, y=~word) %>% 
  layout(title="Most common words (excluding stop-words) in Great Expectations",
         xaxis=list(title="Total Occurrences"),
         yaxis=list(title="")
  )  %>%
  config(displayModeBar = F, showLink = F)

tidy_expectations %>%
  filter(word=="expectations") %>% 
  group_by(chapter) %>% 
  count(word) %>% 
  plot_ly(x=~chapter,y=~n) %>% 
  add_bars(color=I("blue"), alpha=0.5) %>% 
  layout(title="Occurrences of word 'Expectations' by Chapter",
         yaxis=list(title="Occurrences"),
         yaxis=list(title="Chapter")
  )  %>%
  config(displayModeBar = F, showLink = F)
