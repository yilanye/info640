#to import the books from project gutenberg directly 
install.packages("gutenbergr")
#to create and work with corpora 
install.packages("tm")
#for LDA topic models 
install.packages("topicmodels")
install.packages("textdata")

library(tidytext)
library(tm)
library(stringr)
library(topicmodels)
library(gutenbergr)
library(tidyverse) 
library(dplyr) 
library(data.table)

text <- "Because I could not stop for       Death - 
  He kindly stopped for me -
  The Carriage held but just Ourselves - 
  and Immortality"
text

text1<-tolower(text)
removePunctuation(text1)
stripWhitespace(text1)
text1<-stripWhitespace(text1)
text1

stopwords("en")

removeWords(text1, stopwords("en"))

my_stops <- c(stopwords("en"), "death")
removeWords(text1, my_stops)

new_text<-removeWords(text1, my_stops)

new_text

nvec <- unlist(strsplit(new_text, split=" ")) 
nvec
stem_text <- stemDocument(nvec)
print(stem_text)
completed_text <- stemCompletion(stem_text,nvec) 
completed_text

 #Structured Text
peace_res <- read_csv(file = "Dropbox/2 Info 640 Data Analysis/info640Michelle/DataSets/pax_20_02_2018_1_CSV.csv")

str(peace_res) 
glimpse(peace_res)

names(peace_res)[names(peace_res)=="AgtId"] <- "doc_id" 
peace_res$doc_id <- as.character(peace_res$doc_id) 
names(peace_res)[names(peace_res)=="OthAgr"] <- "text" 
colnames(peace_res)

peace_source <- DataframeSource(peace_res) 
peace_corpus <- VCorpus(peace_source)
peace_corpus

peace_corpus[[10]]
peace_corpus[[10]][1]

peace_cleaned <- tm_map(peace_corpus, removeNumbers) 
peace_cleaned[[10]][1]


peace_cleaned <- tm_map(peace_corpus, removePunctuation)
peace_cleaned<-tm_map(peace_cleaned,stripWhitespace)
peace_cleaned<-tm_map(peace_cleaned,content_transformer(tolower))
pc_stops = c(stopwords("en"),"peace","agreement", "shall", "government", "page", "parties")
peace_cleaned= tm_map(peace_cleaned,removeWords,pc_stops)
peace_cleaned[[5]]


#Topic Modeling
peace_dtm <- DocumentTermMatrix(peace_cleaned)
peace_dtm

unique_indexes <- unique(peace_dtm$i)
peace_dtm <- peace_dtm[unique_indexes,] 
peace_dtm

peace_dtm_tidy <- tidy(peace_dtm)
peace_dtm_tidy


 k <- 6
peace_lda <- LDA(peace_dtm, k = k, control = list(seed=1234)) 
peace_lda
 
peace_lda_topics <-as.matrix(peace_lda_words)
write.csv(peace_lda_topics,file=paste("Dropbox/2 Info 640 Data Analysis/info640Michelle/",k,".csv"))
head(peace_lda_topics) 

peace_lda_tidy <- tidy(peace_lda) 
head(peace_lda_tidy)

top_terms <- peace_lda_tidy %>% 
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) 
top_terms


top_terms %>%
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill=factor(topic))) + geom_col(show.legend=FALSE) +
  facet_wrap(~ topic, scales = "free")+ coord_flip()

get_LDA_topics_terms_by_topic <- function (input_corpus, plot = TRUE, number_of_topics = 6,number_of_words=5,path="Dropbox/2 Info 640 Data Analysis/info640Michelle/DataSets/" )
{my_dtm <- DocumentTermMatrix(input_corpus)
unique_indexes <- unique(my_dtm$i) 
my_dtm <- my_dtm[unique_indexes,]
my_lda <- LDA(my_dtm, k = number_of_topics, control = list(seed=1234)) 
my_topics <- tidy(my_lda, matrix="beta")
my_lda_words <- terms(my_lda, number_of_words) 
my_lda_topics <- as.matrix(my_lda_words)
write.csv(my_lda_topics,file=paste(path,k ,".csv"))
my_top_terms <- my_topics %>% 
  group_by(topic) %>% 
  top_n(number_of_words, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)
if(plot==TRUE){ my_top_terms %>%
    mutate(term = reorder(term, beta)) %>% 
    ggplot(aes(term, beta, fill=factor(topic))) + 
    geom_col(show.legend=FALSE) +
    facet_wrap(~ topic, scales = "free")+ 
    coord_flip()
}else{ return(my_top_terms) }}
  

get_LDA_topics_terms_by_topic(peace_cleaned)


get_LDA_topics_terms_by_topic(peace_cleaned, number_of_topics = 4, number_of_words = 4)

peace_lda_document_topics <- tidy(peace_lda, matrix="gamma") 
peace_lda_document_topics
write.csv(peace_lda_document_topics,file=paste("Dropbox/2 Info 640 Data Analysis/info640Michelle/Labs/",k,".csv"))

head(peace_lda_document_topics) 
dim(peace_lda_document_topics)

peace_lda_document <- spread(peace_lda_document_topics, topic, gamma) 
dim(peace_lda_document)
head(peace_lda_document)

peace_lda_document$max_topic <- colnames(peace_lda_document[2:7])[apply(peace_lda_document,1,which.max)]
head(peace_lda_document)

dt1 <- data.table(peace_lda_document, key = "document") 
dt2 <- data.table(peace_res, key = "doc_id")
peace_merged <- dt1[dt2] 
dim(peace_merged) 
colnames(peace_merged)



#Unstructured Text

dq <- gutenberg_download(996) 
dq
dq_source <- VectorSource(dq) 
dq_corpus <- VCorpus(dq_source)

dq_corpus[[1]][2]

#Sentiment Analysis
sentiments
get_sentiments("afinn") 
get_sentiments("nrc") 
get_sentiments("bing")

lyrics_raw <- read.csv("Dropbox/2 Info 640 Data Analysis/info640Michelle/DataSets/songdata.csv",encoding = "utf-8", header = TRUE,stringsAsFactors = FALSE)
summary(lyrics_raw)

tidy_lyrics <- lyrics_raw %>% ungroup() %>% unnest_tokens(word, text)
summary(tidy_lyrics) 
head(tidy_lyrics)

nrc_sent <- get_sentiments("nrc") %>% filter(sentiment == "joy")

tidy_lyrics_bowie <- tidy_lyrics %>% filter(artist == "David Bowie")

tidy_lyrics_bowie %>%inner_join(nrc_sent) %>% 
  dplyr::count(word, sort = TRUE)

bowie_sentiment <- tidy_lyrics_bowie %>% 
  inner_join(get_sentiments("bing")) %>% 
  dplyr::count(song, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
head(bowie_sentiment)


ggplot(bowie_sentiment, aes(negative, positive, color = song)) + geom_jitter(show.legend = FALSE)

bowie_career_sentiment <- mean(bowie_sentiment$sentiment) 
bowie_career_sentiment


unique(lyrics_raw$artist)

#"Whiskeytown"
tidy_lyrics_wht <- tidy_lyrics %>% filter(artist == "Whiskeytown" )

tidy_lyrics_wht %>%inner_join(nrc_sent) %>% 
  dplyr::count(word, sort = TRUE)   


wht_sentiment <- tidy_lyrics_wht %>% 
  inner_join(get_sentiments("bing")) %>% 
  dplyr::count(song, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
head(wht_sentiment)


ggplot(wht_sentiment, aes(negative, positive, color = song)) + geom_jitter(show.legend = FALSE)

wht_career_sentiment <- mean(wht_sentiment$sentiment) 
wht_career_sentiment

#"Noa"
tidy_lyrics_noa <- tidy_lyrics %>% filter(artist == "Noa")

tidy_lyrics_noa %>%inner_join(nrc_sent) %>% 
  dplyr::count(word, sort = TRUE)   


noa_sentiment <- tidy_lyrics_noa %>% 
  inner_join(get_sentiments("bing")) %>% 
  dplyr::count(song, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
head(noa_sentiment)


ggplot(noa_sentiment, aes(negative, positive, color = song)) + geom_jitter(show.legend = FALSE)
noa_career_sentiment <- mean(noa_sentiment$sentiment) 
noa_career_sentiment

#Noa is more positive
                                        