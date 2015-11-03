# This file provides methods to perform tf-idf on 2016 presidential campaign speeches and to
# generate word cloud

# TODO: KLB - Export word tf-idf weights and word frequencies for
## Candidate vs all
## Party vs Party

# Set working directory to this file location
setwd("~/Documents/Blog Posts/Text Mining Presidential Campaign Speeches")
library(tm)
library(wordcloud)

clean_corpus_dtm <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"), 'applause', 'trump', 'audience', 'chafee', 'rubio'))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
}

clean_corpus_dtm_ngram <- function(corpus, tokenizer){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"), 'applause', 'trump', 'audience', 'chafee', 'rubio'))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  TermDocumentMatrix(corpus, control = list(tokenize = tokenizer))
}

generate_word_cloud <- function(matrix, title){
  # Commented out portions are to add a title to wordcloud
  m = as.matrix(matrix)
  v = sort(rowSums(m),decreasing=TRUE)
  d = data.frame(word = names(v),freq=v)
  d_min = head(d, 100)
  
  #layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  #par(mar=rep(0, 4))
  #plot.new()
  #text(x=0.5, y=0.5, title)
  pal = brewer.pal(9,"BuPu")
  wordcloud(words = d_min$word,
            freq = d_min$freq,
            scale = c(3,.8),
            random.order = F,
            colors = pal,
            #main = "Title"
            )
}

democrat_txts <- VCorpus(DirSource("speeches/democrat/", encoding = "UTF-8"), readerControl = list(language = "en"))
republican_txts <- VCorpus(DirSource("speeches/republican/", encoding = "UTF-8"), readerControl = list(language = "en"))
all_txts <- c(democrat_txts, republican_txts)

democrat_tdm <- clean_corpus_dtm(democrat_txts)
# to matrix
idemocrat_tdm = inspect(democrat_tdm)

# Just exploring the data
head(idemocrat_tdm[order(idemocrat_tdm[,'bernie_sanders.txt'], decreasing = TRUE),], 20)

all_tdm <- clean_corpus_dtm(all_txts)
iall_tdm = inspect(all_tdm)
head(iall_tdm[order(iall_tdm[,'donald_trump.txt'], decreasing = TRUE),], 20)
generate_word_cloud(iall_tdm[,'donald_trump.txt'], colnames(iall_tdm)['donald_trump.txt'])

tdm <- TermDocumentMatrix(txts, control = list(weighting = weightTfIdf))
itdm = inspect(tdm)
head(itdm[order(itdm[,4], decreasing = TRUE),], 20)

# The following is if want to tokenize words into ngrams

library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
all_tdm_ngram <- clean_corpus_dtm_ngram(all_txts, BigramTokenizer)
iall_tdm_ngram = inspect(all_tdm_ngram)
head(iall_tdm_ngram[order(iall_tdm_ngram[,12], decreasing = TRUE),], 20)
generate_word_cloud(iall_tdm_ngram[,3], colnames(iall_tdm_ngram)[3])
#options(mc.cores=1) # might be a needed option
colnames(iall_tdm_ngram)
