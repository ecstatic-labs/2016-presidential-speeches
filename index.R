# This file provides methods to perform tf-idf on 2016 presidential
# campaign speeches and to generate word cloud

# Set working directory to this file location - may need to change based on your file location
setwd("~/Documents/Blog Posts/Text Mining Presidential Campaign Speeches")
library(tm)
library(wordcloud)

#############
# Functions #
#############

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"), 'applause', 'trump', 'audience', 'chafee', 'rubio'))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
}

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

# TODO : KLB - export as JSON
export_to_json <- function(){
}

export_to_csv <- function(tdm_freq, tdm_tf_idf, candidate_txt_file){
  word_names <- c(names(head(tdm_tf_idf[order(tdm_tf_idf[, candidate_txt_file], decreasing = TRUE), candidate_txt_file], 100)))
  freq_csv_file_name <- paste(gsub('.txt', '', candidate_txt_file), '_freq.csv', sep='')
  tf_idf_csv_file_name <- paste(gsub('.txt', '', candidate_txt_file), '_tf_idf.csv', sep='')
  word_frequencies <- as.table(inspect(tdm_freq[word_names, candidate_txt_file]))
  word_weights <- as.table(head(tdm_tf_idf[order(tdm_tf_idf[, candidate_txt_file], decreasing = TRUE), candidate_txt_file], 100))
  write.csv(word_frequencies, freq_csv_file_name)
  write.csv(word_weights, tf_idf_csv_file_name)
}

##############################################
# Read speeches from directories into Corpus #
##############################################

democrat_txts <- VCorpus(DirSource("speeches/democrat/", encoding = "UTF-8"), readerControl = list(language = "en"))
republican_txts <- VCorpus(DirSource("speeches/republican/", encoding = "UTF-8"), readerControl = list(language = "en"))
all_txts <- c(democrat_txts, republican_txts)

###################################
# Separate out for Party vs Party #
###################################

all_dem <- c(
  as.String(democrat_txts[['bernie_sanders.txt']][['content']]),
  as.String(democrat_txts[['hillary_clinton.txt']][['content']]),
  as.String(democrat_txts[['jim_webb.txt']][['content']]),
  as.String(democrat_txts[['lincoln_chafee.txt']][['content']]),
  as.String(democrat_txts[['martin_omalley.txt']][['content']])
)

all_rep <- c(
  as.String(republican_txts[['ben_carson.txt']][['content']]),
  as.String(republican_txts[['bobby_jindal.txt']][['content']]),
  as.String(republican_txts[['carli_fiorini.txt']][['content']]),
  as.String(republican_txts[['chris_cristie.txt']][['content']]),
  as.String( republican_txts[['donald_trump.txt']][['content']]),
  as.String( republican_txts[['george_pataki.txt']][['content']]),
  as.String( republican_txts[['jeb_bush.txt']][['content']]),
  as.String( republican_txts[['john_kasich.txt']][['content']]),
  as.String( republican_txts[['lindsey_graham.txt']][['content']]),
  as.String( republican_txts[['marco_rubio.txt']][['content']]),
  as.String( republican_txts[['mike_huckabee.txt']][['content']]),
  as.String(republican_txts[['rand_paul.txt']][['content']]),
  as.String(republican_txts[['rick_perry.txt']][['content']]),
  as.String(republican_txts[['scott_walker.txt']][['content']]),
  as.String(republican_txts[['ted_cruz.txt']][['content']])
)

democrat_corpus <- Corpus(VectorSource(as.String(all_dem)))
republican_corpus <- Corpus(VectorSource(as.String(all_rep)))
party_vs_party_corpus <- c(democrat_corpus, republican_corpus)

######################################################
# Clean Corpus, Get Word Frequencies, TF-IDF Weights #
######################################################

party_vs_party_corpus <- clean_corpus(party_vs_party_corpus)
party_vs_party_tf_idf <- TermDocumentMatrix(party_vs_party_corpus, control = list(weighting = weightTfIdf))
party_vs_party_freq <- TermDocumentMatrix(party_vs_party_corpus)
colnames(party_vs_party_freq)[1] <- 'democrats'
colnames(party_vs_party_freq)[2] <- 'republicans'
colnames(party_vs_party_tf_idf)[1] <- 'democrats'
colnames(party_vs_party_tf_idf)[2] <- 'republicans'

democrat_corpus <- clean_corpus(all_dem)
democrat_tf_idf <- TermDocumentMatrix(democrat_corpus, control = list(weighting = weightTfIdf))
democrat_freq <- TermDocumentMatrix(democrat_corpus)

republican_corpus <- clean_corpus(republican_txts)
republican_tf_idf <- TermDocumentMatrix(republican_corpus, control = list(weighting = weightTfIdf))
republican_freq <- TermDocumentMatrix(republican_corpus)

all_corpus <- clean_corpus(all_txts)
all_tf_idf <- TermDocumentMatrix(all_corpus, control = list(weighting = weightTfIdf))
all_freq <- TermDocumentMatrix(all_corpus)

# TODO: KLB - combine weight and frequency columns into one table and export that single table to csv / json

######################################
# Export to CSV / JSON for D3.js App #
######################################

export_to_csv(all_freq, inspect(all_tf_idf), 'donald_trump.txt')
export_to_csv(party_vs_party_freq, inspect(party_vs_party_tf_idf), 'democrats')
export_to_csv(party_vs_party_freq, inspect(party_vs_party_tf_idf), 'republicans')
          
#####################################
# Word cloud in R - not really used #
#####################################

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

generate_word_cloud(iall_tdm[,'donald_trump.txt'], colnames(iall_tdm)['donald_trump.txt'])

#######################################
# Some work for N-Grams - may not use #
#######################################

# The following is if want to tokenize words into ngrams

library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
all_tdm_ngram <- clean_corpus_dtm_ngram(all_txts, BigramTokenizer)
iall_tdm_ngram = inspect(all_tdm_ngram)
head(iall_tdm_ngram[order(iall_tdm_ngram[,12], decreasing = TRUE),], 20)
generate_word_cloud(iall_tdm_ngram[,3], colnames(iall_tdm_ngram)[3])
#options(mc.cores=1) # might be a needed option
colnames(iall_tdm_ngram)
