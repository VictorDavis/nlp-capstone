
# Dependencies
library(R.utils)

# download, unzip, & create train/test datasets
download <- function(fname) {

  # Corpus we'll be using
  myzip <- "Coursera-SwiftKey.zip"
  myurl <- paste0("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/",myzip)
  
  # Download zip file from source
  if (!file.exists(myzip)) {
    download.file(myurl, myzip, method = "curl")
  }
  
  # unzip english data only
  if (!file.exists(fname)) {
    unzip(myzip, files = c(fname))
  }
  
  # partition corpus into training/testing 80/20
  if (!file.exists(training_file)) {
    training_pct = .1000
    testing_pct = .0010
      
    # loop through file
    conn <- file(fname, "rt")
    training_conn <- file(training_file, "w")
    testing_conn <- file(testing_file, "w")
    while (length(line <- readLines(conn, n = 1, encoding = "UTF-8")) > 0) {
      r <- runif(1, min = 0, max = 1)
      if (r < training_pct) { # accept
        writeLines(line, training_conn, useBytes = T)
      } else if (r < testing_pct + training_pct) {
        writeLines(line, testing_conn, useBytes = T)
      }
    }
    close(conn)
    close(training_conn)
    close(testing_conn)
  }
}

# string reverse
strrev <- function(str) {
    paste(rev(strsplit(str, NULL)[[1]]), collapse = "")
}

# handles special characters (order matters!)
# Note: this seems to cover everything "in limits" [A-Za-z0-9 éÍöşıñ[:punct:]]
scrub <- function(text) {
  text <- gsub("[‘’\u2019`]","'",text)
  text <- gsub("'s","s",text)
  text <- gsub("&"," and ", text)
  text <- gsub("@"," and ", text)
  text <- gsub("#"," number ", text)
  text <- gsub("[0-9]+/[0-9]+/[0-9]+","<date>", text)
  text <- gsub("[0-9]*(\\.[0-9]+)?%","<percent>", text)
  text <- gsub("[\\$£][0-9,]*([\\.][0-9]+)?(¢)?([Kk])?","<money>", text)
  text <- gsub("[1-9]([0-9]){3}((')?s)?","<year>", text)
  text <- gsub("[1-9]0(')?s","<year>", text)
  text <- gsub("[0-9]+(rd|st|nd|th)","<ordinal>", text)
  text <- gsub("[0-9,]*\\.[0-9]+","<decimal>", text)
  text <- gsub("[0-9]+/[0-9]+","<fraction>", text)
  text <- gsub("^[0-9,]+[Kk]?","<number>", text)
  text <- gsub("([^A-Za-z0-9<>]|x)[0-9]+(,[0-9]+)*[Kk]?","\\1<number>", text)
  text <- gsub("[“”″]","\"",text)
  text <- gsub("[––\u2013]","-",text)
  text <- gsub("<number> ?- ?<number>","<range>", text)
  text <- gsub("-"," ",text) # replace all hyphens with space "duck-like" 2 wds
  text <- gsub("/"," ",text) # replace all slashes with space "and/or" 2 wds
  text <- gsub("[…]","\\.",text)
  text <- gsub("<([a-z]+)>","X\\1X",text)
  text <- gsub("[[:punct:]]", "", text) # remove all punctuation EXCEPT <>
  text <- gsub("X([a-z]+)X","<\\1>",text)
  text <- tolower(text)
  text
}

# run interactive to test scrub
if (F) {
  conn <- file(training_file, "rt")
  training <- readLines(conn, encoding = "UTF-8")
  close(conn)
  names(training) <- training
  suspects <- training[grep("[^[:print:]]", training, perl=T)]
  scrub(sample(suspects,1))
}

# tokenizer function
tokenize <- function(text) {
  text <- scrub(text)
  words <- strsplit(text, split = " ")[[1]] # words are separated by spaces
  words <- words[nchar(words) > 0] # words are non-empty
  words
}

sentence <- function(mylist) {
  para <- paste(mylist, collapse = "! ")
  para <- gsub("Mr\\.", "Mr",  para)
  para <- gsub("Dr\\.", "Dr",  para)
  para <- gsub("Ms\\.", "Ms",  para)
  para <- gsub("Mrs\\.", "Mrs",para)
  para <- gsub("St\\.", "St",  para)
  para <- gsub("p\\.", "p",  para)
  para <- gsub("pp\\.", "pp",  para)
  para <- gsub("([A-Z])\\.", "\\1",  para) # initials "Booker T. Washington"
  para <- gsub("([\\.\\?\\!])", "\\1 ",  para) # pad with a space
  sent <- strsplit(para, split = "[\\.\\?\\!]")[[1]]
  
  if (length(sent) == 0)
    sent <- "<stop>"
  
  sent <- sapply(sent, trim)
  sent
}

# dumbs down complex text into stream of words
getStream <- function() {
  word_file = "data/stream.dat"
  if (!file.exists(word_file)) {
    
    conn <- file(training_file, "rt")
    stream_conn <- file(word_file, "w")
    while (length(line <- readLines(conn, n = 1, encoding = "UTF-8")) > 0) {
      # writeLines(line, stream_conn, useBytes = T)
      lines <- sentence(line) # transform from "lines" to "sentences"
      for (line in lines) {
        words <- tokenize(line) # tokenize sentences into words
        words <- words[nchar(trim(words)) > 0] # remove empties
        line <- paste(words, collapse=" ")
        if (nchar(trim(line)) > 0) {
          writeLines(line, stream_conn, useBytes = T)
        }
      }
    }
    close(conn)
    close(stream_conn)
  }
  
  stream <- readLines(word_file, encoding = "UTF-8")
  stream <- sapply(stream, function(x) strsplit(x, split = " ")[[1]])
  stream
}

# converts structured stream into unstructured stream (list into vector)
getWords <- function() {
  words <- g_stream
  words <- sapply(words, function(x) c(x, "<stop>")) # punctuate each line
  words <- unlist(words)
  names(words) <- c()
  df <- data.frame(word = words, stringsAsFactors = F)
  df$seq <- 1:nrow(df)
  df <- merge(df, g_dict[,c("word","wid")], all.x = T, stringsAsFactors = F)
  df[is.na(df$wid),"word"] <- "<unk>"
  df <- df[order(df$seq),]
  words <- df$word
  words
}

# creates an alphabetical word-frequency (~4k word) dictionary, assigning word ids
# note: argument only used if file does not already exist
getDict <- function() {
  dict_file <- "data/dict.dat"
  if (!file.exists(dict_file)) {
    words <- unlist(g_stream)
    tbl <- table(words)
    tbl <- c("<stop>" = length(g_stream),"<unk>" = length(tbl[tbl <= 1]), tbl)
    tbl <- tbl[tbl > 1] # repeats only?
    tbl <- data.frame(tbl)
    names(tbl) <- c("freq")
    tbl$word <- row.names(tbl)
    tbl$wid <- 1:nrow(tbl)
    tbl <- tbl[,c(3,2,1)]
    # before calculating frequency, omit singly-occurring words as non-words
    tbl$prob <- tbl$freq / sum(tbl$freq) # freq > 1 for all records at this point
    write.table(tbl, dict_file, row.names = F, quote = T)
  }
  
  dict <- read.table(dict_file, header = T)
  dict
}

# returns word id = f(word)
getWID <- function(word) {
  ret <- g_dict[g_dict$word == word,"wid"]
  
  # if word is not found, return wid for "unknown word"
  if (length(ret) == 0)
    ret = g_dict[g_dict$word == "<unk>","wid"]
  
  ret
}

# returns word = f(id)
getWord<- function(wid) {
  as.character(g_dict$word[wid])
}

# transforms wordstring to widstring
widify <- function(words) {
  df <- data.frame(seq = 1:length(words), word = words)
  df <- merge(df, g_dict)
  df <- df[order(df$seq),]
  wids <- df$wid
  wids
}

# transforms widstring into wordstring
wordify <- function(wids) {
  as.character(g_dict[wids,"word"])
}

# writes ngrams to disk as numeric data using dictionary word ids
getNGrams <- function(n = 2, text = T) {
  if (n < 2) g_dict[order(g_dict$freq, decreasing=T),]
  else {
    ngram_file <- paste0("data/ngram", n, ".dat")
    if (!file.exists(ngram_file)) {
      
      wids <- g_widstream
      len <- length(wids)
      ngram <- data.frame(wid = wids)
      for (i in 1:(n-1)) {
        wids <- c(0, wids[1:(len-1)])
        cname <- paste0("pre",i)
        ngram[,cname] <- wids
      }
      
      ngram <- ngram[ngram[,n] > 0,] # above process creates n bogus records at the bottom
      w <- getWID("<unk>") # do not predict "unknown word" <unk>
      ngram <- ngram[ngram[,n] != w,]
      ngram$freq <- 1
      
      # predict first word ONLY in 2-grams (note, sentence end can be predicted for any n)
      # "[I am a hat. You are a r]abbit" is not predictive b/c of the sentence break!
      w <- getWID("<stop>")
      if (n > 2) {
        for (i in 2:(n-1)) {
          ngram <- ngram[ngram[,i] != w,]
        }
      }
      
      # aggregate ngram counts
      ngram <- aggregate(freq ~ ., ngram, sum)
      # drop orphans
      ngram <- ngram[ngram$freq > 1,]
      sgram <- ngram[,-1]
      sgram <- aggregate(freq ~ ., sgram, sum)
      names(sgram)[n] <- "total"
      ngram <- merge(ngram, sgram)
      ngram$prob <- ngram$freq / ngram$total
      
      # order most to least frequent
      ngram <- ngram[order(ngram$freq, decreasing = T),]
      write.table(ngram, ngram_file, row.names = F)
    }
  
    ngram <- read.table(ngram_file, header = T)
    if (text) {
      for (i in n:1) {
        cname <- paste0("word",i)
        ngram[,cname] <- wordify(ngram[,i])
      }
    }
    ngram
  }
}

# calculate kneser-ney probability for unigrams
smooth <- function(model) {
  ng1 <- model[[1]]
  ng2 <- model[[2]]
  kn <- data.frame(table(ng2$pre1))
  names(kn) <- c("wid","kn")
  kn$kn <- kn$kn / sum(kn$kn)
  ng1 <- merge(ng1, kn, all.x = T)
  ng1[is.na(ng1$kn),"kn"] <- 0
  # ng1$prob <- ng1$kn # use kn instead of straight freq
  ng1 <- ng1[order(ng1$freq, decreasing = T),]
  model[[1]] <- ng1
  model
}

# returns min,max wid corresponding to words starting with passed n letters
# f("r") = 300:400, f("") = all, f("dkdkdkdkd") = none
getWidRange <- function(pattern = "") {
    lst <- grep(paste0("^",pattern,"(.*)"), g_dict$word)
    len <- length(lst)
    if (len > 0) {
        minwid <- lst[1]
        maxwid <- lst[length(lst)]
        wr <- c(minwid, maxwid)
    } else {
        wr <- c(0,0) # no match
    }
    wr
}

# yes/no word is a special tag <date> <money> <year> etc
special <- function(wid) {
  grepl("<", getWord(wid))
}

# match ngram based on tables
matchNGram <- function(wids, wr, k=3) {
  
  # ignore words more than 8 back
  maxlen <- length(NLM) - 1
  if (length(wids) > maxlen) {
    wids <- rev(rev(wids)[1:maxlen])
  }
  n <- length(wids)
  ngrams <- NLM[[n+1]]
  
  # autocomplete pattern
  match <- T
  match <- match & ngrams$wid >= wr[1]
  match <- match & ngrams$wid <= wr[2]
  
  # match all preceding words
  if (n > 0) {
    for (i in 1:n) {
      match <- match & (ngrams[,i] == wids[i])
    }
  }
  pred <- ngrams[match,c("wid","prob")]
  
  # log which ngram level returned the result
  pred$n <- rep(n, nrow(pred))
  
  # return matches (word, probability, level)
  names(pred) <- c("wid","prob","n")
  pred
}

# pre-digestR phrase for prediction
digestR <- function(phrase) {
  
  # is last character space, punct, or char?
  last <- substr(strrev(phrase),1,1)
  
  # if multiple sentences, only look at last sentence
  phrase <- rev(sentence(phrase))[1]
  
  # tokenize
  tokens <- tokenize(phrase)
  
  # for empty string, start at beginning of sentence!
  tokens <- c("<stop>", tokens)
  
  # 8 max
  len <- length(NLM)
  if (length(tokens) > len) {
    tokens <- rev(rev(tokens)[1:len])
  }
  
  # trailing space: "One of the "
  pattern <- ""
  if (!(tolower(last) %in% letters)) {
  } else if (length(tokens) > 0) {
    # midword: "One of the r"
    tokens <- rev(tokens)
    pattern <- tokens[1]
    tokens <- rev(tokens[-1])
  }
  
  # convert text to ids
  wids <- widify(tokens)
  wr <- getWidRange(pattern)
  
  # return "digestRed" phrase
  list(wids, wr)
}

# R&D on predictions
tablePredictions <- function(phrase) {
  
  # digestR phrase
  d <- digestR(phrase)
  wids <- d[[1]]
  wr <- d[[2]]
  
  # match every n-gram level and interpolate
  n <- length(wids)
  matches <- lapply(1:n, function(x) matchNGram(wids[x:n], wr, k))
  matches[[length(matches)+1]] <- matchNGram(c(), wr, k)
  matches <- do.call(rbind, matches)
  
  # interpolate over n
  # pred <- aggregate(prob ~ wid, matches, sum)
  # pred$prob <- pred$prob / sum(pred$prob) # normalize
  # pred <- pred[order(pred$prob, decreasing = T),]
  matches$word <- sapply(matches$wid, wordify)
  matches
}

# predict/autocomplete next word
predictNextWord <- function(phrase, k=3) {
  
  # digestR phrase
  d <- digestR(phrase)
  wids <- d[[1]]
  wr <- d[[2]]
  # match n-grams from highest to lowest until k predictions found
  n <- length(wids)
  pred <- c()
  while (length(pred) < k) {
    matches <- matchNGram(wids, wr, k)
    pred <- c(pred, matches$wid)
    
    pred <- unique(pred) # no duplicates
    pred <- pred[pred != getWID("<unk>")] # don't predict "unknown word"
    pred <- pred[pred > getWidRange("<")[2]] # don't predict tags at all
    
    if (length(wids) > 0) {
      wids <- wids[-1]
    } else {
      break
    }
  }
  
  # report k answers only
  if (length(pred) > k) {
    pred <- pred[1:k]
  }
  
  # convert ids to text
  if (length(pred) > 0) {
    pred <- wordify(pred)
  } else {
    pred <- NA
  }
  pred
}

# creates test case (phrase -> word) out of a phrase
# ex: c("once upon a t", "time")
extractPhrase <- function(text) {
  whole <- F
  tox <- tokenize(text)
  if (whole) {
    n <- sample(which(strsplit(text, split = "")[[1]] == " "), 1)
  } else {
    n <- sample(1:nchar(text),1)
  }
  phrase <- substr(text, 1, n)
  lastchar <- substr(strrev(phrase),0,1)
  pax <- tokenize(phrase)
  if (lastchar == " ") {
    word <- tox[length(pax) + 1]
  } else {
    word <- tox[length(pax)]
  }
  c(phrase, word)
}

# sanity check on quizzes
quiz <- function(fname) {
  data <- readLines(fname)
  data <- lapply(data, function(x) strsplit(x, split="\\[")[[1]])
  data <- do.call(rbind, data)
  data <- data.frame(data, stringsAsFactors = F)
  names(data) <- c("phrase", "word")
  data$word <- gsub("\\]", "", data$word)
  data$prediction <- sapply(data$phrase, function(x) predictNextWord(x, 1))
  data
}

# test algorithm on training or testing file
benchmarkR <- function(fname) {
  set.seed(91286)
  file <- readLines(fname)
  file <- sample(file[grep(" ", file)], 100)
  data <- lapply(file, extractPhrase)
  data <- data.frame(do.call(rbind, data))
  names(data) <- c("phrase", "word")
  data <- data[!is.na(data$word),] # throw out crap results
  data$phrase <- as.character(data$phrase)
  data$word <- as.character(data$word)
  pred1 <- c()
  pred2 <- c()
  pred3 <- c()
  match <- c()
  easy <- c()
  time <- c()
  for (i in 1:nrow(data)) {
    ptm <- proc.time()
    p <- unlist(predictNextWord(data[i,"phrase"], 3))
    time <- c(time, (proc.time() - ptm)[3])
    pred1 <- c(pred1, p[1])
    pred2 <- c(pred2, p[2])
    pred3 <- c(pred3, p[3])
    match <- c(match, !is.na(p[1]) & p[1] == data[i,"word"])
    easy <- c(easy, !is.na(p[1]) & data[i,"word"] %in% p)
    print(c(sum(match)/i, sum(easy)/i, mean(time)))
  }
  data$pred1 <- pred1
  data$pred2 <- pred2
  data$pred3 <- pred3
  data$match <- match
  data$easy  <- easy
  data$time <- time
  write.csv(data, "data/benchmarkR.csv", row.names = F, fileEncoding="utf-8")
}

# analyze/visualize benchmarkR results
analyze <- function() {
  results <- read.csv("data/benchmarkR.csv")
  results$pattern_len <- sapply(results$phrase, function(x) nchar(rev(tokenize(paste0(x, "x")))[1])-1)
  results$phrase_len <- sapply(results$phrase, function(x) min(c(10,length(tokenize(x)))))
  plot(aggregate(match ~ pattern_len, results, mean), main = "Accuracy by # Letters", xlab = "# of Letters", ylab = "Accuracy %")
  plot(aggregate(match ~ phrase_len, results, mean), main = "Accuracy by # Words", xlab = "# of Words", ylab = "Accuracy %")
  lm(match ~ 0 + pattern_len + phrase_len, results)
}

# GLOBAL
training_file <- "data/training.txt"
testing_file <- "data/testing.txt"
quiz2_file <- "quiz2.txt"
blogsR  <- "final/en_US/en_US.blogs.txt"
news   <- "final/en_US/en_US.news.txt"
twitter<- "final/en_US/en_US.twitter.txt"

# load ngram files into RAM
interactive <- F # F for shiny app
g_encoding <- "UTF-16LE" # "UTF-16LE" for windows, "native.enc" for linux
if (interactive) {
  download(blogsR)
  g_stream <- getStream()
  g_dict <- getDict()
  g_words <- getWords()
  g_widstream <- widify(g_words)
}
g_dict <- getDict()

text <- F # include text, or numeric only to save on RAM?
NLM <- list()
for (i in 1:8) {
  NLM[[i]] <- getNGrams(i,text)
}
NLM <- smooth(NLM)

# NOTES
# 200KB file = 900 lines + 8grams - nonrepeating words = 15MB files = 5MB obj = 31% test / 49% train autocorrect
# 2MB file = 9k lines + 8grams - nonrepeating words = 156MB files = 50MB obj = 38% test / 53% train autocorrect
# 2MB file = 9k lines + 8grams - nonrepeating words/ngrams = 5MB files = 3MB obj = 42% test / 46% train autocorrect
# 20MB file = 90k lines + 8grams - nonrepeating words/ngrams = 57MB files = 25MB obj = 47% test / 49% train autocorrect
# cutting non-repeating words kills train %, but no sig effect on test %
# 10X training corpus = 10X disk & 10X RAM, yields extra +5% accuracy, for 8X response time
# proper punc & number formatting nearly doubled testing % !!!
# higher-order n-grams aren't even being used, dropped from 8 to 3, 50MB to 13MB, no loss of accuracy
# dumping non-repeating ngrams actually increased testing % while decreasing training %, HUGE mem savings
# dropping one-off orphan words & phrases closes the gap between test & train %
# 10X training corpus again yields +5% accuracy, but now response time is dismal 1+ secs
# save on build time by building ng_n+1 from ng_n?
