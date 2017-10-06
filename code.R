setwd("C:/Users/Tqm/Desktop/R-Work")
library(data.table)
library(twitteR)
library(httr)
api_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxx"
api_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


Tweet<-BJP #Tweets for BJP are saved into an object.
Tweet.text = lapply(Tweet,function(t)t$getText()) #Text data from the tweets is subsetted.

clean.text <- function(some_txt) #data cleaning is performed.
{
  some_txt = gsub("&amp", "", some_txt) #Removing symbols
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt) #Removing retweets
  some_txt = gsub("@\\w+", "", some_txt) #Removing handles
  some_txt = gsub("[[:punct:]]", "", some_txt) #Removing punctuations
  some_txt = gsub("[[:digit:]]", "", some_txt) #Removing  digits
  some_txt = gsub("http\\w+", "", some_txt) #Removing hyperlinks
  some_txt = gsub("[ t]{2,}", "", some_txt) #Removing retweets through links
  
}

clean_text = clean.text(Tweet.text) #Text is cleaned.

library(tm)

tweet_corpus = Corpus(VectorSource(clean_text)) #Creating a corpus of clean text
tdm = TermDocumentMatrix(tweet_corpus, control = list(stopwords=stopwords("english"))) 
#Creating a term document matrix from the corpus and removing stop words.
library(wordcloud)
require(plyr)

m = as.matrix(tdm) #we define tdm as matrix

word_freqs = sort(rowSums(m), decreasing=TRUE) #now we get the word orders in decreasing order

dm = data.frame(word=names(word_freqs), freq=word_freqs) #we create our data set

wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")) #and we visualize our data



Hpos=scan("C:/Users/Tqm/Desktop/R-Work/HN_POS.txt",what='character',comment.char = ";")
Hneg=scan("C:/Users/Tqm/Desktop/R-Work/HN_NEG.txt",what='character',comment.char = ";")
pos=scan('C:/Users/Tqm/Desktop/R-Work/PositiveWords.txt',what='character',comment.char=';')
neg=scan('C:/Users/Tqm/Desktop/R-Work/NegativeWords.txt',what='character',comment.char=';')

score.sentiment=function(sentences,pos.words,neg.words,hpos.words,hneg.words,.progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  
  scores=laply(sentences,function(sentence,pos.words,neg.words,hpos.words,hneg.words)
    # clean up sentences with R's regex-driven global substitute, gsub():
  {
    sentence=gsub('[[:punct:]]','',sentence)
    sentence=gsub('[[:cntrl:]]','',sentence)
    sentence=gsub('\\d+','',sentence)
    
    # and convert to lower case:
    sentence=tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list=strsplit(sentence,'\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words=unlist(word.list)
    
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches=match(words,pos.words)
    neg.matches=match(words,neg.words)
    hpos.matches=match(words,hpos.words)
    hneg.matches=match(words,hneg.words)
    # match() returns the position of the matched term or NA
    
    # we just want a TRUE/FALSE:
    pos.matches=!is.na(pos.matches)
    neg.matches=!is.na(neg.matches)
    hpos.matches=!is.na(hpos.matches)
    hneg.matches=!is.na(hneg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score=sum(pos.matches)-sum(neg.matches)
    
    return(score)
  },pos.words,neg.words,.progress=.progress)
  scores.df=data.frame(score=scores,text=sentences)
  return(scores.df)
}

analysis=score.sentiment(clean_textU,pos,neg,Hpos,Hneg)


table(analysis$score)

mean(analysis$score)

hist(analysis$score)


Hpos=scan("C:/Users/Tqm/Desktop/R-Work/POH.txt",what='character')
Hneg=scan("C:/Users/Tqm/Desktop/R-Work/NOH.txt",what='character')


score.sentiment=function(sentences,hpos.words,hneg.words,.progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  
  scores=laply(sentences,function(sentence,hpos.words,hneg.words)
    # clean up sentences with R's regex-driven global substitute, gsub():
  {
    sentence=gsub('[[:punct:]]','',sentence)
    sentence=gsub('[[:cntrl:]]','',sentence)
    sentence=gsub('\\d+','',sentence)
    
    # and convert to lower case:
    sentence=tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list=strsplit(sentence,'\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    words=unlist(word.list)
    
    
    # compare our words to the dictionaries of positive & negative terms
  
    hpos.matches=match(words,hpos.words)
    hneg.matches=match(words,hneg.words)
    # match() returns the position of the matched term or NA
    
    # we just want a TRUE/FALSE:
    
    hpos.matches=!is.na(hpos.matches)
    hneg.matches=!is.na(hneg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score=sum(hpos.matches)-sum(hneg.matches)
    
    return(score)
  },hpos.words,hneg.words,.progress=.progress)
  scores.df=data.frame(score=scores,text=sentences)
  return(scores.df)
}

analysis=score.sentiment(clean_textU,Hpos,Hneg)


table(analysis$score)

mean(analysis$score)

hist(analysis$score)




