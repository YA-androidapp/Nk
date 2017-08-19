# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

# install.packages('dplyr')
# install.packages('quanteda')
# install.packages('RMeCab', repos='http://rmecab.jp/R')
# install.packages("Ruchardet")
# install.packages('rvest')

library(dplyr)
library(quanteda)
library(RMeCab)
library(Ruchardet)
library(rvest)

url.top <- 'http://www.n1kk31.com' # replaced
url.news.first <- paste(url.top, 'news/category/', sep='/')
url.news <- c(
  url.news.first,
  paste(url.news.first, '?bn=', seq(21, 81, by=20), sep='') # item number
  )

pubdate.since <- strptime("2017/08/01 0:00", "%Y/%m/%d %H:%M")
pubdate.last <- strptime("2017/08/15 23:59", "%Y/%m/%d %H:%M")

# acquisition of prompt reports
news.contents <- NULL
sapply(url.news, function(url.news) {
  html.news <- read_html(url.news, encoding='UTF-8')
  url.news.pages.sub <- html.news %>% html_nodes(xpath='//h4[@class="cmn-article_title"]/a') %>% html_attr('href')
  url.news.pages <- paste(url.top, url.news.pages.sub, sep='')

  sapply(url.news.pages, function(page) {
    cat('.')

    html.news.page <- read_html(page, encoding='UTF-8')

    news.title <- html.news.page %>% html_nodes(xpath='//meta[@property="og:title"]') %>% html_attr('content')

    news.datetimestr <- html.news.page %>% html_nodes(xpath='//dd[@class="cmnc-publish"]') %>% html_text()
    news.datetime <- strptime(news.datetimestr, "%Y/%m/%d %H:%M")

    news.content <- html.news.page %>% html_nodes(xpath='//div[@itemprop="articleBody"]') %>% html_text()
    news.content <- gsub('(^[ \r\n　]+)|([ \r\n　]+$)|([\r\n\t]+)', '', news.content)
    news.content <- gsub('。　', '。', news.content)

    news.keywords <- html.news.page %>% html_nodes(xpath='//meta[@name="news_keywords"]') %>% html_attr('content')

    if(pubdate.last >= news.datetime && news.datetime >= pubdate.since) {
      news.contents <<- rbind(news.contents,
                              data.frame(
                                url=page,
                                title=news.title,
                                datetime=news.datetime,
                                text=news.content,
                                keywords=news.keywords,
                                stringsAsFactors=F
                                )
                              )

    }
  })
})

head(news.contents)
str(news.contents)
nrow(news.contents)

write.csv(news.contents, file='nk.txt')

news.contents <- na.omit(news.contents)
news.contents <- subset(news.contents, length(text)>0)

# RMecab
#  https://github.com/koheiw/IJTA/blob/master/extra/mecab.R
texts_seg <<- vector('character')
char_segment <- function(texts) {
  for (i in seq_along(texts)) {
    cat('.')
    if (length(texts[i]) > 0) {
      tryCatch({
        toks <- unlist(RMeCab::RMeCabC(texts[[i]]), use.names = FALSE)
        texts_seg[i] <<- stringi::stri_c(toks, collapse = '|')
        message(i, toks)
      },
      error = function(e) {
        texts_seg[i] <<- ''
      },
      warning = function(e) {
        texts_seg[i] <<- ''
      },
      silent = TRUE)
    } else {
      texts_seg[i] <<- ''
    }
    if (i %% 100 == 0) cat(i, '\n')
  }
  names(texts_seg) <- names(texts)
  return (texts_seg)
}

detectEncoding(news.contents$text)

tmp.sjis <- unlist(lapply(news.contents$text, iconv, from = 'UTF-8', to = 'cp932'))
tmp.sjis <- unlist(lapply(tmp.sjis, gsub, pattern='[[:cntrl:]]+', replacement=''))
tmp.sjis <- unlist(lapply(tmp.sjis, gsub, pattern='a[?]', replacement='。'))
tmp.sjis <- unlist(lapply(tmp.sjis, iconv, from = 'UTF-8', to = 'cp932'))
tmp.sjis <- data.frame(dist=tmp.sjis, stringsAsFactors=F)
news.contents <- cbind( news.contents, ifelse(nrow(tmp.sjis)>1, tmp.sjis, t(tmp.sjis)) )
colnames(news.contents) <- c(colnames(news.contents)[1:(length(colnames(news.contents))-1)],'dist')
detectEncoding(news.contents$dist)
news.contents$dist <- as.character(news.contents$dist)

result.mecab <- unlist(lapply(as.character(news.contents$dist), char_segment))
result.mecab <- data.frame(mecab=result.mecab, stringsAsFactors=F)

news.contents <- cbind( news.contents, ifelse(nrow(result.mecab)>1, result.mecab, t(result.mecab)) )
colnames(news.contents) <- c(colnames(news.contents)[1:(length(colnames(news.contents))-1)],'mecab')

head(news.contents$mecab)
str(news.contents$dist)
nrow(news.contents$dist)
# news.contents[3] <- data.frame(lapply(news.contents[3],iconv,from='cp932',to='utf-8'))

news.contents.csv <- news.contents
news.contents.csv$datetime <- as.character(news.contents.csv$datetime)
write.csv(news.contents.csv, file='nk.csv')
write.csv(result.mecab, file='mecab.txt')

getDfm <- function(texts) {
  quanteda.corpus <- corpus(texts, text_field = 'dist')
  quanteda.tokens <- tokens(quanteda.corpus, what = 'fastestword', remove_punct = FALSE)
  # quanteda.tokens.ngram <- tokens_ngrams(quanteda.tokens, n = 2) # make n-gram
  quanteda.dfm <- dfm(quanteda.tokens)
  return( quanteda.dfm )
}
quanteda.dfm <- getDfm(news.contents)

head(quanteda.tokens)

# Word cloud
png('wordcloud.png')
# textplot_wordcloud(quanteda.dfm, comparison = FALSE)
textplot_wordcloud(quanteda.dfm, random.order=F, rot.per=.25, colors=rainbow(256))
dev.off()

# japanese documents are not supported?
# # Estimate Slapin and Proksch's (2008) 'wordfish' Poisson scaling model of one-dimensional document positions
# pdf(file='wordfish.pdf', encoding='ISOLatin1')
# wfmodel <- textmodel(quanteda.dfm, model='wordfish')
# textplot_scale1d(wfmodel)#, doclabels=substr(news.contents[,2],1,10))
# dev.off()
