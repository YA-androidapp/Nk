# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

# install.packages('dplyr')
# install.packages('quanteda')
# install.packages('RMeCab', repos='http://rmecab.jp/R')
# install.packages('rvest')

library(dplyr)
library(quanteda)
library(RMeCab)
library(rvest)

url.top <- 'http://www.n1kk31.com' # replaced
url.news.first <- paste(url.top, 'news/category/', sep='/')
url.news <- c(
  url.news.first,
  paste(url.news.first, '?bn=', seq(21, 81, by=20), sep='') # item number
  )

# acquisition of prompt reports
news.contents <- NULL
sapply(url.news, function(url.news) {
  html.news <- read_html(url.news, encoding='UTF-8')
  url.news.pages.sub <- html.news %>% html_nodes(xpath='//h4[@class="cmn-article_title"]/a') %>% html_attr('href')
  url.news.pages <- paste(url.top, url.news.pages.sub, sep='')
  
  sapply(url.news.pages, function(page) {
    html.news.page <- read_html(page, encoding='UTF-8')
    
    news.title <- html.news.page %>% html_nodes(xpath='//meta[@property="og:title"]') %>% html_attr('content')
    
    news.datetime <- html.news.page %>% html_nodes(xpath='//dd[@class="cmnc-publish"]') %>% html_text()
    
    news.content <- html.news.page %>% html_nodes(xpath='//div[@itemprop="articleBody"]') %>% html_text()
    news.content <- gsub('(^[ \r\n　]+)|([ \r\n　]+$)|([\r\n\t]+)', '', news.content)
    news.content <- gsub('。　', '。', news.content)

    news.keywords <- html.news.page %>% html_nodes(xpath='//meta[@name="news_keywords"]') %>% html_attr('content')
    # news.keywords <- strsplit(news.keywords, ',')
    
    news.contents <<- rbind(news.contents,
                            data.frame(
                              url=page,
                              title=news.title,
                              datetime=strptime(news.datetime, "%Y/%m/%d %H:%M"),
                              text=news.content,
                              keywords=news.keywords,
                              dist='',
                              stringsAsFactors=F
                              )
                            )
  })
})

head(news.contents)
nrow(news.contents)

# RMecab
#  https://github.com/koheiw/IJTA/blob/master/extra/mecab.R
char_segment <- function(texts) {
  texts_seg = vector('character')
  for (i in seq_along(texts)) {
    if (texts[i] != '') {
      toks <- unlist(RMeCab::RMeCabC(texts[[i]]), use.names = FALSE)
      texts_seg[i] <- stringi::stri_c(toks, collapse = ' ')
    } else {
      texts_seg[i] = ''
    }
    if (i %% 100 == 0) cat(i, '\n')
  }
  names(texts_seg) <- names(texts)
  return (texts_seg)
}

news.contents[3] <- data.frame(lapply(news.contents[2], char_segment), stringsAsFactors=F)
# news.contents[3] <- data.frame(lapply(news.contents[3],iconv,from='cp932',to='utf-8'))

write.csv(news.contents, file='nk.csv')

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
