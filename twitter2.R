nouns = unique(wc)
length(nouns)
nouns = sapply(wc, unique)
nouns
# 1글자 ~ 4글자까지
nouns1 = sapply(nouns, function(x) {
  Filter(function(y) { nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y) }, x)
})
nouns1
wtrans = as(nouns1, "transactions")
class(wtrans)
rules = apriori(wtrans, parameter = list(supp=0.015, conf=0.5))
inspect(sort(rules))
        

library(arulesViz); library(visNetwork)
subrules2 <- head(sort(rules, by="lift"), 20) ## lift 기준으로 상위 20개만을 시각화
ig <- plot( subrules2, method="graph", control=list(type="items") )

subrules2 <- head(sort(rules, by="confidence"), 30)
ig <- plot( subrules2, method="graph", control=list(type="items") )
# saveAsGraph seems to render bad DOT for this case
ig_df <- get.data.frame( ig, what = "both" )

visNetwork(
  nodes = data.frame(id = ig_df$vertices$name,
                     value = ig_df$vertices$support,
                     title = ifelse(ig_df$vertices$label == "", ig_df$vertices$name, ig_df$vertices$label), ig_df$vertices), edges = ig_df$edges) %>%
  visEdges(ig_df$edges) %>%visOptions( highlightNearest = T )

visNetwork(
  nodes = data.frame(id = ig_df$vertices$name,
                     value = ig_df$vertices$support, ig_df$vertices),
  edges = ig_df$edges
)



###########################

install.packages(c('rvest', 'httr', 'stringr'))

library(rvest); library(httr); library(stringr); library(dplyr)
URL= "https://news.naver.com/main/ranking/popularDay.nhn?mid=etc&sid1=111"


html = read_html(URL)
links = html_attr(html_nodes(html, '.content dt a'), 'href')
links = links[!is.na(links)]       # NA 제거
news = list() 
news

for (i in 1:length(links)) {
  try({
    htxt = read_html(paste0('https://news.naver.com', links[i]))
    comments = html_nodes(htxt, '#articleBodyContents')
    # repair_encoding(html_text(comments), from='utf-8')
    get_news = repair_encoding(html_text(comments))
    news[i] = str_trim(get_news)
  }, silent = F)
}



library(rvest); library(httr); library(stringr); library(dplyr)



newsUrl = "https://news.naver.com/main/home.nhn"
newsUrl
html = read_html(newsUrl)
html
links = html_attr(html_nodes(html, '#main_content li a'), 'href')

links = links[!is.na(links)]
links = links
length(links)
links

for (i in 1:length(links)) {
  try({
    htxt = read_html(links[i])
    comments = html_nodes(htxt, '#articleBodyContents')
    get_news = repair_encoding(html_text(comments))   # repair_encoding(html_text(comments), from='utf-8')
    news[i] = str_trim(get_news)
  }, silent = F)
}

removeStopword(news)
for (i in 1:length(news)) {
  news[[i]][1] = removeStopword(news[[i]][1])
  news[[i]][1] = gsub(" flash 오류를 우회하기 위한 함수 추가function flashremoveCallback", "", news[[i]][1])
}
news;
testnews = news
removeStopword = function(t) {
  t = gsub("[[:cntrl:]]", "", t) 
  t = gsub("http[s]?://[[:alnum:].\\/]+", "", t) 
  t = gsub("&[[:alnum:]]+;", "", t)
  t = gsub("@[[:alnum:]]+", "", t)
  t = gsub("@[[:alnum:]]+[:]?", "", t)
  t = gsub("[ㄱ-ㅎㅏ-ㅣ]","",t) 
  t = gsub("\\s{2,}", " ", t) 
  t = gsub("[[:punct:]]", "", t)  
  t = gsub("https", "", t)
  t = gsub("RT", "", t)
  t = gsub("\\s{2,}", " ", t) 
  # mac: emo 제거
  gsub('\\p{So}|\\p{Cn}', '', t, perl = TRUE)
}

wc = sapply(testnews, extractNoun, USE.NAMES = F)
ul = unlist(wc)
ul = ul[nchar(ul) > 1]
wc1 = table(ul)
wc2 = head(sort(wc1, decreasing = T), 100)

pal = brewer.pal(9, "Set1")
wordcloud(names(wc2), freq=wc2, scale=c(3,0.5), rot.per=0.25, 
          min.freq = 1, random.order = F, random.color = T, colors = pal)
