install.packages('tm')

getSources()
getReaders()

folder = system.file("texts", "txt", package="tm")
txtSource = DirSource(folder)   # dir
class(txtSource); str(txtSource)   

doc = VCorpus(txtSource, readerControl = list(language='en'))

class(doc); summary(doc)



meta(doc)

meta(doc, type = 'local')
inspect(doc)
inspect(doc[1])
doc[[1]][1]

getTransformations()

data("crude")
inspect(crude)

crude[[1]][1]
crude = tm_map(crude, stripWhitespace)
crude = tm_map(crude, content_transformer(tolower))
crude = tm_map(crude, removePunctuation)
crude = tm_map(crude, removeWords, stopwords("english"))
crude = tm_map(crude, stripWhitespace)      # 한번 더! (최종 정리) 


crude = tm_map(crude, stemDocument, language="english")
crude[[1]][1]



tdm = TermDocumentMatrix(crude) 
tdm


rownames(tdm)  
tail(as.matrix(tdm))    
head(as.matrix(tdm))

tdm['year',]


findFreqTerms(tdm, 20)
findFreqTerms(tdm, 20, 30) 

wFreq = sort(rowSums(as.matrix(tdm)), decreasing = T)   
wFreq = subset(wFreq, wFreq > 10) 
display.brewer.all() 
doc = TermDocumentMatrix(doc)  

doc = sort(rowSums(as.matrix(doc)), decreasing = T)

brewer.pal.info 

darks = brewer.pal(8, 'Dark2')

install.packages("wordcloud") 

wordcloud(words = names(wFreq), freq=wFreq, min.freq = 1,
          random.order = F, colors = darks)

doc = TermDocumentMatrix(doc)  
dimnames(doc)
doc = removeSparseTerms(doc, 0.8) 

t_doc = sort(rowSums(as.matrix(doc)), decreasing = T) 
t_doc = subset(t_doc, t_doc > 1)

t_doc

wordcloud(words = names(t_doc), freq=t_doc, min.freq = 10,
          random.order = F, colors = darks)
