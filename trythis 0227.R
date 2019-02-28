factor(2, levels=1:4, labels=c('A', 'B', 'O','AB'))

#1
bloodtype=  function(number=1) {
  btresult = (factor(number, levels=1:4, 
                labels=c('A','B','O','AB')))
     return (as.vector(btresult)) }


bloodtype(3)

#2
append = function(v, val) {
  v[length(v) + 1] = val
}
v1 = append(v1, 4)
v1

#3 
m = matrix(1:(10*20), ncol=20, byrow = T)
m
colnames(m) = LETTERS[1:ncol(m)]
rownames(m) = letters[1:nrow(m)]

paste('A','B',seq = '')
paste0('A','B')
colnames(m)[10] = paste0(colnames(m)[10], 10)
colnames(m)[20] = paste0(colnames(m)[20], 20)

df1 = data.frame(column1=11:15, column2=LETTERS[1:5])
df1
class(df1)

df2 = data[1:10, 1:6]
df2
df3
df3 = data[1:10, c(1:3, 7,8)]
df3
cbind(df2, df3[c(4:5)])
df100 = data[101:110, 1:6]
rbind(df2, df100)
lst1 = list(a=1:3, b=4:6)
lst1
str(lst1)

table(data$수학)    
df2
df3


a = cbind(df2, df3[c(4:5)])
a
a[c(1:4,7,6,8,5)]

cn = colnames(a)
a[,c(cn[1:4], '과학', '수학', '예체', '영어')]

dr = data()$result
str(dr)
head(dr)
dr[, 'Item']
data("AirPassengers")
class(AirPassengers)
AirPassengers

data("trees")
class(trees)
trees

month.name
pi

letters[1:3] = c('AA','BB,','CC')
letters

ls(pattern = 'Air')

install.packages('data.table')
options(encoding="UTF-8")
library('data.table')
start = Sys.time()
#read.csv('data/성적.csv')
fread('data/성적.csv', encoding="UTF-8")
Sys.time() - start

sepdata = read.delim('data/sep.txt', sep='#')

sepdata

class(sepdata$name)
str(sepdata)
sepdata$name = as.character(sepdata$name)
summary(sepdata)
View(head(sepdata))
str(sepdata$tel)

tsvdata
tsvdata = read.table("tcv.tsv", sep='\t', header=T, stringsAsFactors=F)

str(tsvdata)
View(tsvdata)

install.packages('readxl')
library('readxl')

#1
mtx = read_excel('data/meltop100.xlsx', sheet = 1, col_names = T)
mtx = mtx[-nrow(mtx), ] 
View(mtx)
save(mtx, file = 'data/meltop100.rda')

#2
meltopcsv = read_excel("data/meltop100.xlsx")
save(meltopcsv, file = 'data/test.rda')
a = load('data/test.rda')
a
#3
read.
fwfdata = read.fwf('data/temper.txt', header=F, widths=c(15,4,68,5,1,100))
fwfdata
result = fwfdata[, c(2,4,5)]
colnames(result) = c('연도','온도','구분자')
result

