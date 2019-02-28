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
read.csv("data/melon_test.csv")
a
#3
read.
fwfdata = read.fwf('data/temper.txt', header=F, widths=c(15,4,68,5,1,100))
fwfdata
result = fwfdata[, c(2,4,5)]
colnames(result) = c('연도','온도','구분자')
result



boxplot(data$수학)

hist(data$수학)

data[data$학번 == 10337, ]

km = data[data$국어 > 90 & data$수학 > 90, ]
km

km[order(km$수학), ]

read.csv("data/ttt.csv")
order(km$수학)
mean(data$수학)

#1
result = data[data$반 == '난' & data$성별 == '남' & data$수학 > 90 & data$국어 > 90, ]
result[order(result$'국어',decreasing=TRUE),]

#2
data$평균 = (data[,4] + data[,5] + data[,6] + data[,7] + data[,8]) / 5

result = aggregate(data=guk80[data$국어 >= 80, ], cbind(국어,평균)~반, mean)
result

options(encoding="UTF-8")




data$pass = ifelse(data$평균 >= 60, TRUE, FALSE)
data
data$scout = ifelse(data$평균 >= 60, 
                    ifelse(data$성별 == '남', 'BoyScout', 'GirlScout'),
                    '')

#1
a = data[data$scout != '',]
qplot(a$scout)

#2
data$학점 = ifelse(data$평균 >= 90, 'A', ifelse(data$평균>=80, 'B', ifelse(data$평균>=70, 'C', ifelse(data$평균>=60, 'D', 'F'))))
data
#3
qplot(data$학점)

mpg = as.data.frame(ggplot2::mpg)
aggregate(data=data, 수학~반, mean)
mpg
km[order(km$수학), ]

aggregate(data=data, cbind(국어,영어,수학)~반, mean)



#1
mpg$평균연비 = (mpg[,8 ] + mpg[, 9] / 2)
mpg

#2
a = mpg[,c(4,12,10)]
b= aggregate(data=a, 평균연비~year+fl ,mean)
b[order(b$year),]

#3
midwest  = as.data.frame(ggplot2::midwest)
View(midwest)
biggest_area = (midwest[midwest$area == max(midwest$area),])
biggest_area[,c(2:4)]
biggest_area = (midwest[midwest$area == max(midwest$area),])



#4
cn = colnames(midwest)
cn[5] = 'total' 
cn[10] = 'asian'
