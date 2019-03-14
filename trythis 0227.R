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
mpg[order(mpg$평균연비, decreasing=TRUE),]

#2
a = mpg[,c(4,12,10)]
b= aggregate(data=a, 평균연비~year+fl ,mean)
b[order(b$year),]

#3
statpop_total = aggregate(data=midwest, poptotal~state, sum)
statpop_asian = aggregate(data=midwest, popasian~state, sum)
result = cbind(statpop_total, statpop_asian[2])
result

######## 3-1
midwest  = as.data.frame(ggplot2::midwest)
View(midwest)
biggest_area = (midwest[midwest$area == max(midwest$area),])
biggest_area[,c(2:4)]

smallest_area = (midwest[midwest$area == min(midwest$area),])
smallest_area[,c(2:4)]



most_popwhite = midwest[midwest$popwhite == max(midwest$popwhite),]
most_popwhite[,c(2,3,7)]

most_popblack = midwest[midwest$popblack == max(midwest$popblack),]
most_popblack[,c(2,3,8)]

most_popamerindian = midwest[midwest$popamerindian == max(midwest$popamerindian),]
most_popamerindian[,c(2,3,9)]

most_popasian = midwest[midwest$popasian == max(midwest$popasian),]
most_popasian[,c(2,3,10)]


describe(midwest)
midwest$state
###########


#4
cn = colnames(midwest)
cn[5] = 'total' 
cn[10] = 'asian'

#5
midwest
total_asian = sum(midwest$popasian)
midwest$asianpct = ( midwest$popasian / total_asian * 100)
hist(midwest$asianpct)

#6 전체 인구(모든 인종) 중 아시아계 인구 분포

statpop_total = aggregate(data=midwest, poptotal~state, sum)
statpop_asian = aggregate(data=midwest, popasian~state, sum)
stat_asianpct = (statpop_asian$popasian / statpop_total$poptotal) * 100
stat_asianpct
barplot(stat_asianpct, names.arg= statpop_total$state)


#7
avr_popasian = mean(midwest$popasian)
midwest$asianrate = ifelse(midwest$popasian > avr_popasian, 'lg', 'sm')
View(midwest)

#8
qplot(midwest$asianrate)

qplot(midwest$asianpct)
hist(midwest$asianpct)






rep(1, times=3)
rep(LETTERS[1:5], length.out=27)


seq(13.5, -4.5 ,length.out=10)


set.seed(100); sample(1:5, size=3)    


sample(1:45, size=6, replace=F)

runif(20)
runif(n=30, min=50, max=60)

sample(1:5, size=5, replace=T)

data$c1 = sample(rep(LETTERS[1:6], times=(nrow(data)/6)), size=nrow(data), replace=F)
str(data$c1)
 
set.seed(255)
smdt = data.frame(stuno = 1:5, 
                  Korean=sample(60:100, 5),
                  English=sample((5:10) * 10, 5),
                  Math=sample(50:100, 5))
smdt

data[1:3, '']
set.seed(100)
sample(1:5, size=30, replace=T)
grep(pattern='^2.*0$', x=data$학번, value = T)   # value= T(값을 출력, F: 행)




install.packages('lubridate')
library(lubridate)


ymd('20190228') 


mdy('02282019')
dt1 = ymd('20190305')
year(dt1)
day(dt1)
month(dt1) = 3
day(dt1) = 1
dt1

month(days_in_month(1:12))
month.name

class(a)
a[1,]
ddays(10)


for (i in 1:nrow(data)) {print (data[i,'수학'])}

i = 0

while(i < 10) { print(i); i = i + 1 } 

i = 0
while(TRUE) {
  i = i + 1
  if (i %% 3 == 1 | i %% 3 == 2)
    next
  if (i > 30)
    break
  print (i)
}

smdt
apply(smdt[, 2:4], MARGIN = 1, FUN = mean)

apply(smdt[, 2:4], MARGIN = 2, FUN = quantile)

lapply(smdt[, 2:4], FUN = mean)
sapply(smdt[, 2:4], FUN = mean, simplify = T)
vapply(smdt[, 2:4], FUN = mean, FUN.VALUE = 1)

library('reshape2')

dfsum = cbind( data.frame(no=1:4, year=2016:2019), 
               matrix(round(runif(16), 3) * 1000, ncol=4, dimnames = list(NULL, paste0('Q', 1:4))) )
dimnames = lst(1:4, paste0('Q', 1:4))
dimnames

dfsum
dim(dfsum)
matrix(round(runif(16),3) * 1000, ncol=4, )

dfsum = cbind( data.frame(no=1:4, year=2016:2019), 
               matrix(round(runif(16), 3) * 1000, ncol=4, dimnames = list(NULL, paste0('Q', 1:4))) )


dfsum
melt(data=dfsum[,2:6], id.vars = "year")




##########################
#1
data$group = sample(rep(LETTERS[1:3], times=(nrow(data)/3)), size=nrow(data), replace=F)



#2
while(TRUE) {
  x = as.integer(readline(prompt = "Input the number: "))
  vect = c(0,1)
  if ( x <= 0 ) break
  
  else {
    for (i in 1:x - 2)
      vect[length(vect) + 1 ] = vect[length(vect) - 1] + vect[length(vect)]
  }
  print ( vect[1:x] )
}


#3
smdt[nrow(smdt) + 1 , 2:4] = apply(smdt[, 2:4], MARGIN = 2, FUN = mean)
smdt[nrow(smdt),1] = '계'
smdt$total = apply(smdt[,2:4], MARGIN = 1, FUN = sum)
smdt$avg = apply(smdt[,2:4], MARGIN= 1, FUN=mean)
smdt$avg = round(smdt$avg)


#4
df_4 = cbind( data.frame(no=1:4, year=2016:2019), 
               matrix(round(runif(48), 3) * 100000, ncol=12, dimnames = list(NULL, month.name))) 

meltsum = melt(df_4[,2:14], id.vars = "year", variable.name = 'month', value.name = 'saleamt')
colna

mes(meltsum)[3] = 'saleamt'
meltsum


t = c(5, 7, 2, 8, 20, 11, 19)
t[order(t)]

smdt[order(smdt$avg, -smdt$Korean),]
t
rev(t)

dataArray = array(1:24, dim=c(3, 4, 2))  
dim(dataArray) 

dataArray


#### dplyr
options = 
data = read.csv('data/성적.csv')
library(dplyr)
data
data = rename(data, math=수학)
head(data)

attach(data)
mean(math)
sum(math)

detach(data)

mean(data$국어)
data[data$group == 'C',]
with(data, mean(math))


data
data %>% filter(group == 'C' & math > 90)

data %>% filter(수학 %in% c('50', '75'))

#### try 1
mpg %>% 
  filter(class == 'suv' | class == 'compact') %>%
  select (model, cty, hwy, fl)
head(mpg)

### try 2
mpg %>% 
  arrange(desc(hwy)) %>%
  head(5)



###  try 3
mpg %>% 
  filter(class == 'suv') %>% 
  group_by(manufacturer) %>%
  summarise(m = mean( (cty + hwy) / 2)) %>%
  arrange(desc(m)) %>%
  head(5)

df2 = data[1:10, 1:6]


#### try 4
fl = c('c','d','e','p','r')
type = c('CNG', 'diesel', 'E85', 'Premi', 'Reqular')
price = c(1.33, 1.02, 0.92, 1.99, 1.22)
fl_info = data.frame(fl,type,price, stringsAsFactors =F)
fl_info

mpg = inner_join(mpg, fl_info, by=c('fl', 'fl')) %>%
  select(-type)
  
mpg





a = mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == 'suv') %>% 
  mutate(total_fl = (cty + hwy) / 2 ) %>%
  arrange(desc(manufacturer)) 
a %>% head(5)
  

data %>%
  mutate(kor_eng = kor + eng) %>%
  arrange(desc(kor_eng)) %>%
  head

data %>%
  group_by(cls, gen) %>%
  summarise(m = mean(math))

data %>%
  group_by (cls) %>%
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            medi_math = median(math),
            n_math = n()) %>%
  arrange(desc(mean_math))


data %>% 
  group_by (gen) %>%
  summarise(mean_kor = mean(kor),
            sum_kor = sum(kor),
            mean_eng = mean(eng),
            n_stu = n())



dfsum = cbind( data.frame(yno=1:4, year=2016:2019), 
               matrix(round(runif(16), 3) * 1000, ncol=4, dimnames = list(NULL, paste0('Q', 1:4))))

dfsum


sales = cbind( data.frame(no=1:12, year=2016:2019), 
               matrix(round(runif(144), 3) * 100000, ncol=12, dimnames = list(NULL, month.abb)) )
sales


right_join (sales, dfsum, by=c('no' = 'yno'))

inner_join (sales, dfsum, by=c('year'='year'))

semi_join(sales, dfsum, by=c('year' = 'year', 'no' = 'yno'))

full_join(sales, dfsum, by=c('year' = 'year', 'no' = 'yno'))





data




plot(x=1, y=1)

smdt


plot(x = smdt$stuno, y = smdt$Korean,
     col = '#0000FF',
     cex = 2,
     las= 1,
     type = 'p',         # p, l, b, c, o, s
     xlim = c(1, 5),
     ylim = c(50, 100),
     pch = 14,                         # > ?points
     xlab = '학번', ylab = '국어',
     main = '그래프 타이틀')



xl = c(0, 8); yl = c(40, 100)


plot(x = smdt$stuno, y = smdt$Korean,
     col='#0000FF', cex=3, pch = 8,
     xlim = xl, ylim = yl,           # xl = c(-0.5, 5.5); yl = c(0, 100)
     xlab = '학번', ylab = '국어, 수학',
     main = '우리반 국어 / 수학 성적')



plot(x = smdt$stuno, y = smdt$Korean,
     col='#0000FF', cex=3, pch = 8,
     xlim = xl, ylim = yl,           # xl = c(-0.5, 5.5); yl = c(0, 100)
     xlab = '학번', ylab = '국어, 수학',
     main = '우리반 국어 / 수학 성적')
par(new = T)
plot(x = smdt$stuno, y = smdt$Math,
       col='#ff0000', cex=3, pch = 21,
       xlim = xl, ylim = yl,
       xlab = '', ylab = '')

legend('bottomright',     
        legend=c('국어', '수학'),
        pch=c(8, 21), col=c('blue', 'red'), bg='gray')

library('dplyr')

t = data  %>% filter(eng > 90) %>% select('cls', 'gen') %>% table
t

barplot(t,
          beside = T,
        xlim = c(0,12),
          border = 'dark blue',
          density = 20*4 : 1,
          angle = 15 + 10*1:2,
          xlab = '학급별 성별', ylab = '영어',
          legend=rownames(t),
          col=heat.colors(4))
mpg

d1 = mpg %>% group_by (year,displ)
d1


save(data, file='data/data_eng.rda')

### try 1
d1 = mpg %>% 
  filter(year == 1999) %>% 
  group_by(year,displ) %>% 
  summarise(m1 = mean(cty), m2 = mean(hwy))
d2 = mpg %>% 
  filter(year == 2008) %>%
  group_by(year, displ) %>% 
  summarise(m1 = mean(cty), m2 = mean(hwy))
b = bind_cols(d1, d2)
View(b)

ggplot( b, aes(x=displ)) + 
  geom_line(aes(y=m1, color='1999 cty')) + 
  geom_line(aes(y=m2, color='1999 hwy')) +
  geom_line(aes(y=m11, color='2008 cty'), size=2) +
  geom_line(aes(y=m21, color='2008 hwy'), size=2) +
  scale_colour_manual("", breaks = c("1999 cty", "1999 hwy","2008 cty", "2008 hwy"),
                      values = c("red", "grey", "blue", "black")) +
  xlab("배기량") +
  xlim(1, 7) +
  scale_y_continuous("연비", limits = c(5, 45)) +
  labs(title = '연도별 통합연비', subtitle = '굵은선 = 2008년') 

## try2

a = data  %>% group_by (cls, gen)%>% filter(kor >= 80)
a



ggplot(a, aes(cls)) +
  geom_bar(aes(fill=gen),
           width = 0.5) +
  theme(axis.text.x = element_text(angle=0,       # 글씨의 기울기
                                   vjust=0.6)) +   # 글씨의 하단 맞춤(띄우기)
  scale_fill_discrete(name = "성별") + # legend
  xlab("학급") + 
  ylab("학생수") + 
  labs(title = '국어 우수 학생', subtitle = '80점 이상')


### try3
try3 = data %>% group_by(cls) %>% filter(kor >= 95)
try3
ggplot(try3, aes(kor)) +
  geom_density(aes(fill=factor(cls)), alpha=0.5) +
  labs(title="반별 국어 우수 학생", subtitle = "(국어성적 A+)",
       caption="Source: ggplot2::mpg",
       x = "성적",
       y = "밀도",
       fill = "실린더수")

### try 4

try4 = midwest %>% filter(poptotal <= 500000 & popasian <= 10000)
try4

ggplot(data, aes(cls, kor)) +
  geom_point(aes(color=cls, size=kor), 
             alpha=0.3)


ggplot(try4, aes(poptotal, popasian)) +
  geom_point(aes(color=poptotal, size=popasian), 
             alpha=0.3)
