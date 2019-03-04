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
midwest$asianrate = ifelse(midwest$popasian >= avr_popasian, 'lg', 'sm')
View(midwest)




#8
qplot(midwest$asianrate)
