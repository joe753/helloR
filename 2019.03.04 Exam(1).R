#1
mpg$평균연비 = (mpg[,8 ] + mpg[, 9] / 2)
mpg[order(mpg$평균연비, decreasing=TRUE),]


#2
a = mpg[,c(4,12,10)]
b= aggregate(data=a, 평균연비~year+fl ,mean)
b[order(b$year),]
