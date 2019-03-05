#1
data$group = sample(rep(LETTERS[1:3], times=(nrow(data)/3)), size=nrow(data), replace=F)



#2
while(TRUE) {
  x = as.integer(readline(prompt = "Input the number: "))
  vect = c(0,1)
  if ( x <= 0 ) break
  
  else {
    for (i in 3:x - 2)
      vect[length(vect) + 1 ] = vect[length(vect) - 1] + vect[length(vect)]
  }
  print ( vect[1:x] )
}


#3
smdt[nrow(smdt) + 1 , 2:4] = apply(smdt[, 2:4], MARGIN = 2, FUN = mean)
smdt[nrow(smdt),1] = '계'
smdt$total = apply(smdt[,2:4], MARGIN = 1, FUN = sum)
smdt$avg = apply(smdt[,2:4], MARGIN= 1, FUN=mean)
smdt



#4
df_4 = cbind( data.frame(no=1:4, year=2016:2019), 
               matrix(round(runif(48), 3) * 1000, ncol=12, dimnames = list(NULL, month.name))) 

meltsum = melt(df_4[,2:14], id.vars = "year", variable.name = 'month')
colna

mes(meltsum)[3] = 'saleamt'
meltsum
