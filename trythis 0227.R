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

