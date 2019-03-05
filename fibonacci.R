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

