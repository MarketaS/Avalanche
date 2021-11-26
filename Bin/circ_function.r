circ <- function(x){
  a <- mean(circular(x, units = 'degrees'), na.rm = T)[[1]]
  a[which(a < 0)] <- a[which(a < 0)] +360
  return(a)
}
