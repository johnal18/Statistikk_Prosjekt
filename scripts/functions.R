
clean_row_and_id = function(x, y){
  rownames(x) = 1:nrow(x)
  rownames(y) = 1:nrow(y)
  
  x$id = rownames(x)
  y$id = rownames(y)
}

find_correlation = function(x, y, n){
  return(((sum((x - mean(x))*(y - mean(y))))*(1/n))/
           ((sqrt(sum((x - mean(x))^2)/n))*(sqrt(sum((y - mean(y))^2)/n))))
}

number_of_raindays = function(x){
  count = 0
  for (i in x) {
    if(i != 0){
      count = count + 1
    }
  }
  return(count)
}

number_of_raindays(oslo$nedbor)
number_of_raindays(stavanger$nedbor)
