X = c(1,6,7,8,2,3,4,10,5,9,11,12,13,14,15,16,17,18,19,20)

I = matrix(0,nrow=length(X),ncol=length(X))
m = 50
#s = sample(X, 3)


for(z in 1:m){
  s = sample(X, 20, replace = F)
  for(i in s){
    for(j in s){
      if(i != j){
        r = match(c(i),X)
        c = match(c(j),X)
        I[r, c] = I[r, c] + 1
      }
    }
  }
}


print(I)




