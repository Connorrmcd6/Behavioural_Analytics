setwd("~/Desktop/Masters/STK802/Assignment 2")

S = matrix(0,nrow=20,ncol=20)

z = matrix(0,nrow=20,ncol=2)
z[1,1] = 1
z[1,2] = 3
z[2,1] = 4
z[2,2] = 3
z[3,1] = 2
z[3,2] = 3
z[4,1] = 3
z[4,2] = 1
z[5,1] = 7
z[5,2] = 3
z[6,1] = 8
z[6,2] = 2
z[7,1] = 5
z[7,2] = 1
z[8,1] = 9
z[8,2] = 1
z[9,1] = 10
z[9,2] = 3
z[10,1] = 6
z[10,2] = 3
z[11,1] = 11
z[11,2] = 1
z[12,1] = 12
z[12,2] = 1
z[13,1] = 13
z[13,2] = 3
z[14,1] = 14
z[14,2] = 1
z[15,1] = 15
z[15,2] = 3
z[16,1] = 16
z[16,2] = 2
z[17,1] = 17
z[17,2] = 2
z[18,1] = 18
z[18,2] = 1
z[19,1] = 19
z[19,2] = 3
z[20,1] = 20
z[20,2] = 2


y = z[,2]

vals = z[,1]


m = 100
for(z in 1:m){
  for(i in 1:length(y)){
    for(j in 1:length(y)){
      if(y[i] == y[j] & vals[i] != vals[j] ){
        S[i, j] = S[i, j] + 1
      }
    }
  }
  
}


print(y)
print(S)










