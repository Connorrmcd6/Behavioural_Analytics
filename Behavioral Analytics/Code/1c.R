library(mixtools)

d = read.csv('q1.csv')
set.seed(100)
clusters = mvnormalmixEM(d, k = 4,epsilon=1e-04)
plot(clusters,which=2, alpha = 0.05)

posterior_prob = clusters[["posterior"]]
