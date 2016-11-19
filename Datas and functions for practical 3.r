# Introduction to Multivariate Methods - Practical 3 (17.2.)

# Working Directory / Työkansio
setwd('...')

# The datas and the codes from previous practical sessions
# Datat ja koodit aiemmilta harjoituskerroilta
data1 <- data.frame(read.table('bank.dat'))
length <- data1[, 1]
left <- data1[, 2]
right <- data1[, 3]
bottom <- data1[, 4]
top <- data1[, 5]
diag <- data1[, 6]
names(data1) <- c('length', 'left', 'right', 'bottom', 'top', 'diag') 

data2 <- data.frame(read.table('iris.dat'))
species <- data2[, 1]
sepall <- data2[, 2]
sepalw <- data2[, 3]
petall <- data2[, 4]
petalw <- data2[, 5]
names(data2) <- c('species', 'sepall', 'sepalw', 'petall', 'petalw')

options(digits = 5)

#From previous practicals / aiemmista harjoituksista:
z <- c(rep(1, 100), rep(2, 100))
data1b <- data1
data1b$bottom <- 0.1*data1b$bottom #To centimeters / Senttimetreiksi
data1b$top <- 0.1*data1b$top
data1b$diag <- 0.1*data1b$diag

bs1 <- data1[1:100, ] #Real bills
bs2 <- data1[101:200, ] #Forged bills
sp1 <- subset(data2[2:5], species == 1) #Species 1
sp2 <- subset(data2[2:5], species == 2) #Species 2
sp3 <- subset(data2[2:5], species == 3) #Species 3

#Some useful vectors / Muutama hyödyllinen vektori
tr1 <- c(1:50, 101:150)
tes1 <- c(51:100, 151:200)
tr2 <- c(1:25, 51:75, 101:125)
tes2 <- c(26:50, 76:100, 126:150)


library(ICS)
#library(JADE)
library(MASS)
library(class)


# Independent component analysis / Riippumattomien komponenttien analyysi (ICA)
ICS1 <- ics(data1)
summary(ICS1)
plot(ICS1, col = z)
plot(data1, col = z)
indc1 <- ics.components(ICS1)
cor(data1, indc1) 



# LINEAR DISCRIMINANT ANALYSIS, SWISS BILLS
# TRAINING SET AND TEST SET
train <- tr1
test <- tes1
lda.results <- lda(data1[train, ], z[train])
lda.results
lda.pred <- predict(lda.results, data1[test, ])$class
names(lda.pred) <- rownames(data1[test, ])
lda.pred

table(lda.pred, z[test])



# k-nearest neighbour classification - IRIS DATA
#WITH TRAINING SET AND TEST SET
train <- data2[tr2, 2:5]
test <- data2[tes2, 2:5]
species.train <- data2[tr2, 1]
knn.pred <- knn(train, test, cl = species.train, k = 3, prob = TRUE)
knn.pred
table(knn.pred, species[tes2])




# HIERARCHICAL CLUSTERING - SWISS BILLS
h <- hclust(dist(data1), method = "average")
plot(h)

h2 <- cutree(h, k = 2)
names(h2) <- rownames(data1)

plot(data1, col = z)
plot(data1, col = h2)

h3 <- cutree(h, k = 3)
names(h3) <- rownames(data1)
plot(data1, col = h3)


# k-MEANS CLUSTERING - SWISS BILLS
k <- kmeans(data1, 2)
names(k$cluster) <- rownames(data1)

plot(data1, col = z)
plot(data1, col = k$cluster)

k <- kmeans(data1, 3)
names(k$cluster) <- rownames(data1)
plot(data1, col = k$cluster)

k <- kmeans(data1, 4)
names(k$cluster) <- rownames(data1)
plot(data1, col = k$cluster)
