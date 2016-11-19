setwd("C:/Users/JMFlin/Desktop/R/R files/Multivariate Methods")

#1. Find the principal variables for the Swiss Bank Notes data. Then find
#the principal variables separately for outer measurements and inner
#measurements and plot the results to compare the genuine and forged bills.
source("read_and_analyse.r")

# From practical session 1 / Harjoitusten 1 koodeja
bs1 <- data1[1:100, ]
bs2 <- data1[101:200, ]
sp1 <- subset(data2[2:5], species == 1) #Species 1
sp2 <- subset(data2[2:5], species == 2) #Species 2
sp3 <- subset(data2[2:5], species == 3) #Species 3
z < -c(rep(1, 100), rep(2, 100))


#PCA and CCA: Preliminary codes codes from file read_and_analyse.r
#PCA ja CCA: Alustavat koodit tiedostosta read_analyse.r

# PRINCIPAL COMPONENT ANALYSIS (PCA) / P??????KOMPONENTTIANALYYSI (PCA)
# Maximization of the variance - Varianssin maksimointi
#
# 1 Swiss Bank Note data
# All variables / kaikki muuttujat
results1 <- prcomp(data1)
summary(results1)
#The first principal component explains 66.8 % of the total variation (variation of the original variables)
#  1st and 2nd component explains already 87,6 % of the variation.
#  Perhaps 1 or 2 components for further analysis (dimension reduction)?
#Ensimm???inen p??????komponentti selitt?????? 66,8 % kokonaisvaihtelusta (alkuper???isten muuttujien vaihtelusta)
#  1. ja 2. komponentti selitt?????? 87,6 % vaihtelusta.
#  Mahd. 1 tai 2 komponenttia mukaan lis???analyyseihin (dimension supistaminen/muuttujien v???hent???minen)?

plot(results1) #Scree plot of the variances / P??????komponenttien varianssit pylv???skuviona

# Estimate of variable loadings / Muuttujien latausmatriisin estimaatti
results1$rotation
#Noticed some different signs than in lecture notes? Do not worry!
# From R help of function prcomp:
#   "The signs of the columns of the loadings and scores are arbitrary,
#   and so may differ between different programs for PCA, and even between different builds of R."
#Huomasitko eri etumerkkej??? kuin luentomateriaalissa? Ei h???t??????!
# R-ohjelman helpist??? l???yt?????? t???st??? maininnan prcomp-muuttujan kohdalta:
#   Latausmatriisin sarakkeiden etumerkit ovat sattumanvaraisia,
#   joten ne saattavat vaihdella ohjelmien ja jopa R-ohjelman eri versioiden v???lill???.

# The principal components / P??????komponentit
PCdata1 <- data.frame(as.matrix(data1) %*% results1$rotation)
# Scatterplot matrix of principal components / Sirontakuviomatriisi p??????komponenteista
plot(PCdata1, col = z) #PC1 is learly very important / PC1 on selv???sti hyvin t???rke???

# Plot of the original variables (for comparison) / Alkuper???isten muuttujien kuva (vertailun vuoksi)
plot(data1, col = z) 

# Correlations between original variables and principal components
# Alkuper???isten muuttujien ja p??????komponenttien v???liset korrelaatiot
cor(data1, PCdata1)
#e.g. PC1: Very high correlations with bottom (-0.92) and diag (0.87).
# Use this knowledge to name the principal component?
#Esim.PC1: Hyvin suuret korrelaatiot bottom- (-0.92) ja diag-muuttujien (0.87) kanssa.
# K???yt??? t???t??? tietoa p??????komponentin nime???misess????

# Different measurement units / eri mittausarvot
#   3 first in millimeters, 3 next in centimeters: Measurement dependence!
#   3 ensimm???ist??? millimetrein???, 3 viimeist??? senttimetrein???: Mittausriippuvuus!
data1b <- data1
data1b$bottom <- 0.1*data1b$bottom #To centimeters / Senttimetreiksi
data1b$top <- 0.1*data1b$top
data1b$diag <- 0.1*data1b$diag
results1bb <- prcomp(data1b)
PCdata1bb <- data.frame(as.matrix(data1b) %*% results1bb$rotation)
plot(PCdata1bb, col = z) # Clearly different from the previous plot / Poikkeaa selv???sti edellisest??? kuvasta

#Very bad - be careful!
# Principal components analysis is sensitive to changes in measurement units
# P??????komponenttianalyysi on herkk??? yksikk???muunnoksille


# Outer measurements of the bills / Setelien ulkomitat
# As data1 is in data.frame form, data1[1:3] is the same than data1[,1:3] as each
#   column is a variable. If data1 was in matrix form only data1[,1:3] would be acceptable
# Koska data1 on data.frame -muodossa, data1[1:3] ja data1[,1:3] tarkoittavat samaa, sill??? jokainen
#   sarake on muuttuja. Jos data1 olisi matriisimuodossa, vain data1[,1:3] kelpaisi
results1b <- prcomp(data1[1:3])
summary(results1b)
#1st principal component explains 61,5 % of the total variation
#1. p??????komponentti selitt?????? 61,5 % kokonaisvaihtelusta

plot(results1b)
results1b$rotation #Loading matrix / Latausmatriisi
PCdata1b <- data.frame(as.matrix(data1[1:3]) %*% results1b$rotation)
plot(PCdata1b, col = z) #Plot of the principal components / Kuva p??????komponenteista

plot(data1[1:3], col = z) #Plot of the original variables / Kuva alkuper???isist??? muuttujista
cor(data1[1:3], PCdata1b)
#PC1: Very high correlations with left (0.91) and right (0.92)
#PC1: Hyvin suuret korrelaatiot left- (0.91) ja right-muuttujien (0.92) kanssa


# Inner measurements of the bills / Setelien sis???mitat
results1c <- prcomp(data1[4:6])
summary(results1c)
plot(results1c)
#1st principal component explains 71,5 % of the total variation
#1. p??????komponentti selitt?????? 71,5 % kokonaisvaihtelusta

results1c$rotation
PCdata1c <- data.frame(as.matrix(data1[4:6]) %*% results1c$rotation)
plot(PCdata1c, col = z)
#Clear differences between billtypes in principal component PC1
#Selvi??? eroja setelityyppien v???lill??? p??????komponentissa PC1

plot(data1[4:6], col = z)
cor(data1[4:6], PCdata1c)




#2. Find the principal variables for the Fisher's Iris data. Then find the
#principal variables separately for sepal measurements and petal
#measurements and plot the results to compare different species.

# 2: IRIS (PCA)
#
# All variables
results2 <- prcomp(data2[2:5])
summary(results2)
#The first principal component explains 92,5 % of the total variation! Probably enough for further analysis.
#Ensimm???inen p??????komponentti selitt?????? 92,5 % kokonaisvaihtelusta! Ehk???p??? riitt???v??? jatkoanalyyseihin.

plot(results2)
PCdata2 <- data.frame(as.matrix(data2[2:5]) %*% results2$rotation)
plot(PCdata2, col = species) #PC1 seperates the species pretty well / PC1 erottelee lajit melko hyvin
plot(data2[2:5], col = species)
round(cor(data2[2:5], PCdata2), digits = 2) #Correlations between original variables and principal components
#PC1: Very high correlations with three original variables
#PC1: Hyvin suuret korrelaatiot kolmen alkuper???isen muuttujan kanssa


# Sepal measurements / Verholehtimittaukset
results2b <- prcomp(data2[2:3])
summary(results2b)
plot(results2b)
PCdata2b <- data.frame(as.matrix(data2[2:3]) %*% results2b$rotation)
plot(PCdata2b, col = species)
plot(data2[2:3], col = species)
#Almost no change between the plots of original variables and principal components
#Ei melkein yht??????n eroa alkuper???isten muuttujien ja p??????komponenttien v???lill???
results2b$rotation
cor(data2[2:3], PCdata2b)
#Correlation (~1) with PC1 and sepall / PC2 and sepalw explains the lack of major changes in plots
# Muuttujien PC1 ja sepall / PC2 ja sepalw v???linen korrelaatio (~1) selitt?????? kuvien eron pienuuden


# Petal measurements / Ter???lehtimittaukset
results2c <- prcomp(data2[4:5])
summary(results2c)
#First principal component basically explains all the variation
#Ensimm???inen p??????komponentti selitt?????? k???yt???nn???ss??? koko vaihtelun

plot(results2c)
PCdata2c <- data.frame(as.matrix(data2[4:5]) %*% results2c$rotation)
plot(PCdata2c, col = species) #Differentiates the species well / erottelee lajit hyvin
plot(data2[4:5], col = species)
cor(data2[4:5], PCdata2c)
#Both variables correlates VERY highly with the first principal components
# Use only the first principal component for possible further analysis
#Molemmat muuttujat korreloivat HYVIN vahvasti ensimm???isne p??????komponentin kanssa
# K???yt??? vain ensimm???ist??? p??????komponenttia mahdollisiin lis???analyyseihin.


#3. In the Swiss Bank Notes data, consider the linear dependence between
#outer and inner measurements.  Perform CCA (canonical correlation analysis) separately for genuine and forged bills.

#CANONICAL CORRELATION (CCA) / Kanoninen korrelaatio (CCA)
# Maximization of the correlation between the two sets of variables
# Kahden muuttujaryhm???n korrelaation maksimointi

# 3 (CCA)
# WHOLE SWISS BANK NOTES DATA SET
x <- data1[1:3]
y <- data1[4:6]
results<- cancor(x, y)
results
#cor: Correlations between X1 and Y1, X2 and Y2, X3 and Y3
#     Uusien muututjien X1 ja Y1, X2 ja Y2 sek??? X3 ja Y3 v???liset korrelaatiot
#xcoef, ycoef: see lecture notes Chapter 4, page 10 / Katso luentomateriaalin (engl.) luku 4, sivu 10
#xcenter, ycenter: Means of the variables used for centering in CCA
#                   Muuttujien keskiarvot keskist???mist??? varten analyysiss???

# Repeat 1 as many times as there are observations per variable
# Toista luku 1 aineiston rivien lukum??????r???n verran
en <- c(rep(1, nrow(x)))

CCdata1x <- data.frame((as.matrix(x) - en %*% t(results$xcenter)) %*% results$xcoef)
CCdata1y <- data.frame((as.matrix(y) - en %*% t(results$ycenter)) %*% results$ycoef)
CCData1 <- cbind(CCdata1x, CCdata1y)
names(CCData1) <- c('X1', 'X2', 'X3', 'Y1', 'Y2', 'Y3')

plot(data1, col = z) #For comparison / Vertailun vuoksi
plot(CCData1, col = z)
#Probably the pair (X1, Y1) is enough for furher analysis (dimension reduction)
#Todenn???k???isesti muuttujapari (X1, Y1) on riitt???v??? jatkoanalyyseihin (dimension supistaminen)

round(cor(data1, CCData1), 3)
#X-variables were constructed by first three variables and y-variables by last three
#X-muuttujat luotiin kolmesta ensimm???isest??? ja y-muuttujat kolmesta viimeisest??? muuttujasta
# The interesting correlations / Mielenkiintoiset korrelaatiot:
#
#           X1     X2     X3     Y1     Y2     Y3
#length -0.287 -0.428  0.857
#left    0.777  0.159  0.609
#right   0.860 -0.450  0.240
#bottom                       0.826 -0.120 -0.550
#top                          0.628 -0.247  0.738
#diag                        -0.912 -0.379 -0.156
# Check ther dependencies between the new canonical variables
# Tarkastele uusien kanonisten muuttujien v???lisi??? yhteyksi???
round(cor(CCData1), 3)


# GENUINE BILLS
xb <- bs1[1:3]
yb <- bs1[4:6]
resultsb <- cancor(xb, yb)
resultsb
en <- c(rep(1, nrow(xb)))
CCdata1xb <- data.frame((as.matrix(xb) - en %*% t(resultsb$xcenter)) %*% resultsb$xcoef)
CCdata1yb <- data.frame((as.matrix(yb) - en %*% t(resultsb$ycenter)) %*% resultsb$ycoef)
CCData1b <- cbind(CCdata1xb, CCdata1yb)
names(CCData1b) <- c('X1', 'X2', 'X3', 'Y1', 'Y2', 'Y3')

plot(bs1)
plot(CCData1b)
round(cor(bs1, CCData1b), 3)
round(cor(CCData1b), 3) #Correlations between the canonical variables


# FORGED BILLS
xc <- bs2[1:3]
yc <- bs2[4:6]
resultsc <- cancor(xc, yc)
resultsc
en <- c(rep(1, nrow(xc)))
CCdata1xc <- data.frame((as.matrix(xc) - en %*% t(resultsc$xcenter)) %*% resultsc$xcoef)
CCdata1yc <- data.frame((as.matrix(yc) - en %*% t(resultsc$ycenter)) %*% resultsc$ycoef)
CCData1c <- cbind(CCdata1xc, CCdata1yc)
names(CCData1c) <- c('X1', 'X2', 'X3', 'Y1', 'Y2', 'Y3')

plot(bs2)
plot(CCData1c)
round(cor(bs2, CCData1c), 3)
round(cor(CCData1c), 3)


#4. Consider the linear dependence between petal and sepal measurements in the Fisher's Iris data. Perform CCA.

# 4 (CCA)
# WHOLE IRIS DATASET
x2 <- data2[2:3]
y2 <- data2[4:5]
results2 <- cancor(x2, y2)
results2
#There is a high correlation (0.94) between sepal and petal measurements (canonical variables X1 and Y1)
#Ter???- ja verholehtien muuttujilla (kanoniset muuttujat X1 ja Y1) on hyvin korkea korrelaatio (0.94)

en <- c(rep(1, nrow(x2)))
CCdata2x <- data.frame((as.matrix(x2) - en %*% t(results2$xcenter)) %*% results2$xcoef)
CCdata2y <- data.frame((as.matrix(y2) - en %*% t(results2$ycenter)) %*% results2$ycoef)
CCData2 <- cbind(CCdata2x, CCdata2y)
names(CCData2) <- c('X1', 'X2', 'Y1', 'Y2')

plot(data2[2:5], col = species)
plot(CCData2, col = species)
#Very high correlation between X1 and Y1 is easily visible in the figure
#Muuttujien X1 ja Y1 korkea korrelaatio on helppoa n???hd??? kuvasta
round(cor(data2[2:5], CCData2), 3)
#Interesting correlations / mielenkiintoiset korrelaatiot:
#           X1    X2     Y1    Y2
#sepall -0.929 0.370
#sepalw  0.477 0.879
#petall              -0.990 0.143
#petalw              -0.914 0.405

# Correlations between canonical variables / kanonisten muuttujien v???liset korrelaatiot
round(cor(CCData2), 3)


#Extra stuff / Ylim??????r???ist??? asiaa
# SPECIES 1
x2b <- sp1[1:2]
y2b <- sp1[3:4]
results2b <- cancor(x2b, y2b)
results2b
en <- c(rep(1, nrow(x2b)))
CCdata2xb <- data.frame((as.matrix(x2b) - en %*% t(results2b$xcenter)) %*% results2b$xcoef)
CCdata2yb <- data.frame((as.matrix(y2b) - en %*% t(results2b$ycenter)) %*% results2b$ycoef)
CCData2b <- cbind(CCdata2xb, CCdata2yb)
names(CCData2b) <- c('X1', 'X2', 'Y1', 'Y2')

plot(sp1)
plot(CCData2b)
round(cor(sp1, CCData2b), 3)
round(cor(CCData2b), 3)  #Cor(X1,Y1) = 0.334


# SPECIES 2
x2c <- sp2[1:2]
y2c <- sp2[3:4]
results2c <- cancor(x2c, y2c)
results2c
en <- c(rep(1, nrow(x2c)))
CCdata2xc <- data.frame((as.matrix(x2c) - en %*% t(results2c$xcenter)) %*% results2c$xcoef)
CCdata2yc <- data.frame((as.matrix(y2c) - en %*% t(results2c$ycenter)) %*% results2c$ycoef)
CCData2c <- cbind(CCdata2xc, CCdata2yc)
names(CCData2c) <- c('X1', 'X2', 'Y1', 'Y2')

plot(sp2)
plot(CCData2c)
round(cor(sp2, CCData2c), 3)
round(cor(CCData2c), 3) #Cor(X1,Y1) = 0.78


# SPECIES 3
x2d <- sp3[1:2]
y2d <- sp3[3:4]
results2d<- cancor(x2d, y2d)
results2d
en <- c(rep(1, nrow(x2d)))
CCdata2xd <- data.frame((as.matrix(x2d) - en %*% t(results2d$xcenter)) %*% results2d$xcoef)
CCdata2yd <- data.frame((as.matrix(y2d) - en %*% t(results2d$ycenter)) %*% results2d$ycoef)
CCData2d <- cbind(CCdata2xd, CCdata2yd)
names(CCData2d) <- c('X1', 'X2', 'Y1', 'Y2')

plot(sp3)
plot(CCData2d)
round(cor(sp3, CCData2d), 3)
round(cor(CCData2d), 3)  #Cor(X1,Y1) = 0.864

#5. Perform factor analysis (FA) for the whole Swiss Bank Notes data,
#and then separately for genuine bills and for forged bills. Compare the results.


# Factor analysis / Faktorianalyysi
# 5 Swiss Bank Note data

# The whole data / Koko data
#
# One factor / Yksi faktori
factanal(data1, factors = 1)

#Uniqueness: specific variance to the variable (i.e. the variance not shared with other variables).
# The greater the uniqueness is (i.e. the lower the communality is)
#   the lower the relevance the variable has in the model (random error variances are high)
# Uniqueness = variability - communality
#Uniqueness: Muuttujan ominaisvarianssi (eli varianssi, jota ei jaettu muiden muutujien kanssa)
# Mit??? suurempi t???m??? arvo on (eli mit??? pienempi kommunaliteetti on),
#   sit??? v???hemm???n muuttujalla on merkityst??? mallissa (ominaisfaktoreiden / virhetermien varianssi suuri)
# Ominaisvarianssi = Vaihtelu - kommunaliteetti
#
#SS loadings = loadings squared and added together, i.e. sums of squared of the loadings
#SS loadings = lataukset korotettuna toisene potenssiin ja summattu, eli latausten neli???summat
#   (1.587 = (-0,18)^2 + 0.351^2 + 0.427^2 + ...)
#
#Proportion of variance explained: 40,9 %
#Varianssin selitysosuus: 40,9 %
#
#Test of the hypothesis: Is 1 factor enough? P-value < 0,001 => hypothesis rejected => 1 factor not enough
#Hypoteesin testi: onko 1 faktori tarpeeksi? P-arvo < 0,001 => hyl???t??????n hypoteesi => 1 faktori ei tarpeeksi


factanal(data1, factors = 2)
factanal(data1, factors = 3)
# No more then 3 factors possible
# Enemm???n kuin 3 faktoria ei mahdollista


# Genuine bills
factanal(bs1, factors = 1) 
factanal(bs1, factors = 2)
#Test of the hypothesis: 2 factors may be enough (hypothesis not rejected: p-value 0.146 > 0.05)
#Hypoteesin testi: 2 faktoria saattaa riitt?????? (hypoteesia ei voitu hyl???t???: p-arvo 0,146 > 0,05)
#
#
#The idea of rotation: Simplest possible structure is wanted -> easier to interpret
#Rotaation idea: Halutaan mahdollisimman yksinkertainen rakenne, jota on helppo tulkita

# Varimax rotation is default. Let's try promax (it preserves the variance of the factors)
# Varimax-rotaatio on oletuksena. Testataan promax-rotaatiota (s???ilytt?????? tiedot faktorien kovariansseista)
factanal(bs1, factors = 2, rotation = "promax")
#Results differ a bit and you get the correlations between the factors too
# Correlation = 0.068 doesn't seem that high
# Proportion of variance explained by these factors if 55.4 %
#Tulokset muuttuvat hieman ja lis???ksi tuloksissa on faktorien v???liset korrelaatiot
# Korrelaatio = 0,068 ei vaikuta kovin suurelta
# Kahden faktorin selitt???m??? osuus varianssista on 55,4 %


# You can still check what happens if you add one more factor
# Edelleen voi katsoa mit??? tapahtuu, jos lis?????? uuden faktorin
factanal(bs1, factors = 3)
#Uniqueness of the variable diag is now close to zero (previously close to one) - now meaningful (factor 3)
#diag-muuttujan ominaisvarianssi on nyt l???hell??? nollaa (aiemmin l???hell??? yht???) - nyt merkityst??? (faktori 3)

#Forged bills
factanal(bs2, factors = 1)
factanal(bs2, factors = 2) #p-value still small / p-arvo edelleen pieni
factanal(bs2, factors = 3)


#Genuine vs. forged:
# Top and bottom variables have high loadings to same factor in the models (with 2 or 3 factors)
#Aidot vs. v??????rennetyt:
# Top- ja bottom-muuttujilla on korkeat lataukset samaan faktoriin malleissa (joissa 2 tai 3 faktoria)

#6. Try to perform factor analysis (FA) for the Fisher's Iris data. Why doyou get problems in the estimation of parameters?


# 6 Fisher's Iris data - Factor analysis

#The whole data
factanal(data2[2:5], factors = 1)
factanal(data2[2:5], factors = 2)
#The number of the variables is too low compared to number of factors -> parameter estimation does not work
#Liian v???h???n muuttujia v???hint??????n 2 faktorille -> parametrien estimointi ei onnistu