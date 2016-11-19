setwd("C:/Users/JMFlin/Desktop/R/R files/Multivariate Methods")

#1.  Consider  the  univariate  distributions  in  the  Swiss  Bank  Notes  data
#with  the  moments  (means,  variances,  skewness,  kurtosis),  box  plots
#and histograms.  Which variables tend to be bimodal?

# Make sure you have the following four (4) files in your working directory:
# -  bank.dat
# -  iris.dat
# -  own_functions.r
# -  read_and_analyse.r

# Input prespecified own functions
source("own_functions.r")


# Input the both datasets (code from read_and_analyse.r)
data1<-data.frame(read.table('bank.dat'))
length<-data1[,1] #First column of data1 / ensimm???inen sarake data1-aineistosta
left<-data1[,2]
right<-data1[,3]
bottom<-data1[,4]
top<-data1[,5]
diag<-data1[,6]
# Set names for the variables
# Asetetaan muuttujille nimet
names(data1)<-c('length', 'left', 'right', 'bottom', 'top', 'diag') 

data2<-data.frame(read.table('iris.dat'))
species<-data2[,1]
sepall<-data2[,2]
sepalw<-data2[,3]
petall<-data2[,4]
petalw<-data2[,5]
names(data2)<-c('species', 'sepall', 'sepalw', 'petall', 'petalw')


#1 Swiss Bank Note data
# Let's consider univariate distributions in this data

# Means of the variables (two ways to do this)
colMeans(data1)
sapply(data1, mean)

#sapply-function returns a vector of the certain function (in previous case: mean vector)

# Variances of the variables
sapply(data1, var)

# Skewness for every variable + comparison to zero skewness (=symmetric)
sapply(data1, b1) 

#b1 (and many others from now on) is a self-made function in the file own_functions.r


# Kurtosis for every variable + comparison to kurtosis 3 (=kurtosis of the normal distribution)
sapply(data1, b2) #b2 --||--

# Box plots with box plots from simulated normal distributions with same means and standard deviations
par(mfrow = c(3, 2)) #Many plots in one picture / Useampi kuva yhteen kuvaan
mapply(boxplot.with.normal, data1, main = colnames(data1))
par(mfrow = c(1, 1)) #Reset to default / Palautetaan oletusasetukseen


# Histograms with normal distribution curves with same means and standard deviations
par(mfrow=c(3,2))
mapply(hist.with.normal, data1, main = colnames(data1))
par(mfrow=c(1,1))

#At least the distribution of bottom and especially the distribution of diag seem to be bimodal
#Ainakin muuttujan bottom ja erityisesti muuttujan diag jakaumat vaikuttavat kaksihuippuisilta


#2.  Consider the univariate distributions in the Fisher's Iris data with the
#moments  (means,  variances,  skewness,  kurtosis),  box  plots  and  histograms.  Which variables tend to be bi- or multimodal?

#2 Fisher's Iris data
# Same thing than in #1, different data

# Means / Keskiarvot
#First variable is defines the species (categorical) - you can use only columns 2-5 from data2
colMeans(data2[2:5]) 

#Variances / Varianssit
sapply(data2[2:5], var)

#Skewness / Vinous
sapply(data2[2:5], b1)

#Kurtosis / Huipukkuus
sapply(data2[2:5], b2)

#Box plots / Laatikko-jana-kuviot
par(mfrow = c(2, 2)) #Many plots in one picture / Useampi kuva yhteen kuvaan
mapply(boxplot.with.normal, data2[2:5], main = colnames(data2[2:5]))
par(mfrow = c(1, 1))

#Histograms / Histogrammit
par(mfrow = c(2, 2)) #Many plots in one picture / Useampi kuva yhteen kuvaan
mapply(hist.with.normal, data2[2:5], main = colnames(data2[2:5]))
par(mfrow = c(1, 1))

#Clearly the distributions of petall and petalw are at least bimodal
#Selvasti muuttujien petall ja petalw jakaumat ovat ainakin kaksihuippuisia


#3.  Consider  interesting  bivariate  distributions  in  the  Swiss  Bank  Notes
#data by plotting the observed values together with 50 and 90 % tolerance ellipses of the distribution.

#3
#Scatterplots (Swiss Bank Note data) / Sirontakuviot

# z: First 100 are genuine, last 100 are forged
z <- c(rep(1, 100), rep(2, 100))
# Let's lot the whole data
plot(data1)
# Plot data with different colors to genuine and  forged bills
plot(data1, col = c("red", "blue")[z]) #Genuine and forged seperately

# Scatterplots with tolerance ellipses
# Sirontakuviot toleranssiellipsien kanssa

#Left vs. right
bivariateplot(data1$left, data1$right) #All / Kaikki
bivariateplot(data1$left[1:100], data1$right[1:100]) #Genuine / Aidot
bivariateplot(data1$left[101:200], data1$right[101:200]) #Forged / Vaarennetyt

#Top vs. bottom
bivariateplot2(data1$top, data1$bottom, c(7,13), c(7,13), "black") #All / Kaikki
bivariateplot2(data1$top[1:100], data1$bottom[1:100], c(7, 13), c(7, 13), "red") #Genuine / Aidot
par(new = TRUE)
bivariateplot2(data1$top[101:200], data1$bottom[101:200],c(7, 13),c(7, 13), "blue") #Forged / Vaarennetyt
# All values together: Huge ellipses
#   Values seperately: smaller ellipses that do not touch each other
#   Specifications of forged bills seem to differ from genuine bills
# Kaikki arvot samassa: isot ellipsit
#   Arvot erikseen: Pienemmat ellipsit, jotka eivat kosketa toisiaan
#   Vaaennettyjen setelien ominaisuudet nayttaisivat eroavan aitojen setelien ominaisuuksista


#4.  Consider  interesting  bivariate  distributions  in  the  Fisher's  Iris  data
#by plotting the observed values together with 50 and 90 % tolerance ellipses of the distribution.

#4
# Same things than in #3, with Iris data
# Samat asiat kuin tehtavassa #3, Iris-datalla
plot(data2[2:5])
plot(data2[2:5],col = c("red", "blue", "green")[species])

#petall vs. petalw
bivariateplot2(data2$petall, data2$petalw, c(1, 7), c(0, 2.5)) #All

bivariateplot2(data2$petall[species == 1], data2$petalw[species == 1], c(1, 7), c(0, 2.5), "green") #Species 1 / laji 1
par(new = TRUE)
bivariateplot2(data2$petall[species == 2], data2$petalw[species == 2], c(1, 7), c(0, 2.5), "blue") #Species 2 / laji 2
par(new = TRUE)
bivariateplot2(data2$petall[species == 3], data2$petalw[species == 3], c(1, 7), c(0, 2.5), "red") #Species 3 / laji 3
# Again the same thing: All values together: Huge ellipses
#   Values seperately: smaller ellipses that only partly touch each other
#   Species seem to differ from each other by these variables
# Taas: Kaikki arvot samassa: isot ellipsit
#   Arvot erikseen: Pienemmat ellipsit, jotka koskettavat vain osin toisiaan
#   Lajit nayttavatroavan toisistaan naiden muuttujien osalta


#5.  Find the covariance and correlation matrices for the Swiss Bank Notes data.  
#Find the covariance and correlation matrices separately for genuine and forged bills.  Compare the matrices.

#5 Swiss Bank Note data - covariances and correlations / kovarianssit ja korrelaatiot

# Let's create dataset of genuine bills / Luodaan aineisto aidoista seteleistä
bs1 <- data1[1:100, ]
S1 <- cov(bs1) #Covariance matrix / Kovarianssimatriisi
S1
cor(bs1) #Correlation matrix / Korrelaatiomatriisi
round(S1, 4) #As many decimals as we want / Niin monta desimaalia kuin haluamme
round(cor(bs1), 4)

# Dataset of forged bills / Aineisto väärennetyistä seteleistä
bs2 <- data1[101:200, ]
S2 <- cov(bs2) #Covariance
S2
cor(bs2) #Correlation
round(S2, 4)
round(cor(bs2), 4)


#e.g.
# The variance of variable bottom is larger in forged bills (1.28) than in genuine bills (0.41)
#   (bottom = width of the bottom margin of the bill)
# The correlation between length and bottom is positive in genuine bills (0.22)
#   and negative in forged bills (-0.25)
#
#esim.
# bottom-muuttujan varianssi on suurempi väärennetyillä seteleillä (1.28) kuin aidoilla (0.41)
#   (bottom = setelin alaosan marginaalin leveys)
# Muuttujien bottom ja length korrelaatio aidoilla seteleillä positiivinen (0.22)
#   ja väärennetyillä negatiivinen (-0.25)


# The whole data / Koko data
#   Combined covariance estimate (See pages 7 and of Chapter 2)
#   Yhdistetty estimaatti kovarianssimatriisille (kts. sivut 7 ja 8 luvusta 2)
cov1 <- (1/198)*(99*cov(bs1) + 99*cov(bs2))
# Within groups correlation matrix
# Ryhmien sisäinen kovarianssimatriisi
round(cov1, 4)
round(cov2cor(cov1), 4) #Covariance to correlation / Kovarianssista korrelaatioksi


#6.  Find the covariance and correlation matrices for the Fisher's Iris data.
#Find the covariance and correlation matrices separately for the three species.  Compare the matrices.


#6 Fisher's Iris data - covariances and correlations

# Datasets of different species / Aineistot eri lajeista
# Subsets of the whole data / Osa-aineistot koko datasta
sp1 <- subset(data2[2:5], species == 1) #Species 1 - Laji 1
sp2 <- subset(data2[2:5], species == 2) #Species 2 - Laji 2
sp3 <- subset(data2[2:5], species == 3) #Species 3 - Laji 3

# Species 1
S1b <- cov(sp1) # Covariance / Kovarianssi
round(S1b, 4)
round(cor(sp1), 4) #Correlation / Korrelaatio

# Species 2
S2b <- cov(sp2)
round(S2b, 4)
round(cor(sp2), 4)

# Species 3
S3b <- cov(sp3)
round(S3b, 4)
round(cor(sp3), 4)

#e.g.
# The variance of sepall is smallest in species 1 (0.12) and largest in species 3 (0.40)
#   (sepall = sepal length)
# All the correlations between the varaibles are positive
# Species 1 has the highest correlation between sepal length and sepal width (0.74)
# Species 2 has generally quite high correlations between the variables (all > 0.5)
#
#esim.
# sepall-muuttujan varianssi on pienin lajilla 1 (0.12) ja suurin lajilla 3 (0.40)
#   (sepall = verholehden pituus)
# Kaikki muuttujien väliset korrelaatiot ovat positiivisia
# Lajin 1 kohdalla verholehden pituuden ja leveyden korrelaatio on suurin (0.74)
# Lajin 2 kohdalla kaikki korrelatiot ovat melko suuria (kaikki > 0.5)


# The whole data / Koko data
#   Combined covariance estimate sigma (See pages 11 and 12 of Chapter 2)
#   Yhdistetty estimaatti kovarianssimatriisille sigma (kts. sivut 11 ja 12 luvusta 2)
cov2 <- (1/147)*(49*cov(sp1) + 49*cov(sp2) + 49*cov(sp3))
# Within groups correlation matrix

round(cov2, 4)
round(cov2cor(cov2), 4)


#7.  In the Swiss Bank Notes data, the  rst 100 observations are for genuine bills and the the last 100 for forged bills.  
#Use Hotelling's T2 test to compare the distributions of genuine and forged bills.  
#In the comparisons, use (i) all 6 measurement, (ii) 3 outer measurements, (iii) 3 innermeasurements.

#7 Swiss Bank Note data - Hotellings T^2-test

# All six measurements / Kaikki kuusi mittausta
Hotelling2(bs1, bs2)

# 3 outer measurements / 3 ulkomittaa (length, left, right)
Hotelling2(bs1[1:3], bs2[1:3])

# 3 inner measurements / 3 sisämittaa (top, bottom, diag)
Hotelling2(bs1[4:6], bs2[4:6])

# T^2-statistic is larger in inner measurements than in outer measurements
# T^2-testisuure on suurempi sisämittojenkohdalla kuin ulkomittojen kohdalla

# z: First 100 are genuine, last 100 are forged (as before)
z <- c(rep(1, 100), rep(2, 100)) 
# Plot data with different colors to genuine and  forged bills (as before)
plot(data1, col = c("red", "blue")[z])

# There's a clearer difference between the two groups in inner measurements than in outer measurements
# Kahdella ryhmällä on selvemmät erot sisämitojen kohdalla kuin ulkomittojen kohdalla

#However, in all cases p-value < 0.001, i.e. the differences between the groups are statistically significant


#8.  To analyse the Fisher's Iris data, use multivariate analysis of variance (MANOVA) to compare the distributions of di erent species.
#In thecomparisons, use (i) all 4 measurement, (ii) 2 sepal measurements, (iii) 2 petal measurements.


#8 Fisher's Iris data - MANOVA

# All measurements / Kaikki havainnot
MANOVA(data2[1], data2[2:5])

# Sepal measurements / verholehden mitat
MANOVA(data2[1], data2[2:3])

# Petal measurements /
MANOVA(data2[1], data2[4:5])

# Q-statistic is larger in petal measurements than in sepal measurements
# Q-testisuure on suurempi ter???lehden mittojen kohdalla kuin verholehden mittojen kohdalla
# Plot of the data as before / Sirontauvio kuten aiemmin
plot(data2[2:5], col = c("red", "blue", "green")[species])

# There are clearer differences between the species in petal measurements than in sepal measurements

#However, in all cases p-value < 0.001, i.e. the differences between the species are statistically significant