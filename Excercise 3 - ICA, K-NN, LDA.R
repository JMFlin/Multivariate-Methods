setwd("C:/Users/JMFlin/Desktop/R/R files/Master's Data")

#1. For the Swiss Bank Notes data, find the independent components (with FOBI) 
#for the whole data and then separately for genuine bills and for forged bills. Compare the results.

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

#From previous practicals / aiemmista harjoituksista:
z <- c(rep(1, 100), rep(2, 100))
data1b <- data1
data1b$bottom <- 0.1*data1b$bottom #To centimeters / Senttimetreiksi
data1b$top <- 0.1*data1b$top
data1b$diag <- 0.1*data1b$diag

bs1 <- data1[1:100, ] #Real bills
bs2 <- data1[101:200, ] #Forged bills
sp1 <- subset(data2[2:5], species == 1) #Species 1 / Laji 1
sp2 <- subset(data2[2:5], species == 2) #Species 2
sp3 <- subset(data2[2:5], species == 3) #Species 3

#Note! / Huom.!
data2[2:5]
data2[-1]
#Same thing in this data: first chooses columns 2:5, the other removes the first column
#Sama asia t???ss??? aineistossa: ensimm???inen valitsee sarakkeet 2:5, toinen poistaa sarakkeen 1

# Check Packages- and Help-sections in RStudio (bottomright corner)
#   or google "R package package_name pdf" (where you replace package_name by the name of the package)
# Katso Packages (paketit) ja Help-osiot RStudiossa (oikea alakulma)
#   or googlaa "R package paketin_nimi pdf" (jossa korvaat paketin_nimi:n halutun paketin nimell???)

#R packages we are using today:
#T???n??????n k???ytett???v???t R-paketit:

#Package ICS for independent component analysis
#Paketti ICS riippumattomien komponenttien analyysiin
library(ICS)
#Alternative: FOBI function from package JADE
#Vaihtoehto: FOBI-funktio paketista JADE
library(JADE)
#You also need packages MASS (for function lda) and class (for function knn) later
#Tarvitset paketteja MASS (lda-funktiota varten) ja class (knn-funktiota varten) my???hemmin
library(MASS)
library(class)


options(digits = 3) #Less decimals in results / V???hemm???n desimaaleja tuloksissa


# Independent component analysis / Riippumattomien komponenttien analyysi (ICA)
# FOBI = Fourth Order Blind Identification
# ICS = Invariant coordinate selection
#
# 1 Swiss Bank Note data
# The whole data / koko data
#
# The idea in ICA is to find an estimate for an unmixing matrix
#   that backtransforms the observations to latent independent components.
# ICA:n idea on l???yt?????? matriisi (unmixing matrix), jolla havainnot saadaan "muunnettua takaisin"
#   taustalla oleviksi piileviksi (latenteiksi) riippumattomiksi komponenteiksi
ICS1 <- ics(data1)
summary(ICS1)
# Kurtosis values in order from highest to lowest (all kurtosis values multiplied equals to 1)
# Huipukkuudet j???rjestyksess??? korkeimmasta matalimpaan (kaikki huipukkuudet kesken??????n kerrottuna = 1)

plot(ICS1, col = z)
#You can see greater spreading of values in the last components than in first one
#  Reveals that in independent component IC.6 consists of two distinct populations
#  This method may also reveal the outliers
#Viimeisten komponenttien kohdalla havainnot ovat levitt???ytyneet paljon enemm???n kuin ensimm???isiss???
#  Paljastaa sen, ett??? riippumaton komponentti IC.6 koostuu kahdesta erillisest??? populaatiosta
#  Menetelm???ll??? voidaan l???yt?????? my???s mm. poikkeavia havaintoja

plot(data1, col = z) #For comparison / Vertailun vuoksi

indc1 <- ics.components(ICS1) #Extract the components / poimi komponentit
# Correlations between original variables and independent components
# Alkuper???isten muuttujien ja riippumattomien komponenttien v???liset varianssit
cor(data1, indc1) 


# Different measurement units don't affect the results / Eri mittausyksik???t eiv???t muuta tuloksia
ICS11 <- ics(data1b)
summary(ICS11)
plot(ICS11, col = z)


#Genuine bills
ICS1b <- ics(bs1)
summary(ICS1b)
plot(ICS1b)
indc1b <- ics.components(ICS1b)
cor(bs1, indc1b)

#Forged bills
ICS1c <- ics(bs2)
summary(ICS1c)
plot(ICS1c)
indc1c <- ics.components(ICS1c)
cor(bs2, indc1c)

#There's greater differences in spreading between the independent components in forged bills
# (see the kurtosis values and the scatterplot matrix)
#   Forged: IC.1: packed together nicely, with some outliers, IC.6: spreaded across the range
#   Genuine: some outliers in IC.1?
#
#V??????rennettyjen rahojen kohdalla on enemm???n eroja havaintojen jakautumisessa eri komponettien v???lill???
# (katso huipukkuusarvot ja sirontakuviomatriisi)
#   V??????rennetyt: IC.1: pakkautuneet yhteen, jokunen poikkeava havainto, IC.6: levitt???ytyneet koko v???lille
#   Aidot: poikkeavia havaintoja komponentissa IC.1?


#2. For the Fisher's Iris data, find the independent components with FOBI.

# 2 Fisher's Iris data (ICA)
#
# All data / Koko aineisto
ICS2 <- ics(data2[2:5])
summary(ICS2)
plot(ICS2, col = species)
plot(data2[2:5], col = species) # For the comparison / vertailun vuoksi
indc2 <- ics.components(ICS2)
cor(data2[2:5], indc2)
#4th independent component reveals grouping (lowest kurtosis)
#4. riippumaton komponentti paljastaa ryhmittelyn (alin huipukkuus)

# Species 1 / Laji 1
ICS2b <- ics(sp1)
summary(ICS2b)
plot(ICS2b)
indc2b <- ics.components(ICS2b)
cor(sp1, indc2b)

# Species 2 / Laji 2
ICS2c <- ics(sp2)
summary(ICS2c)
plot(ICS2c)
indc2c <- ics.components(ICS2c)
cor(sp2, indc2c)

# Species 3 / Laji 3
ICS2d <- ics(sp3)
summary(ICS2d)
plot(ICS2d)
indc2d <- ics.components(ICS2d)
cor(sp1, indc2d)




# Some useful vectors for choosing the train and test datas 
# Muutama hy???dyllinen vektori opetus- ja testiaineiston valintaan
tr1 <- c(1:50, 101:150)
tes1 <- c(51:100, 151:200)
tr2 <- c(1:25, 51:75, 101:125)
tes2 <- c(26:50, 76:100, 126:150)

#3. For the Swiss Bank Notes data, use training set and test set to compare Fisher's linear discriminant analysis (LDA) 
#and k-nearest neighbor classification.

# 3 Swiss Bank Note data

# Linear Discriminant Analysis / Lineaarinen erotteluanalyysi

# TRAINING SET AND TEST SET / OPETUSAINEISTO JA TESTIAINEISTO
train <- tr1 #Train data (first 50 obs. from each group) / Opetusaineisto (50 ensimm???ist??? havaintoa molemmista)
test <- tes1 #Test data (last 50 obs. from each group) / Testiaineisto (50 viimeist??? havaintoa molemmista)
lda.results <- lda(data1[train, ], z[train]) #The analysis / Analyysi
lda.results
#Before any rules, prior probabilities to each group is 1/2 (both classes have 50 observations in use)
#  Group means: the means of the variables to genuine and forged bills seperately (now 50 observations each)
#  Coefficients of linear discriminants: Coefficients for transforming observations to discriminant functions
#
#Ennen s??????nt???j??? todenn???k???isyys kuulua tiettyyn ryhm??????n on 1/2 (molemmista luokista k???ytet??????n 50 havaintoa)
#  Group means: Muuttujien keskiarvot aidoille ja v??????rille seteleille (t???ss??? molemmista 50 havaintoa)
#  Coefficients of linear ...: Kertoimet, joilla saadaan alkuper???iset havainnot muunnettua erottelufunktioiksi

lda.pred <- predict(lda.results, data1[test, ])$class #Predictions for test data / Ennusteet testiaineistolle
names(lda.pred) <- rownames(data1[test, ])
lda.pred #The predictions for each observation in test data / Ennusteet jokaiselle testiaineiston havainnolle

table(lda.pred, z[test]) #Classification table / Luokittelutaulukko
#In this case, all observations from group 2 (forged bills) is correctly classified
#  One observation from group 1 (genuine bills) if misclassified (observation number 70), others correctly
#T???ss??? tapauksessa kaikki ryhm???n 2 (v??????rennetyt setelit) tulee luokiteltua oikein
#  Yksi havainto ryhm???st??? 1 (aidot setelit) tulee luokiteltua v??????rin (havainto nro 70), muut oikein

# Observation number 70 (genuine) - compare to means of the variables of genuine and forged bills
# Havaintonumero 70 (aito) - vertaa aitojen ja v??????rien seteleiden muuttujien keskiarvoihin
data1[70, ]
colMeans(data1[1:100, ]) #All genuine bills / kaikki aidot setelit
colMeans(data1[101:200, ]) #All forged bills / kaikki v??????rennetyt setelit

plot(data1[test, ], col = lda.pred)
plot(data1[test, ], col = z[test]) #For comparison / vertailun vuoksi


# k-nearest neighbour classification / k:n l???himm???n naapurin menetelm???
#TRAINING SET AND TEST SET (usually distributed e.g. 80% - 20%, here 50-50)
#OPETUSAINEISTO JA TESTIAINEISTO (yleens??? jaetaan esim. 80% - 20%, t???ss??? 50-50)
train <- data1[tr1, ]
test <- data1[tes1, ]
bills.train <- z[tr1]
# Try first with k = 3 (three closest neighbours used for classification)
# Kokeillaan ensin arvoa k = 3 (kolmea l???hint??? naapuria k???ytet??????n luokitteluun)
knn.pred <- knn(train, test, cl = bills.train, k = 3, prob = TRUE)

# Computed classes (top) and probabilities (in this case all 1's) that
#   they belong to that class (bottom)
# Lasketut ryhm???t (yl???osa) ja todenn???k???isyydet (t???ss??? kaikki ykk???si???) sille,
#   ett??? se on sijoitettu kyseiseen ryhm??????n (alaosa)
knn.pred 
table(knn.pred, z[tes1]) #Classification table / Luokittelutaulukko
#Again, one observation from group 1 misclassified
#T???ss???kin yksi havainto ryhm???st??? 1 luokittuu v??????rin


# Now try different k's (e.g. 6) / Kokeile nyt muita k:n arvoja (esim. 6)
knn.pred <- knn(train, test, cl = bills.train, k = 6, prob = TRUE) #No class changes / ei muutoksia luokkiin
knn.pred
table(knn.pred, z[tes1])

#LDA vs. k-nearest neighbour - which is better? - No difference in this example
#LDA vs. k:n l???himm???n naapurin menetelm??? - kumpi on parempi? - Ei eroa t???ss??? esimerkiss???



#4. For the Fisher's Iris data, use training set and test set to compare
#Fisher's linear discriminant analysis (LDA) and k-nearest neighbor classification.

# 4 Fisher's Iris data

# Linear Discriminant Analysis / Lineaarinen erotteluanalyysi
train <- tr2 #Train data (1/2 observations from each group) / Opetusaineisto (1/2 jokaisen ryhm???n havainnoista)
test <- tes2 #Test data (---||---) / Testiaineisto (---||---)
lda.results <- lda(data2[train, 2:5], data2[train, 1])
lda.results
#Again, every group has the same number of observations (probability 1/3) and group means are presented
#  Now three groups classified: more than one discriminant function may be used (See coefficients LD1, LD2)
#  99,37 % of the separation into classes can be achieved with first discriminant function
#J???lleen kaikissa ryhmiss??? yht??? paljon havaintoja (tod.n???k. 1/3) ja ryhm???keskiarvot esitet??????n
#  Nyt kolme ryhm?????? eroteltavana: Erottelufunktioita voi olla useampi kuin yksi (katso kertoimet LD1, LD2)
#  99,37 % erottelusta eri luokkiin saadaan aikaan yhdell??? erottelufunktiolla



lda.pred <- predict(lda.results, data2[test, 2:5])$class #Predictions for test data / Ennusteet testiaineistolle
names(lda.pred) <- rownames(data2[test, ])
lda.pred
table(lda.pred, species[test]) #3 misclassifications / 3 v??????rinluokittelua

plot(data2[test, 2:5], col = lda.pred)
plot(data2[test, 2:5], col = species[test]) #For comparison / vertailun vuoksi

# k-nearest neighbour classification
#WITH TRAINING SET AND TEST SET
train <- data2[tr2, 2:5]
test <- data2[tes2, 2:5]
species.train<- data2[tr2, 1]
knn.pred <- knn(train, test, cl=species.train, k = 3, prob=TRUE)
knn.pred
table(knn.pred, species[tes2])
# 5 or 6 misclassifications: 59th observation goes to group 2 or group 3 with a probability of 0.5
#   R randomly selects one of them, hence you may get different result
#   If the probability if > 0.5 then observation naturally goes to one class every time
# 5 tai 6 v??????rinluokittelua: 59:s havainto luokittuu ryhm??????n 2 tai ryhm???n 3 todenn???k???isyydell??? 0,5
#   R arpoo sen satunnaisesti jompaankumpaan ryhm??????n
#   Mik???li todenn???k???isyys on yli 0,5, niin havainto luonnollisesti menee yhteen luokkaan joka kerta

knn.pred <- knn(train, test, cl = species.train, k = 5, prob = TRUE)
knn.pred
table(knn.pred,species[tes2]) # 6 misclassifications / 6 v??????rinluokittelua

knn.pred <- knn(train, test, cl = species.train, k = 7, prob = TRUE)
table(knn.pred,species[tes2]) # 6 misclassifications / 6 v??????rinluokittelua

knn.pred <- knn(train, test, cl = species.train, k = 8, prob = TRUE)
knn.pred
table(knn.pred,species[tes2]) # 3-5 misclassifications / 3-5 v??????rinluokittelua
#Two probabilities 0.5 -> results vary
#Kaksi todenn???k???isyytt??? 0,5 -> tulokset vaihtelevat

#LDA vs. k-nearest neighbour - which is better? - Maybe LDA in this example
#LDA vs. k:n l???himm???n naapurin menetelm??? - kumpi on parempi? - Ehk??? LDA t???ss??? esimerkiss???

#All of the species 1 correctly classified every time (differs from other more than others from each other)
#Kaikki lajin 1 havainnot luokiteltiin oikeen joka kerta (eroaa muista enemm???n kuin muut toisistaan)

#5. For the Swiss Bank Notes data, use hierarchical clustering and k-means clustering to find two, three and four clusters in the data.

# 5 Swiss Bank Note data
#
# HIERARCHICAL CLUSTERING / HIERARKKINEN KLUSTEROINTI (tai ryv???st???minen / ryhmittely)
h <- hclust(dist(data1), method = "average")
# The dendrogram (a "tree" that reveals branching, where clusters are divided to subclusters)
# Dendrogrammi (puu, jossa n???kee haarautumat, jossa klusterit jakautuvat pienempiin osiin)
plot(h)

# Cut the tree so that we get two clusters
# Leikataan puu siten, ett??? saadaan kaksi klusteria
h2 <- cutree(h, k = 2)
names(h2) <- rownames(data1)
h2
#Only the observation 70 (again..) belongs to "wrong" cluster
#Vain havainto 70 (taas..) ryhmittyy "v??????r??????n" ryhm??????n
plot(data1, col = z) #Original variables / Alkuper???iset muuttujat
plot(data1, col = h2) #Almost the same than the previous plot / L???hes samanlainen kuin edellinen kuva

h3 <- cutree(h, k = 3) #Three clusters / Kolme klusteria
names(h3) <- rownames(data1)
h3
#Observation 5 makes its own cluster - not good!
#Havainto 5 muodostaa oman klusterinsa - ei hyv??? asia!
plot(data1, col = h3)

h4 <- cutree(h, k = 4)
names(h4) <- rownames(data1)
h4
#Forged bills are divided into two subclusters
#V??????rennetyt setelit jakautuvat kahteen aliklusteriin
plot(data1, col = h4) #4 clusters for bill data / 4 klusteria setelidatalle

# k-MEANS CLUSTERING / k:n KESKIARVON KLUSTEROINTI

#Uses an iterative algorithm - Different initial clusters (randomly generated) may lead to different
# results (not always the global optimum, but only local optimum).
# From R help: "centers: either the number of clusters,
#   say k, or a set of initial (distinct) cluster centres.
#   If a number, a random set of (distinct) rows in x is
#   chosen as the initial centres."
# See the text on lecture notes: Chapter 8 page 24 (lower half)
#
#What can you do?
# You should run the code multiple times and check if results vary
#   The lower the Within cluster sums of squares (SS)
#     (the variation within clusters) are, the better it is,
#     i.e. between_SS / total_SS is higher
#
#Then what's the correct number on clusters?
#   One way to do it is to stop, when adding 1 more cluster
#   does not increase the explained variance in a significant way.
#
#
#
#Algoritmi, jolla k:n keskiarvon menetelm??? laskee, on iteratiivinen ja se tarvitsee jotkin alkuarvot.
# Oletuksena alkuarvot ovat R:ss??? satunnaisia havaintorivej??? aineistosta.
#   Mik???li n???m??? alkuarvot ovat huonoja, niin menetelm??? saattaa tuottaa vain lokaalin optimiarvon
#   (arvo on optimaalinen paikallisesti, mutta jossain muussa kohdassa voi olla viel??? sopivampi arvo),
#     mutta ei globaalia. Alkuarvot siis vaikuttavat tuloksiin.
#   T???m???n vuoksi (kuten luentomateriaalissa lukee), alkuarvojen asettamiseen (ne voi siis asettaa itse)
#     voisi k???ytt?????? jotain toista klusterointimenetelm??????.
#   Lis???ksi haluttiin mahdollisimman pienet vaihtelut klustereille,
#     jolloin klustereiden sis???iset vaihtelut (Within cluster sum of squares (SS) by cluster)
#     olisivat hyv??? olla mahdollisimman pieni???.
#     - T???m??? tarkoittaa sit???, ett??? se between_SS / total_SS -prosenttiosuus
#       olisi hyv??? olla mahdollisimman suuri (aja useamman kerran ja tarkastele arvoja).
#
# Klustereiden lukum??????r???n p??????tt???miseksi voi katsoa esimerkiksi sit???, milloin yhden klusterin lis??????minen
#   ei lis?????? selitetyn varianssin osuutta (between_SS / total_SS) en?????? paljoakaan

k <- kmeans(data1, 2)
names(k$cluster) <- rownames(data1)
k
#Cluster sizes 100 and 100, cluster means: Cluster 1: length = 214.82...
#  Clusters are exactly the two different types of bills
#  Sums of squares: between_SS / total_SS =  58.8 %
#Klustereiden koot 100 ja 100, klustereiden keskipisteet: 1. klusteri: pituus = 214,82...
#  Klusterit ovat tarkalleen kaksi eri setelityyppi???
#  Neli???summat: klustereiden v???linen neli???summa / kokonaisneli???summa = 58,8%

plot(data1, col = z) #Original variables / Alkuper???iset muuttujat
plot(data1, col = k$cluster) #By cluster / Klustereittain
#The plots are exactly the same (colors may be the other way around)
#Kuvat ovat k???yt???nn???ss??? samat (v???rit saattavat k??????nty??? toisin p???in)

k <- kmeans(data1, 3)
names(k$cluster) <- rownames(data1)
k
plot(data1, col = k$cluster)
#You may get two different types of result (you can run a few times to see the both):
#  between_SS / total_SS =  64.3 % or between_SS / total_SS =  70.4 %
#  The latter one has smaller within cluster variation i.e. it seems to be better
#  Forged bills in two different clusters now
#
#Saatat saada kahta erilaista tulosta (aja uudelleen jokunen kerta, niin n???hnet molemmat)
#  between_SS / total_SS =  64.3 % tai between_SS / total_SS =  70.4 %
#  J???lkimm???isell??? klustereiden sis???iset vaihtelut ovat pienempi??? eli se vaikuttaa paremmalta
#  V??????rennetyt setelit ovat nyt kahdessa eri klusterissa


k <- kmeans(data1, 4)
names(k$cluster) <- rownames(data1)
k
plot(data1, col = k$cluster)
#Again two different results may appear. Choose the one with b_SS / t_SS 75,4 %
#  Genuine bills now divided into two clusters too (+ the weird observation 70)
#Taas kahta eri tyyppist??? tulosta n???ytt?????? tulevan. Valitse esim. se, jossa b_SS / t_SS 75,4 %
#  Aidot setelit my???s nyt kahdessa eri klusterissa (+ havainto 70, joka on oma lukunsa)

#6. For the Fisher's Iris data, use hierarchical clustering and k-means clustering to find two, three and four clusters in the data.


# 6 Fisher's Iris data
#
# HIERARCHICAL CLUSTERING / HIERARKKINEN KLUSTEROINTI (tai ryv???st???minen / ryhmittely)
h <- hclust(dist(data2[2:5]), method = "average")
plot(h)

h2 <- cutree(h, k = 2)
names(h2) <- rownames(data2)
h2
#Species 1 in their own cluster, other species in the other one
#Laji 1 omassa klusterissaan, muut lajit toisessa

plot(data2[2:5], col = species) #Original variables / Alkuper???iset muuttujat
plot(data2[2:5], col = h2) #2 species in one cluster / 2 lajia samassa klusterissa

h3 <- cutree(h, k = 3)
names(h3) <- rownames(data2)
h3
table(h3, species)
#14 from species 3 is clustered with species 2 observations
#14 lajin 3 edustajaa on klusteroitunut lajin 2 havaintojen kanssa
plot(data2[2:5], col = h3)

h4 <- cutree(h, k = 4)
names(h4) <- rownames(data2)
h4
table(h4, species) #Species 1 is still its own cluster / Laji 1 muodostaa edelleen oman klusterinsa
plot(data2[2:5], col = h4)




# k-MEANS CLUSTERING / k:n KESKIARVON KLUSTEROINTI
k <- kmeans(data2[2:5], 2)
names(k$cluster) <- rownames(data2)
k
#Cluster sizes 97 and 53 (latter includes everyone of species 1 and a few from species 2)
#Klusterikoot 97 ja 53 (j???lkimm???inen sis???lt?????? kaikki lajista 1 ja muutaman lajista 2)
plot(data2[2:5], col = species)
plot(data2[2:5], col = k$cluster)

k <- kmeans(data2[2:5], 3)
names(k$cluster) <- rownames(data2)
k
#Again, you may choose the one with 88.4 % b_SS / t_SS (cluster sizes are more close to each other too)
#Voit valita sen, jossa on 88,4 % (joskus tulee 79,0) vaihteluosuus (my???s pienemm???t erot klusterien koossa)
plot(data2[2:5], col = k$cluster)

k <- kmeans(data2[2:5], 4)
names(k$cluster) <- rownames(data2) #91,6 % = :)
k
plot(data2[2:5], col = k$cluster)
# 4 clusters (one contain the species 1)
# 4 klusteria (yhdess??? koko laji 1)