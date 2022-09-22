#wczytanie potrzebnych biblotek

library(readr)
library(spatstat)
library(animation)
library(plotrix)

#wczytanie danych
daily_ice_edge <- read_csv("C:/Users/KN/Downloads/daily_ice_edge.csv")
#test, test1, test2 - macierze do wyliczenia wspó³rzêdnych x, y na których podstawie liczymy kontur zasiêgu
test <- data.frame(matrix(0, 361, 3))
#model = macierz do przechowania wyników dla modelu matematycznego
model <- data.frame(matrix(0,9288,361))
testmodel <- data.frame(matrix(0, 361, 3))

#dodanie kolumny, pokazuj¹cej iloœæ dni, który up³yn¹³ od pocz¹tku okresu
daily_ice_edge[1,363]<- 0
#pomiar co 2 dni w latach 1978-1987
for (i in 1:1589){daily_ice_edge[i+1,363] <- 2*i}
#pomiar codzienny w póŸniejszym okresie
for (i in 1590:9287){daily_ice_edge[i+1,363] <- i-1589+3178}
#uwzglêdnienie przerwy w pomiarach na prze³omie 1987 i 1988 roku
for (i in 1736:9288){daily_ice_edge[i,363] <- daily_ice_edge[i,363]+42}

#allmodel - amplituda fali, zale¿na liniowo od minimalnego zasiegu lodu
allmodel <- matrix(0, ncol = 2, nrow = 361)

for (i in 1:361)
{
  allmodel[i,1] <- (max(daily_ice_edge[,i+1])-min(daily_ice_edge[,i+1]))/2
  allmodel[i,2] <- min(daily_ice_edge[,i+1])
}

allmodellm <- lm(allmodel[,1]~allmodel[,2])

#testing - sta³a dodana do fali, zale¿na liniowo od minimalnego zasiegu lodu
testing <- data.frame(matrix(0, ncol = 2, nrow = 361))

for (i in 1:361){
  testing[i,1] <- mean(as.matrix(daily_ice_edge[1736:9041,i+1]))
  testing[i,2] <- min(daily_ice_edge[,i+1])
}

testinglm <- lm(testing[,1]~testing[,2])

#faza - faza fali, sta³a dla wszystkich k¹tów
faza <- 0
prog_bar<-txtProgressBar(min=0,max=362,style=3)
for (i in 2:362){
  mini <- 1
  maxi <- 1
  for (j in 2:183){
    if (daily_ice_edge[j,i] < daily_ice_edge[mini,i]) {mini <- j}
    if (daily_ice_edge[j,i] > daily_ice_edge[maxi,i]) {maxi <- j}
  }
  faza <- faza + maxi/722 + mini/722
  setTxtProgressBar(prog_bar, i)
}
faza <- (2*faza)

#model2 - model dla wszystkich danych
model2 <- matrix(0, ncol = 362, nrow = 9289)

for(i in 2:9289)
{
  model2[i,1] <- as.matrix(daily_ice_edge[i-1,363])
}

for(i in 2:362)
{
  model2[1,i] <- min(daily_ice_edge[,i])
}

for(i in 2:362){
  for(j in 2:9289){
    model2[j,i] <- (allmodellm$coefficients[2]*(model2[1,i])+allmodellm$coefficients[1])*sin(2*pi/365.25*model2[j,1]+(faza*2*pi/365.25))+testinglm$coefficients[1]+testinglm$coefficients[2]*model2[1,i]
  }
}



#wspó³rzêdne do konturu dla minimalnego zasiêgu lodu
for (i in 1:361){
  test[i,1] = 90+min(daily_ice_edge[ ,i+1])
  testmodel[i,1] <- 90+min(daily_ice_edge[ ,i+1])
  test[i,2] = test[i,1]*cos(pi*(361-i+90)/180)
  test[i,3] = test[i,1]*sin(pi*(361-i+90)/180)
}

psptest <- psp(test[1:360,2],test[1:360,3],test[c(2:361),2],test[c(2:361),3],window = disc(40))
#model matematyczny - wykorzystanie funkcji sinusoidalnej (przyjêty okres równy rokowi astronomicznemu)

for (i in 2:362){
  fun <- lm(as.matrix(daily_ice_edge[,i])~cos(daily_ice_edge$...363/365.25*2*pi)+sin(daily_ice_edge$...363/365.25*2*pi))
  testmodel[i-1,2] <- fun$coefficients[2]
  testmodel[i-1,3] <- fun$coefficients[3]
  model[,i-1] <- fun$coefficients[1]+fun$coefficients[2]*cos(daily_ice_edge$...363/365.25*2*pi)+fun$coefficients[3]*sin(daily_ice_edge$...363/365.25*2*pi)
}

prog_bar<-txtProgressBar(min=0,max=500,style=3)
saveGIF({
for (j in 1:9288){
  #wyliczanie i wyœwietlanie wyników co 18 dni rzeczywistych
  if (daily_ice_edge[j,363] %%  18 == 0){

    test2 <- data.frame(matrix(0, 361, 3))
    test3 <- data.frame(matrix(0, 361, 3))
    test4 <- data.frame(matrix(0, 361, 3))

    for (i in 1:361)
      {
    #wspó³rzêdne dla zmian rzeczywistych
      test2[i,1] = 90+daily_ice_edge[j,i+1]
      test2[i,2] = test2[i,1]*cos(pi*(361-i+90)/180)
      test2[i,3] = test2[i,1]*sin(pi*(361-i+90)/180)
    #wspó³rzêdne dla modelu matematycznego
      test3[i,1] = 90+model[j,i]
      test3[i,2] = test3[i,1]*cos(pi*(361-i+90)/180)
      test3[i,3] = test3[i,1]*sin(pi*(361-i+90)/180)
      
      test4[i,1] = 90+model2[j+1,i+1]
      test4[i,2] = test4[i,1]*cos(pi*(361-i+90)/180)
      test4[i,3] = test4[i,1]*sin(pi*(361-i+90)/180)
    }
    #Tworzenie zmiennych typu line segment pattern na podstawie wyliczonych wspó³rzêdnych
     psptest2 <- psp(test2[1:360,2],test2[1:360,3],test2[c(2:361),2],test2[c(2:361),3],window = disc(40))
     psptest3 <- psp(test3[1:360,2],test3[1:360,3],test3[c(2:361),2],test3[c(2:361),3],window = disc(40))
     psptest4 <- psp(test4[1:360,2],test4[1:360,3],test4[c(2:361),2],test4[c(2:361),3],window = disc(45))
    #Wykres
     plot(psptest, col = 4, border = 0, main = 'Zasiêg lodu morskiego wokó³ Antarktydy w latach 1978-2008')
     plot(psptest2,col = 1, border = 0, add = TRUE)
     plot(psptest3,col = 2, border = 0, add = TRUE)
     plot(psptest4,col = 3, border = 0, add = TRUE)
     draw.circle(0,0,10, border = 'grey')
     draw.circle(0,0,20, border = 'grey')
     draw.circle(0,0,30, border = 'grey')

     for (i in seq(0,330,by=30)){
      
      draw.radial.line(deg = i%%180, start = c(0,0),end = c(40*cos(i/180*pi),40*sin(i/180*pi)), col = 'grey')
     }
     text(-39,39.5,daily_ice_edge[j,1])
  }
  setTxtProgressBar(prog_bar, j)
  }
},interval = 0.1,movie.name = "Antarktyda.gif")
