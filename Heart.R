# Wykrywanie Choroby Niedokrwiennej Serca (CHD)

#install.packages("AMORE")
library(AMORE)

# import data
Heart = read.csv("~/R/R_Lab1/SAheart.csv")
Heart

# split data
Ilosc.Danych=nrow(Heart)
#set.seed(8) #Linux
set.seed(666) #Windows
idxTren<-sample(1:Ilosc.Danych,2*Ilosc.Danych/3)
idxTest<-setdiff(1:Ilosc.Danych,idxTren) 

# data to number values 
target<-function(x)
{
  n<-length(x)
  wartosci<-levels(x)
  l<-length(wartosci)
  T<-matrix(0,nrow=n,ncol=l)
  for(i in 1:l)
    T[,i]<-(x==wartosci[i])
  colnames(T)<-wartosci
  return(T)
}

wZadane<-target(Heart$chd)
wZadane

#set.seed(3) #Linux
set.seed(777) #Windows

# create network
siec<-newff(n.neurons=c(9,4,2),
            learning.rate.global=0.02,
            momentum.global=0.8,
            hidden.layer="sigmoid",
            output.layer="purelin",
            method="ADAPTgdwm",
            error.criterium="LMS")

# train netwok
wynik<-train(siec,
             Heart[idxTren,-10],
             wZadane[idxTren,],
             error.criterium="LMS",
             report=TRUE,
             show.step=10,
             n.shows=1000)

# show plot
plot(wynik$Merror,
     type="l",
     xlab="Iteracja (x100)",
     ylab="Blad", 
     col="blue",
     xlim = c(0,100)
     )

# run test data
y<-sim(wynik$net,Heart[idxTest, -10])

test.klasyf<-function(zad,wy)
{
  zadane<-max.col(zad)
  rozpoznane<-max.col(wy)
  print(table(zadane,rozpoznane))
}
wynik<-test.klasyf(wZadane[idxTest,],y)

cat("Dokladnosc klasyfikacji:",
    sum(diag(wynik))/sum(wynik)*100, "%\n")

