#
#
#
#https://web.stanford.edu/~hastie/ElemStatLearn/datasets/

library(AMORE)

setwd("D:\\R") #Zmieniamy katalog roboczy na folder zawieraj�cy dane
#Wczytywanie danych ucz�cych sie�
#Nag�owki w pliku glass.data zosta�y dodane r�cznie "IR,Na,Mg,Al,Si,K,Ca,Ba,Fe,Rodzaj"
#Usuni�to pierwsz� kolumn� zawieraj�c� indeksy

Heart = read.csv("SAHeart.csv")

Ilosc.Danych=nrow(Heart)
set.seed(666)
idxTren<-sample(1:Ilosc.Danych,2*Ilosc.Danych/3)  # pod�oga z 2/3 losowych indeks�w do trenowania sieci
idxTest<-setdiff(1:Ilosc.Danych,idxTren) #pozosta�e 1/3 indeks�w

#definiujemy funkcj� zmieniaj�c� zmienn� zawieraj�c� nazw� etykiety 
#na liczb� zmiennych binarnych, Ilosc.Danych jest warto�ci etykiet
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
#zastosowanie powy�szej funkcji dla danych okre�laj�cych etykiety
wZadane<-target(Heart$chd)
wZadane

set.seed(777)

#tworzymy struktur� sieci
siec<-newff(n.neurons=c(9,4,2),
            learning.rate.global=0.02,
            momentum.global=0.8,
            hidden.layer="sigmoid",
            output.layer="purelin",
            method="ADAPTgdwm",
            error.criterium="LMS")

#trenujemy sie�
wynik<-train(siec,
             Heart[idxTren,-10],
             wZadane[idxTren,],
             error.criterium="LMS",
             report=TRUE,
             show.step=10,
             n.shows=1000)

#wy�wietlam warto�ci b��d�w
plot(wynik$Merror,type="l",xlab="Iteracja (x10)",
     ylab="B��d", col="darkred")

#stosuj� wytrenowan� sie� do danych testowych
y<-sim(wynik$net,Heart[idxTest, -10])
y

#definiuj� funkcj� oceny klasyfikacji (zamieniam liczby na etykiet�)
test.klasyf<-function(zad,wy)
{
  zadane<-max.col(zad)
  rozpoznane<-max.col(wy)
  print(table(zadane,rozpoznane))
}
wynik<-test.klasyf(wZadane[idxTest,],y)

#okre�lamy dok�adno�� klasyfikacji
cat("Dok�adno�� klasyfikacji:",
    sum(diag(wynik))/sum(wynik)*100, "%\n")
