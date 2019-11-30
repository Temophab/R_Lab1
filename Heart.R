#Wykrywanie Choroby Niedokrwiennej Serca (CHD)
#Zadanie polega na wytrenowaniu sztucznej sieci neuronowej tak aby
#wykrywa³a wyst¹pienie CHD u pacjenta
#Na podstawie:
#sbp - skurczowe ciœnienie krwi
#tobacco - tytoñ skumulowany (kg)
#ldl - Cholesterol lipoproteinowy o niskiej gêstoœci
#adiposity - oty³oœæ
#famhist - rodzinna historia chorób serca (1 - obecna, 0 - nieobecna)
#typea - zachowania typu A
#obesity - oty³oœæ
#alcohol - aktualne zu¿ycie alkoholu
#age - wiek na pocz¹tku
#chd - choroba niedokrwienna serca
#Retrospektywna próbka danych mê¿czyzn z regionu wysokiego ryzyka choroby serca
#Prowincji Przyl¹dkowej Zachodniej w RPA.
#Istniej¹ mniej wiêcej dwie kontrole na przypadek CHD.
#Wielu mê¿czyzn z pozytywnym wynikiem CHD przesz³o
#leczenie obni¿aj¹ce ciœnienie krwi i inne programy maj¹ce na celu
#zmniejszenie czynników ryzyka po wyst¹pieniu CHD.
#W niektórych przypadkach pomiary by³y wykonane po tych zabiegach.
#Dane te pochodz¹ z wiêkszego zbioru danych, opisany w:
#Rousseauw et al, 1983, South African Medical Journal. 
#Ÿród³o: https://web.stanford.edu/~hastie/ElemStatLearn/datasets/
#Metoda oceniania: hold-out, lambda=2/3

library(AMORE)

setwd("D:\\R") #Zmieniamy katalog roboczy na folder zawieraj¹cy dane
#Wczytywanie danych ucz¹cych sieæ
#Nag³owki w pliku glass.data zosta³y dodane rêcznie "IR,Na,Mg,Al,Si,K,Ca,Ba,Fe,Rodzaj"
#Usuniêto pierwsz¹ kolumnê zawieraj¹c¹ indeksy

Heart = read.csv("SAHeart.csv")

Ilosc.Danych=nrow(Heart)
set.seed(666)
idxTren<-sample(1:Ilosc.Danych,2*Ilosc.Danych/3)  # pod³oga z 2/3 losowych indeksów do trenowania sieci
idxTest<-setdiff(1:Ilosc.Danych,idxTren) #pozosta³e 1/3 indeksów

#definiujemy funkcjê zmieniaj¹c¹ zmienn¹ zawieraj¹c¹ nazwê etykiety 
#na liczbê zmiennych binarnych, Ilosc.Danych jest wartoœci etykiet
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
#zastosowanie powy¿szej funkcji dla danych okreœlaj¹cych etykiety
wZadane<-target(Heart$chd)
wZadane

set.seed(777)

#tworzymy strukturê sieci
siec<-newff(n.neurons=c(9,4,2),
            learning.rate.global=0.02,
            momentum.global=0.8,
            hidden.layer="sigmoid",
            output.layer="purelin",
            method="ADAPTgdwm",
            error.criterium="LMS")

#trenujemy sieæ
wynik<-train(siec,
             Heart[idxTren,-10],
             wZadane[idxTren,],
             error.criterium="LMS",
             report=TRUE,
             show.step=10,
             n.shows=1000)

#wyœwietlam wartoœci b³êdów
plot(wynik$Merror,type="l",xlab="Iteracja (x10)",
     ylab="B³¹d", col="darkred")

#stosujê wytrenowan¹ sieæ do danych testowych
y<-sim(wynik$net,Heart[idxTest, -10])
y

#definiujê funkcjê oceny klasyfikacji (zamieniam liczby na etykietê)
test.klasyf<-function(zad,wy)
{
  zadane<-max.col(zad)
  rozpoznane<-max.col(wy)
  print(table(zadane,rozpoznane))
}
wynik<-test.klasyf(wZadane[idxTest,],y)

#okreœlamy dok³adnoœæ klasyfikacji
cat("Dok³adnoœæ klasyfikacji:",
    sum(diag(wynik))/sum(wynik)*100, "%\n")

