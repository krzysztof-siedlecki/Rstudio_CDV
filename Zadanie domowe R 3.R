Imię: [Krzysztof]
Nazwisko: [Siedlecki]
Numer albumu: [29486]

# --------------------------------------------------------
# Zadanie.12 
#
# W R dostępne są zbiory WWWusage, presidents oraz mdeaths. Wczytaj je i dokonaj eksploracji danych.
# Sprawdź czy są to zbiory typu "time series". Opisz jakich okresów dotyczą dane oraz jakim okresom 
# czasu odpowiadają poszczególne obserwacje. Sporządź rółwnież wykresy liniowe obrazujące te szeregi czasowe.
#
# Pełne rozwiązanie tego zadania musi zawierać dla każdego zbioru: 
#	- Twój kod R, 
#	- Twój opis i interprację szeregów czasowych,
#   - czytelną wizualizację na wykresach. 
#
# --------------------------------------------------------
# tu wpisz swój kod: 

WWWusage
class(WWWusage) #ts oznacza Time series
is.ts(WWWusage)
summary(WWWusage)
time(WWWusage)
plot(WWWusage)

#mniej niż 100 osób spędza poniżej okokło 17 minut na stronie i poniżej 100 osób spędza od 65 do 80 minut na stroni
#od około 100 do 150 spędza od 17 do 60 minut i od 80 do 83 minut nastronie
#między około 150 a 200 osób spędza na stronie od 42 do 60 minut i około 81 do 91 minut
#
View(presidents)
class(presidents) #ts oznacza Time series
is.ts(presidents)
summary(presidents)
time(presidents)
plot(presidents)
#średnia wieku prezydentów to 56 lat, najmłodszy prezydent miał 23 lata a najstarszy 87

View(mdeaths)
class(mdeaths) #ts oznacza Time series
is.ts(mdeaths)
summary(mdeaths)
time(mdeaths)
plot(mdeaths)
#widać cyklicznie pojawiające się odczyty na temat ilości śmierci. Najwięcej było w 1976 roku


# --------------------------------------------------------
# Zadanie.13
#
# Dostępny jest zbiór danych prezentujących liczbą małżeństw (v1). Prezentuje on dane za okres od stycznia
# 2009 do grudnia 2011. Dokonaj jego dekompozycji. Wybierz właściwy typ modelu wahań w czasie. Wykreśl
# wyniki. Odpowiedz na pytania:
#
# W których miesiącach jest najwyższy dodatni poziom wahań sezonowych, 
# a w których jest najniższy? Uzasadnij odpowiedź i zinterpretuj wyniki.
#  
# O ile procent w każdym miesiącu wskaźniki sezonowości odchylają się od średniej dla danego miesiaca?
#
# --------------------------------------------------------
# tu wpisz swĂłj kod: 

v1 <-  c(10.8, 12.9, 4.7, 28.1, 6.1, 23.1, 20.2, 23.7, 30.4, 24.6, 7.7, 14.8,
         10.2, 10.9, 4.9, 27.3, 6.0, 27.6, 16.7, 28.3, 26.7, 24.8, 8.2, 12.9,
         9.8,  7.9, 8.1, 25.7, 6.3, 25.2, 18.8, 29.6, 26.2, 26.1, 8.3, 12.9)
summary(v1)
plot(v1)
View(v1)
start(v1)
end(v1)
time(v1)

wektor_v1_daty <- ts(v1,  start = 2009, frequency = 12)
wektor_v1_daty 
plot(wektor_v1_daty)
time(wektor_v1_daty)
decompose <- decompose(wektor_v1_daty)
decompose #rozdzdzielenie wektora na odchylenie od 'normy'
plot(decompose)
sezonowosc <- decompose$seasonal
sezonowosc
max <- which.max(sezonowosc)  
max #najwyższa wartość wektora występowała we wrześniu 
min <- which.min(sezonowosc)
min #najniższa wartość wektora była w marcu 2009
odchylenie <- (sezonowosc - mean(sezonowosc)) / 12 * 100
odchylenie
# --------------------------------------------------------
# Zadanie.14
#
# W pakiecie ISLR dostępny jest zbiór "Auto "z informacją o różnych modelach samochodów.
# Zapoznaj się ze znaczeniem zmiennych zbioru. Dokonaj eksploracji zbioru za pomocą
# poznanych funkcji (np. head, summary, itd.).
#
# Utwórz model regresji liniowej ze zmienną mpg jako objaśnianą a horsepower jako objaśniającą.
# Zastanów się co zawierajÄ zmienne i czy istnieje zwiÄzek przyczynowo-skutkowy. 
# Zinterpretuj zmienne i uzasadnij zwiÄzek.
# 
# Dokonaj analizy parametrĂłw modelu za pomocÄ funkcji summary(). Na podstawie je wynikĂłw odpowiedz 
# na nastÄpujace pytania:
# a) Czy istnieje zwiÄzek pomiÄdzy zmiennÄ objaĹniajÄcÄ a objaĹnianÄ?
# b) Jak moĹźna interpretowaÄ wartoĹÄ wspĂłĹczynnika regresji?
# c) JakÄ liczbÄ mil per galon paliwa model przewiduje dla 98 koni mechanicznych? 
# {wskazĂłwka: uĹźyj funkcji predict z argumentem interval="conficence"
# d) utwĂłrz wykres rozrzutu dla tych dwĂłch zmiennych; dodaj liniÄ regresji za pomocÄ funkcji "abline"
# e) uĹźyj funkcji "plot", Ĺźeby obejrzeÄ wykresy diagnostyczne
#
# SprĂłbuj stworzyÄ model dla wiÄkszej liczby zmiennych. 
# Najpierw stwĂłrz macierz korelacji dla zmiennych numerycznych uĹźywajÄch funkcji cor(). 
# (pamiÄtaj, Ĺźe w argumencie funkcji cor powinny byÄ dane bez zmiennych kategorycznych)
# f) ktĂłre zmienne numeryczne sÄ silnie skorelowane ze zmiennÄ objaĹnianÄ?
#
# StwĂłrz teraz model2 oparty na wszystkich zmiennych.
# g) KtĂłre zmienne sÄ powiÄzane ze zmiennÄ objaĹnianÄ (mpg) w statystycznie istotnym stopniu?
# h) Jak moĹźna interpretowaÄ wspĂłĹczynnik przy zmiennej "year"?
#
# PorĂłwnaj modele ze wzglÄdu na wartoĹÄ R kwadrat 
# i stwĂłrz wykresy diagnostyczne dla obu modeli.

?Auto

#
# --------------------------------------------------------
# tu wpisz swĂłj kod: 

install.packages("ISLR")
library(ISLR)
Auto
class(Auto)
data(Auto)
View(Auto)
head(Auto)
tail(Auto)
summary(Auto)

regresja_liniowa <- lm(mpg ~ horsepower, data = Auto)
regresja_liniowa

summary(regresja_liniowa)
plot(regresja_liniowa)

# a) jest związek ujemny
# b) współczynnik regresji liniowej ujemny -0.157845
# c)
predict(regresja_liniowa, newdata = data.frame(horsepower = 98), interval = "confidence")
#przewidywany mpg wynosi około 24,5, najmniej około 14 / najwięcej prawie 25
# e)
korelacja <- cor(Auto[, sapply(Auto, is.numeric)])
korelacja
# f) silne korelację negatywną z mpg ma weight i displacement, powyżej 0.8, trochę słabszą ma cylinders i horsepower. 
model2 <- lm(mpg ~ ., data = Auto)
model2
# w statystycznie istotnym stopniu powiązana jest wartość cylinders
#h) współczynnik przy year wynosi 0.6 więc można wziąć go pod uwagę aczkolwiek nie będzie najważniejszą zmienną 
summary(regresja_liniowa) #0.6059
summary(model2) #0.9816
plot(regresja_liniowa)
plot(model2)
# --------------------------------------------------------
# Zadanie.15
#
# Wygeneruj 1000-elementowy wektor z rozkĹadu Poisonna z parametrem lambda=6. Policz dla tego wektora:
# - ĹredniÄ, 
# - rozstÄp, 
# - wariancjÄ, 
# - kwartyle, 
# - rozstÄp miÄdzykwartylowy, 
# - kurtozÄ, 
# - skoĹnoĹÄ ,
# - oraz dominantÄ.
# Zwizualizuj wektor za pomocÄ histogramu oraz wykresu pudeĹkowego (uĹźyj kolorĂłw, ustaw tytuĹy osi, wykresu).
# Jak majÄ siÄ do siebie Ĺrednia, mediana i dominanta?
# Wygeneruj ponownie losowy wektor (korzystajÄc z tej samej komendy) i uruchom ponownie napisane komendy. 
# Czy sÄ wyraĹşne rĂłĹźnice?


#
# --------------------------------------------------------
# tu wpisz swĂłj kod: 

install.packages("moments")
library(moments)

set.seed(1)
rozklad_pios <- rpois(1000, lambda = 6)
rozklad_pios
srednia <- mean(rozklad_pios)
srednia
rozstep <- range(rozklad_pios)
rozstep
wariancja <- var(rozklad_pios)
wariancja
kwartyle <- quantile(rozklad_pios)
kwartyle
rozstep_międzykwartylowy <- IQR(rozklad_pios)
rozstep_międzykwartylowy
kurtoza <- kurtosis(rozklad_pios)
kurtoza
skosnosc <- skewness(rozklad_pios)
skosnosc
dominanta <- as.numeric(names(which.max(table(rozklad_pios))))
dominanta
hist(rozklad_pios,col='red', main = "Histogram", xlab = "Rozkład")
boxplot(rozklad_pios,col='pink', main = "Boxplot")
median(rozklad_pios)

#1 raz wygenerowane: średnia: 5,996 ; mediana:6 ; dominanta:5
# mediana i dominanta pozostają takie same, średnia wynosi tym razem 5.921 więc jest minimalna różnica
