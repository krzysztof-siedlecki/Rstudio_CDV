ImiÄ: Krzysztof
Nazwisko: Siedlecki
Numer albumu: 29486

# --------------------------------------------------------
#Zadanie 16. Firma sprzedaje trzy kategorie produktów A, B oraz C. Każdy z nich
#jest obłożony inną stawką podatku VAT (odpowiednio: 8%, 10% i 20%). Pobierz 
#kategorię i cenę. Użyj wyrażenia warunkowego, aby wyliczyć cenę z podatkiem. 
#Zakomunikuj wynik do użytkownika.

##> Obowiązuje stawka VAT 8%. Cena wraz z podatkiem wynosi 54.
##> Obowiązuje stawka VAT 10%. Cena wraz z podatkiem wynosi 55.
##> Obowiązuje stawka VAT 20%. Cena wraz z podatkiem wynosi 60.

category <- 'A'
price <- 50


if (category =='A'){
  cat('Obowiązuje stawka VAT 8%.','Cena wraz z podatkiem wynosi',price *1.08)  
} else if (category =='B'){
  cat('Obowiązuje stawka VAT 10%.','Cena wraz z podatkiem wynosi',price *1.10)  
} else {
  cat('Obowiązuje stawka VAT 20%.','Cena wraz z podatkiem wynosi',price *1.20)  
}     


#	Zadanie 15. Oblicz iloczyn elementów dowolnego wektora x za pomocą pętli while, repeat i for (każdej z osobna).	
### istnieje funkcja wbudowana dla tego problemu prod(1:5) 


x <- (1:5)

while (x <- 5) {
  x <- x * (x+1)
  
}

i <- 0
repeat {
  i <- i + 1
  print(i)
  if (i == 3) break
}



# Zadanie.17

#
# Użyj zbioru airquality. Odpowiedz na pytania: 	
#  a) ile jest przypadków w zbiorze
#  b) ile jest przypadków z brakami danych
#  c) ile, i jakich, jest zmiennych w zbiorze
#  d) ile, i jakich, jest zmiennych z brakami danych


View(airquality)
#a)
dim(airquality) #153 wiersze (przypadki), 6 kolumn

cat(nrow(airquality), 'przypadków w zbiorze')

#b)
is.na(airquality)
cat(sum(is.na(airquality)), 'przypadków z brakami danych')
nrow(airquality[!complete.cases(airquality),]) # tyle jest linijek z brakami danych

#c)
dim(airquality) #153 wiersze, 6 kolumn(zmiennych)
colnames(airquality) #naZwy zmiennych

#d)
colnames(airquality)[colSums(is.na(airquality)) > 0] #kolumny z brakami danych to Ozone i Solar.R



# Zadanie.09
#
#	Ciąg Fibonacciego to ciąg liczb naturalnych, taki że każdy kolejny
# wyraz stanowi sumę dwóch poprzednich. 
#	https://pl.wikipedia.org/wiki/Ci%C4%85g_Fibonacciego
#	
#	Użyj pętli for, aby stworzyć 20 pierwszych wyrazów ciągu.
#	Podaj ich sumę. {użyj pętli for}

fibonacci <- function(n) {
  a <- 0
  b <- 1
  
  cat("Fibonacci Sequence:")
  for(i in 1:n) {
    cat(a, " ")
    next_num <- a + b
    a <- b
    b <- next_num
  }
}

ilosc_wyrazow <- 20
wyrazy <- fibonacci(ilosc_wyrazow)

sum <- 0
total <- 0
for(i in wyrazy) {
  total <- total + sum
  sum <- sum + wyrazy[i]
}
print(sum)