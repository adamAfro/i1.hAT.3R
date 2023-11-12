#' @title Tworzy ramkę danych
#' @description Funkcja tworzy ramkę danych z podanych argumentów, nie sprawdzając nazw.
#' @param ... Argumenty, które mają zostać umieszczone w ramce danych.
#' @return Ramka danych zawierająca podane argumenty.
#' @examples
#' # Utwórz ramkę danych z wektorami
#' df <- zramkuj(a = 1:3, b = c("A", "B", "C"))
#' @export
ramka <- function(...) data.frame(..., check.names = FALSE)

#' @title Dodaje ramki danych
#' @description Funkcja dodaje ramki danych w pionie lub w poziomie.
#' @param ... Ramki danych do dodania.
#' @param poziomo Czy ramki danych mają być dodane w poziomie. Domyślnie FALSE.
#' @return Ramka danych zawierająca dodane ramki danych.
#' @examples
#' # Utwórz dwie ramki danych
#' df1 <- data.frame(a = 1:3, b = c("A", "B", "C"))
#' df2 <- data.frame(a = 4:6, b = c("D", "E", "F"))
#' # Dodaj ramki danych w pionie
#' df <- dodaj(df1, df2)
#' @export
połącz <- function(..., poziomo = FALSE) {
  if (poziomo)
    return(cbind(...))
  else
    return(rbind(...))
}

#' @title Pobiera nazwy wierszy lub kolumn
#' @description Funkcja zwraca nazwy wierszy lub kolumn ramki danych.
#' @param poziomo Czy mają być zwrócone nazwy kolumn. Domyślnie FALSE.
#' @return Wektor z nazwami wierszy lub kolumn.
#' @examples
#' # Utwórz ramkę danych
#' df <- data.frame(a = 1:3, b = c("A", "B", "C"))
#' # Pobierz nazwy kolumn
#' nazwy(df, poziomo = TRUE)
#' @export
nazwy = function(..., poziomo = TRUE) {
  if (poziomo)
    return(names(...))
  else
    return(rownames(...))
}

#' @title Wczytuje dane z pliku CSV
#' @description Funkcja wczytuje dane z pliku CSV i zwraca ramkę danych.
#' @param file Nazwa pliku CSV do wczytania.
#' @return Ramka danych z wczytanymi danymi.
#' @examples
#' # Wczytaj dane z pliku "dane.csv"
#' dane <- wczytaj("dane.csv")
#' @export
wczytaj <- read.csv

#' @title Tworzy wektor
#' @description Funkcja tworzy wektor z podanych argumentów.
#' @param ... Argumenty, które mają zostać umieszczone w wektorze.
#' @return Wektor zawierający podane argumenty.
#' @examples
#' # Utwórz wektor z liczbami od 1 do 3
#' wektor <- wektoryzuj(1, 2, 3)
#' @export
wektoryzuj <- c

#' @title Tworzy wektor
#' @description Funkcja tworzy wektor z podanych argumentów.
#' @param ... Argumenty, które mają zostać umieszczone w wektorze.
#' @return Wektor zawierający podane argumenty.
#' @examples
#' # Utwórz wektor z liczbami od 1 do 3
#' wektor <- v(1, 2, 3)
#' @export
v <- c

#' @title Tworzy listę
#' @description Funkcja tworzy listę z podanych argumentów.
#' @param ... Argumenty, które mają zostać umieszczone w liście.
#' @return Lista zawierająca podane argumenty.
#' @examples
#' # Utwórz listę z liczbami od 1 do 3
#' lista <- zlistuj(1, 2, 3)
#' @export
zlistuj <- list