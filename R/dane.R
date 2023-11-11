#' @title Tworzy ramkę danych
#' @description Funkcja tworzy ramkę danych z podanych argumentów, nie sprawdzając nazw.
#' @param ... Argumenty, które mają zostać umieszczone w ramce danych.
#' @return Ramka danych zawierająca podane argumenty.
#' @examples
#' # Utwórz ramkę danych z wektorami
#' df <- zramkuj(a = 1:3, b = c("A", "B", "C"))
#' @export
zramkuj <- function(...) data.frame(..., check.names = FALSE)

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