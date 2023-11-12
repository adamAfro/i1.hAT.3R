#' @title Tworzy ramkę danych
#' @description Funkcja tworzy ramkę danych z podanych argumentów, nie sprawdzając nazw.
#' @param ... Argumenty, które mają zostać umieszczone w ramce danych.
#' @return Ramka danych zawierająca podane argumenty.
#' @examples
#' # Utwórz ramkę danych z wektorami
#' df <- zramkuj(a = 1:3, b = c("A", "B", "C"))
#' @export
chomikuj <- function(..., nazwy = NULL) data.frame(..., row.names = nazwy, check.names = FALSE)

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
nazwij = function(..., poziomo = TRUE) {
  if (poziomo)
    return(names(...))
  else
    return(rownames(...))
}

#' @title Ustawia nazwy wierszy lub kolumn
#' @description Funkcja ustawia nazwy wierszy lub kolumn ramki danych.
#' @param x Ramka danych, której nazwy mają być ustawione.
#' @param value Wektor z nowymi nazwami.
#' @param poziomo Czy mają być ustawione nazwy kolumn. Domyślnie FALSE.
#' @return Ramka danych z ustawionymi nazwami.
#' @examples
#' # Utwórz ramkę danych
#' df <- data.frame(a = 1:3, b = c("A", "B", "C"))
#' # Ustaw nazwy kolumn
#' nazwy(df, poziomo = TRUE) <- c("nowa_a", "nowa_b")
#' @export
`nazwij<-` <- function(x, value, poziomo = TRUE) {
  if (poziomo)
    colnames(x) <- value
  else
    rownames(x) <- value
  x
}


#' @title Znajduje indeksy spełniające warunek
#' @description Funkcja zwraca indeksy elementów wektora spełniających podany warunek.
#' @param ... Warunek do sprawdzenia.
#' @return Wektor z indeksami spełniającymi warunek.
#' @examples
#' # Utwórz wektor
#' v <- c(1, 2, 3, 4, 5)
#' # Znajdź indeksy elementów większych od 3
#' które(v > 3)
#' @export
które = which

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
wylistuj <- list

#' @title Podgląd danych
#' @description Funkcja tworzy histogram i boxplot dla podanej próbki.
#' @param próbka Wektor danych, dla których mają być stworzone wykresy.
#' @param słupki Liczba słupków w histogramie. Domyślnie 10.
#' @param y Etykieta dla osi y. Domyślnie "Wartość".
#' @param gęstość Czy dodać krzywą gęstości do histogramu. Domyślnie FALSE.
#' @return Wykres ggplot z histogramem i boxplotem.
#' @export
rysuj_próbkę <- function(próbka, słupki = 10, y = "Wartość", gęstość = FALSE, tytuł = "Wgląd w próbkę") {

    df <- data.frame(y = próbka, typ = c(rep("Histogram", length(próbka)), rep("Boxplot", length(próbka))))

    if (gęstość)
        p1 <- ggplot2::ggplot(df[df$typ == "Histogram",], ggplot2::aes(x = y)) + 
            ggplot2::geom_histogram(ggplot2::aes(y = ..density..), color="black", fill="white", bins = słupki, boundary = 0) +
            ggplot2::geom_density(alpha = .2, fill="black") +
            ggplot2::geom_vline(ggplot2::aes(xintercept=mean(y)), linetype="dashed") +
            ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0)) +
            ggplot2::xlab(y) + ggplot2::ylab("Prawdopodobieństwo")
    else
        p1 <- ggplot2::ggplot(df[df$typ == "Histogram",], ggplot2::aes(x = y)) + 
            ggplot2::geom_histogram(color="black", fill="white", bins = słupki, boundary = 0) +
            ggplot2::geom_vline(ggplot2::aes(xintercept=mean(y)), linetype="dashed") +
            ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0)) +
            ggplot2::xlab(y) + ggplot2::ylab("Liczebność")
    
    p2 <- ggplot2::ggplot(df[df$typ == "Boxplot",], ggplot2::aes(x = y)) + 
        ggplot2::geom_boxplot() +
        ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())+
        ggplot2::xlab(y) + ggplot2::ggtitle(tytuł)
    
    p = patchwork::wrap_plots(p2, p1) + patchwork::plot_layout(heights = c(1, 3))

    return(p)
}