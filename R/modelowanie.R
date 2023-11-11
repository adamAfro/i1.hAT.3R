#' @title Wykonaj model liniowy używając `lm`
#' @description Funkcja tworzy model liniowy na podstawie podanych argumentów, używając funkcji `lm` z R.
#' @param ... Argumenty przekazywane do funkcji `lm`.
#' @return Model liniowy stworzony za pomocą funkcji `lm`.
#' @examples
#' # Utwórz model liniowy na podstawie zestawu danych mtcars
#' model <- zamodeluj(mpg ~ ., data = mtcars)
#' @export
zamodeluj <- function(...) lm(...)


#' @title Testy założenia liniowości
#' @description Funkcja wykonuje testy liniowości na podanym modelu. Dostępne testy to: "Rainbow", "RESET" i "Harvey-Collier", patrz `lmtest`.
#' @param model Model liniowy, na którym mają być wykonane testy.
#' @param testy Wektor tekstowy z nazwami testów do wykonania. Domyślnie są to wszystkie dostępne testy.
#' @return Ramka danych z wynikami testów. Kolumny to: "statystyka", "st. swobody \[1\]", "st. swobody \[2\]" nie dotyczy Harv.-Coll., "p". Wiersze to nazwy wykonanych testów.
#' @export
liniowość <- function(model, testy = c("Rainbow", "RESET", "Harvey-Collier")) {

    wyniki = data.frame()
    
    nazwy = c()

    if ("Rainbow" %in% testy) {

        test = raintest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$parameter[1], test$parameter[2], test$p.value))
        nazwy = c(nazwy, "Rainbow")
    }

    if ("RESET" %in% testy) {

        test = resettest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$parameter[1], test$parameter[2], test$p.value))
        nazwy = c(nazwy, "RESET")
    }

    if ("Harvey-Collier" %in% testy) {
        
        test = harvtest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$parameter[1], NA               , test$p.value))
        nazwy = c(nazwy, "Harvey-Collier")
    }

    colnames(wyniki) = c("statystyka", "st. swobody [1]",  "st. swobody [2]", "p")
    rownames(wyniki) = nazwy
        
    return(wyniki)
}