#' @title Testy założenia liniowości
#' @description Funkcja wykonuje testy liniowości na podanym modelu. Dostępne testy to: "Rainbow", "RESET" i "Harvey-Collier", patrz `lmtest`.
#' @param model Model liniowy, na którym mają być wykonane testy.
#' @param testy Wektor tekstowy z nazwami testów do wykonania. Domyślnie są to wszystkie dostępne testy.
#' @return Ramka danych z wynikami testów. Kolumny to: "statystyka", "st. swobody \[1\]", "st. swobody \[2\]" nie dotyczy Harv.-Coll., "p". Wiersze to nazwy wykonanych testów.
#' @export
testuj_liniowość <- function(model, testy = c("Rainbow", "RESET", "Harvey-Collier")) {

    wyniki = data.frame()
    
    nazwy = c()

    if ("Rainbow" %in% testy) {

        test = lmtest::raintest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$parameter[1], test$parameter[2], test$p.value))
        nazwy = c(nazwy, "Rainbow")
    }

    if ("RESET" %in% testy) {

        test = lmtest::resettest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$parameter[1], test$parameter[2], test$p.value))
        nazwy = c(nazwy, "RESET")
    }

    if ("Harvey-Collier" %in% testy) {
        
        test = lmtest::harvtest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$parameter[1], NA               , test$p.value))
        nazwy = c(nazwy, "Harvey-Collier")
    }

    colnames(wyniki) = c("statystyka", "st. swobody [1]",  "st. swobody [2]", "p")
    rownames(wyniki) = nazwy
        
    return(wyniki)
}







#' @title Testy normalności
#' @description Funkcja wykonuje wybrane testy normalności na podanej próbce.
#' @param próbka Wektor danych, na których mają być wykonane testy.
#' @param testy Wektor nazw testów do wykonania. Domyślnie wykonuje wszystkie dostępne testy.
#' @return Ramka danych z wartościami statystyk testowych i wartościami p dla każdego wykonanego testu.
#' @export
testuj_normalność <- function(próbka, testy = c("Shapiro-Wilk", "Anderson-Darling", "Lilliefors", "Jarque-Bera", "D'Agostino", "Kolmogorov-Smirnov")) {

    wyniki = data.frame()
    nazwy = c()

    if ("Shapiro-Wilk" %in% testy) {
        test = stats::shapiro.test(próbka)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Shapiro-Wilk")
    }

    if ("Anderson-Darling" %in% testy) {
        test = nortest::ad.test(próbka)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Anderson-Darling")
    }

    if ("Lilliefors" %in% testy) {
        test = nortest::lillie.test(próbka)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Lilliefors")
    }

    if ("Jarque-Bera" %in% testy) {
        test = tseries::jarque.bera.test(próbka)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Jarque-Bera")
    }

    if ("D'Agostino" %in% testy) {
        test = moments::agostino.test(próbka)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "D'Agostino")
    }

    if ("Kolmogorov-Smirnov" %in% testy) {
        test = stats::ks.test(próbka, "pnorm", mean(próbka), sd(próbka))
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Kolmogorov-Smirnov")
    }

    colnames(wyniki) = c("statystyka", "p")
    rownames(wyniki) = nazwy

    return(wyniki)
}

#' @title Testy autokorelacji
#' @description Funkcja wykonuje testy autokorelacji na podanym modelu. Dostępne testy to: "Durbin-Watson" i "Breusch-Godfrey".
#' @param model Model liniowy, na którym mają być wykonane testy.
#' @param testy Wektor tekstowy z nazwami testów do wykonania. Domyślnie są to wszystkie dostępne testy.
#' @param rząd Określa rząd autokorelacji do testowania w teście Breusch-Godfrey. Domyślnie jest to 1.
#' @param maksymalny Określa maksymalny rząd autokorelacji do testowania w teście Breusch-Godfrey. Domyślnie jest to 1.
#' @return Ramka danych z wynikami testów. Kolumny to: "statystyka", "p". Wiersze to nazwy wykonanych testów.
#' @examples
#' # Utwórz model liniowy na podstawie zestawu danych mtcars
#' model <- lm(mpg ~ ., data = mtcars)
#' # Wykonaj testy autokorelacji
#' wyniki_autokorelacji <- autokorelacja(model)
#' @export
testuj_autokorelacja <- function(model, testy = c("Durbin-Watson", "Breusch-Godfrey"), rząd = 1, maksymalny = 1) {

    wyniki = data.frame()
    nazwy = c()

    if ("Durbin-Watson" %in% testy) {
        test = lmtest::dwtest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Durbin-Watson")
    }

    if ("Breusch-Godfrey" %in% testy) {

        test = lmtest::bgtest(model, order = rząd)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, paste("Breusch-Godfrey", "rz.", rząd))

        if (rząd + 1 <= maksymalny) {

            for (i in (rząd + 1):maksymalny) {
                
                test = lmtest::bgtest(model, order = i)
                wyniki = rbind(wyniki, c(test$statistic, test$p.value))
                nazwy = c(nazwy, paste("Breusch-Godfrey", "rz.", i))
            }
        }
    }

    colnames(wyniki) = c("statystyka", "p")
    rownames(wyniki) = nazwy

    return(wyniki)
}

#' @title Testy homoskedastyczności
#' @description Funkcja wykonuje testy homoskedastyczności na podanym modelu. Dostępne testy to: "Breusch-Pagan", "White" i "Goldfeld-Quandt".
#' @param model Model liniowy, na którym mają być wykonane testy.
#' @param testy Wektor tekstowy z nazwami testów do wykonania. Domyślnie są to wszystkie dostępne testy.
#' @return Ramka danych z wynikami testów. Kolumny to: "statystyka", "p". Wiersze to nazwy wykonanych testów.
#' @export
testuj_homoskedastyczność <- function(model, testy = c("Breusch-Pagan", "Goldfeld-Quandt")) {

    wyniki = data.frame()
    nazwy = c()

    if ("Breusch-Pagan" %in% testy) {
        test = lmtest::bptest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Breusch-Pagan")
    }

    # TODO White

    if ("Goldfeld-Quandt" %in% testy) {
        test = lmtest::gqtest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Goldfeld-Quandt")
    }

    colnames(wyniki) = c("statystyka", "p")
    rownames(wyniki) = nazwy

    return(wyniki)
}