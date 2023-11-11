#' @title Wykonaj model liniowy używając `lm`
#' @description Funkcja tworzy model liniowy na podstawie podanych argumentów, używając funkcji `lm` z R.
#' @param ... Argumenty przekazywane do funkcji `lm`.
#' @return Model liniowy stworzony za pomocą funkcji `lm`.
#' @examples
#' # Utwórz model liniowy na podstawie zestawu danych mtcars
#' model <- zamodeluj(mpg ~ ., data = mtcars)
#' @export
zamodeluj <- function(...) lm(...)

#' @title Ocena modelu liniowego
#' @description Funkcja ocenia model liniowy, obliczając różne statystyki, takie jak R^2, skorygowane R^2, p-wartość dla statystyki F, AIC i BIC.
#' @param model Model liniowy do oceny.
#' @return Ramka danych z wynikami oceny. Kolumny to: "R^2", "R^2 skoryg.", "p [F]", "AIC", "BIC".
#' @examples
#' # Utwórz model liniowy na podstawie zestawu danych mtcars
#' model <- zamodeluj(mpg ~ ., data = mtcars)
#' # Oceń model
#' wyniki <- ocena(model)
#' @export
ocena <- function(model) {

    ynazwa = as.character(formula(model))[2]
    y = model$model[,ynazwa]

    rsummary = summary(model)

    wyniki = data.frame(check.names = FALSE,
        "R^2" = rsummary$r.squared,
        "R^2 skoryg." = rsummary$adj.r.squared,
        "p [F]" = pf(rsummary$fstatistic[1], rsummary$fstatistic[2], rsummary$fstatistic[3], lower.tail = FALSE),
        "AIC" = AIC(model),
        "BIC" = BIC(model),
        "MAE" = Metrics::mae(y, model$fitted.values),
        "RMSE" = Metrics::rmse(y, model$fitted.values),
        "MSE" = Metrics::mse(y, model$fitted.values),
        "PRESS" = sum( (  (modele[[3]]$residuals)  /  (1 - stats::lm.influence(modele[[3]])$hat)  )^2 ) # TODO poprawić
    )

    rownames(wyniki) = NULL

    return(wyniki)
}

#' @title Pobierz parametry modelu liniowego
#' @description Funkcja zwraca parametry modelu liniowego jako ramkę danych. Dla każdego parametru tworzone są dwie kolumny: jedna dla wartości parametru i druga dla wartości p.
#' @param model Model liniowy, z którego mają być pobrane parametry.
#' @return Ramka danych z parametrami modelu. Dla każdego parametru tworzone są dwie kolumny: jedna dla wartości parametru i druga dla wartości p.
#' @examples
#' # Utwórz model liniowy na podstawie zestawu danych mtcars
#' model <- zamodeluj(mpg ~ ., data = mtcars)
#' # Pobierz parametry modelu
#' parametry_modelu <- parametry(model)
#' @export
parametry <- function(model) {
    wyniki <- summary(model)$coefficients
    df <- data.frame()
    for (i in 1:nrow(wyniki)) {
        df[1, rownames(wyniki)[i]] <- wyniki[i, 1]
        df[1, paste("p", paste0('[', rownames(wyniki)[i]), ']')] <- wyniki[i, 4]
    }
    return(df)
}

#' @title Pobierz rezydua modelu liniowego
#' @description Funkcja zwraca rezydua z modelu liniowego.
#' @param model Model liniowy, z którego mają być pobrane rezydua.
#' @return Wektor rezyduów z modelu.
#' @examples
#' # Utwórz model liniowy na podstawie zestawu danych mtcars
#' model <- zamodeluj(mpg ~ ., data = mtcars)
#' # Pobierz rezydua modelu
#' rezydua_modelu <- rezydua(model)
#' @export
rezydua <- residuals

odstające <- function(dane) {

}

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

#' @title Diagnostyka inflacji wariancji
#' @description Funkcja oblicza współczynniki inflacji wariancji (VIF) dla modelu.
#' @param dane Model liniowy, dla którego mają być obliczone współczynniki VIF.
#' @return Ramka danych z wartościami VIF dla każdej zmiennej w modelu.
#' @export
inflacja <- function(dane) {

    wyniki = t(data.frame(car::vif(modele[[1]])))
    rownames(wyniki) = NULL

    return(wyniki)
}


#' @title Testy normalności
#' @description Funkcja wykonuje wybrane testy normalności na podanej próbce.
#' @param próbka Wektor danych, na których mają być wykonane testy.
#' @param testy Wektor nazw testów do wykonania. Domyślnie wykonuje wszystkie dostępne testy.
#' @return Ramka danych z wartościami statystyk testowych i wartościami p dla każdego wykonanego testu.
#' @export
normalność <- function(próbka, testy = c("Shapiro-Wilk", "Anderson-Darling", "Lilliefors", "Jarque-Bera", "D'Agostino", "Craméra-von Mises", "Kolmogorov-Smirnov")) {

    wyniki = data.frame()
    nazwy = c()

    if ("Shapiro-Wilk" %in% testy) {
        test = shapiro.test(próbka)
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

    if ("Craméra-von Mises" %in% testy) {
        test = cramer::cvm.test(próbka)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Craméra-von Mises")
    }

    if ("Kolmogorov-Smirnov" %in% testy) {
        test = ks.test(próbka, "pnorm", mean(próbka), sd(próbka))
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
autokorelacja <- function(model, testy = c("Durbin-Watson", "Breusch-Godfrey"), rząd = 1, maksymalny = 1) {

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
homoskedastyczność <- function(model, testy = c("Breusch-Pagan", "White", "Goldfeld-Quandt")) {

    wyniki = data.frame()
    nazwy = c()

    if ("Breusch-Pagan" %in% testy) {
        test = lmtest::bptest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Breusch-Pagan")
    }

    if ("White" %in% testy) {
        test = bstats::white.test(model)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "White")
    }

    if ("Goldfeld-Quandt" %in% testy) {
        test = lmtest::gqtest(model)
        wyniki = rbind(wyniki, c(test$statistic, test$p.value))
        nazwy = c(nazwy, "Goldfeld-Quandt")
    }

    colnames(wyniki) = c("statystyka", "p")
    rownames(wyniki) = nazwy

    return(wyniki)
}

#' @title Diagnostyka modelu
#' @description Funkcja tworzy wykres punktowy i krzywą wygładzaną dla dwóch zmiennych z modelu.
#' @param model Model liniowy, na którym ma być wykonana diagnostyka.
#' @param x Nazwa zmiennej, która ma być na osi x. Może to być "Empiryczne", "Dopasowane", "Reszty", "Standryzowane reszty" lub "Spierwiastkowane standryzowane reszty". Domyślnie "Empiryczne".
#' @param y Nazwa zmiennej, która ma być na osi y. Może to być "Empiryczne", "Dopasowane", "Reszty", "Standryzowane reszty" lub "Spierwiastkowane standryzowane reszty". Domyślnie "Dopasowane".
#' @return Wykres ggplot z punktami danych i krzywą wygładzaną.
#' @export
diagnoza <- function(model, x = "Empiryczne", y = "Dopasowane") {

    xnazwa = x
    ynazwa = y

    v = function(nazwa) {

        nazwa = tolower(nazwa)

        if (nazwa == "reszty" || nazwa == "rezydua")
            return(residuals(model))
        if (nazwa == "dopasowane")
            return(fitted.values(model))
        if (nazwa == "empiryczne")
            return(model$model[,as.character(formula(model))[2]])
        if (nazwa == "standryzowane reszty")
            return(rstandard(model))
        if (nazwa == "spierwiastkowane standryzowane reszty")
            return(sqrt(abs(rstandard(model))))
    }

    p = ggplot(data.frame(x = v(xnazwa), y = v(ynazwa)), aes(x = x, y = y)) +
        geom_point() + geom_smooth(formula = "y ~ x", method = "loess") +
        theme_minimal() + labs(x = xnazwa, y = ynazwa)

    return(p)
}