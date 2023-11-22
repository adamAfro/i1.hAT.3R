#' @title Wykonaj model liniowy używając `lm`
#' @description Funkcja tworzy model liniowy na podstawie podanych argumentów, używając funkcji `lm` z R.
#' @param ... Argumenty przekazywane do funkcji `lm`.
#' @return Model liniowy stworzony za pomocą funkcji `lm`.
#' @examples
#' # Utwórz model liniowy na podstawie zestawu danych mtcars
#' model <- zamodeluj(mpg ~ ., data = mtcars)
#' @export
modeluj <- function(...) lm(...)

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
oceń <- function(model) {

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
        "PRESS" = sum( (  (model$residuals)  /  (1 - stats::lm.influence(model)$hat)  )^2 ) # TODO poprawić
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
parametryzuj <- function(model) {
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
resztkuj <- residuals



#' @title Diagnostyka modelu
#' @description Funkcja tworzy wykres punktowy i krzywą wygładzaną dla dwóch zmiennych z modelu.
#' @param model Model liniowy, na którym ma być wykonana diagnostyka.
#' @param x Nazwa zmiennej, która ma być na osi x. Może to być "Empiryczne", "Dopasowane", "Reszty", "Standryzowane reszty" lub "Spierwiastkowane standryzowane reszty". Domyślnie "Empiryczne".
#' @param y Nazwa zmiennej, która ma być na osi y. Może to być "Empiryczne", "Dopasowane", "Reszty", "Standryzowane reszty" lub "Spierwiastkowane standryzowane reszty". Domyślnie "Dopasowane".
#' @return Wykres ggplot z punktami danych i krzywą wygładzaną.
#' @export
rysuj_diagnoza <- function(model, x = "Empiryczne", y = "Dopasowane") {

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

    p = ggplot2::ggplot(data.frame(x = v(xnazwa), y = v(ynazwa)), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point() + ggplot2::geom_smooth(formula = "y ~ x", method = "loess") +
        ggplot2::theme_minimal() + ggplot2::labs(x = xnazwa, y = ynazwa)

    return(p)
}

#' @title Statystyki próbki
#' @description Funkcja oblicza podstawowe statystyki dla podanej próbki.
#' @param próbka Wektor danych, dla których mają być obliczone statystyki.
#' @return Ramka danych z obliczonymi statystykami.
#' @export
oblicz_statystyki <- function(próbka) {

    kwartyle = quantile(próbka)

    return(data.frame(

        minimum = min(próbka),
        Q1 = kwartyle[[2]],
        mediana = median(próbka),
        średnia = mean(próbka),
        Q3 = kwartyle[[4]],
        maksimum = max(próbka),
        rozstęp = max(próbka) - min(próbka),

        odchylenie = sd(próbka),
        wariancja = var(próbka),
        kurtoza = moments::kurtosis(próbka),
        skośność = moments::skewness(próbka),
        zmienność = sd(próbka) / mean(próbka)
    ))
}

#' @title Diagnostyka inflacji wariancji
#' @description Funkcja oblicza współczynniki inflacji wariancji (VIF) dla modelu.
#' @param model Model liniowy, dla którego mają być obliczone współczynniki VIF.
#' @return Ramka danych z wartościami VIF dla każdej zmiennej w modelu.
#' @export
oblicz_inflacja <- function(model) {

    wyniki = t(data.frame(car::vif(model)))
    rownames(wyniki) = NULL

    return(wyniki)
}