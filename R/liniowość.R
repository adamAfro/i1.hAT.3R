#' Sprawdza założenie liniowości modelu i zwraca ramkę z wynikami
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