% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modelowanie.R
\name{rysuj_diagnoza}
\alias{rysuj_diagnoza}
\title{Diagnostyka modelu}
\usage{
rysuj_diagnoza(model, x = "Empiryczne", y = "Dopasowane")
}
\arguments{
\item{model}{Model liniowy, na którym ma być wykonana diagnostyka.}

\item{x}{Nazwa zmiennej, która ma być na osi x. Może to być "Empiryczne", "Dopasowane", "Reszty", "Standryzowane reszty" lub "Spierwiastkowane standryzowane reszty". Domyślnie "Empiryczne".}

\item{y}{Nazwa zmiennej, która ma być na osi y. Może to być "Empiryczne", "Dopasowane", "Reszty", "Standryzowane reszty" lub "Spierwiastkowane standryzowane reszty". Domyślnie "Dopasowane".}
}
\value{
Wykres ggplot z punktami danych i krzywą wygładzaną.
}
\description{
Funkcja tworzy wykres punktowy i krzywą wygładzaną dla dwóch zmiennych z modelu.
}
