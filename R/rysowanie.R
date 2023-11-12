#' @title Siatka wykresów
#' @description Funkcja tworzy siatkę wykresów na podstawie podanych wykresów.
#' @param ... Wykresy do umieszczenia w siatce.
#' @param kolumny Liczba wierszy w siatce. Domyślnie NULL.
#' @param rzędy Liczba kolumn w siatce. Domyślnie NULL.
#' @return Siatka wykresów.
#' @export
ułóż = function(..., kolumny = NULL, rzędy = NULL) patchwork::wrap_plots(..., ncol = kolumny, nrow = rzędy)