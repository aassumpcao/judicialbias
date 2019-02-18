cpopg_doc_parse_ <- function(file_name) {
  file_name %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//div[contains(@class, 'fundo') and not(@id)]") %>%
    purrr::map(xml2::xml_find_all, "div") %>%
    purrr::map(xml2::xml_text) %>%
    purrr::map(stringr::str_squish) %>%
    purrr::map_dfr(tibble::as_tibble, .id = "id") %>%
    tidyr::separate(
      col = value,
      into = c("key", "value"),
      sep = ": ?",
      fill = "left",
      extra = "merge"
    ) %>%
    tidyr::replace_na(list(key = "n_processo"))
}

#' Parseia uma página baixada
#'
#' Retorna uma tibble com os resultados. Atualmente não está pegando os
#' incidentes, por simplicidade. Mas depois vamos adicionar.
#'
#' @param file_name vetor com os caminhos dos processos a serem parseados.
#'
#' @return tibble com os processos parseados. Cada linha é um arquivo e a coluna
#'   output é uma list-column que contém tibbles com os resultados de cada
#'   arquivo
#'
#' @examples
#'
#' \dontrun{
#' arqs <- fs::dir_ls("data-raw/cpopg/96926147868")
#' resultados <- cpopg_doc_parse(arqs)
#' }
#'
#' @export
cpopg_doc_parse <- function(file_name) {
  abjutils::pvec(purrr::set_names(file_name), cpopg_doc_parse_)
}
