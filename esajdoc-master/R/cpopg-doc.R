cpopg_foros <- function() {
  "https://esaj.tjsp.jus.br/cpopg/open.do" %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE)) %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//select[@id='id_Foro']//option") %>% {
      tibble::tibble(
        id_foro = xml2::xml_attr(., "value"),
        nm_foro = xml2::xml_text(.)
      )
    } %>%
    dplyr::filter(id_foro != "-1")
}

cpopg_url <- function() {
  "https://esaj.tjsp.jus.br/cpopg/search.do"
}
cpopg_url_pag <- function() {
  "https://esaj.tjsp.jus.br/cpopg/trocarPagina.do"
}

cpopg_query <- function(doc, pag = 1, foros = "-1") {
  list(
    "paginaConsulta" = pag,
    "conversationId" = "",
    "dadosConsulta.localPesquisa.cdLocal" = foros,
    "cbPesquisa" = "DOCPARTE",
    "dadosConsulta.tipoNuProcesso" = "UNIFICADO",
    "dadosConsulta.valorConsulta" = doc,
    "uuidCaptcha" = ""
  )
}

cpopg_npags <- function(x) {
  x %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//span[@class='resultadoPaginacao']") %>%
    xml2::xml_text() %>%
    stringr::str_extract("(?<=de )[0-9]+") %>%
    as.integer() %>%
    magrittr::divide_by(25) %>%
    ceiling()
}

cpopg_doc_ <- function(pag, doc, foros, path) {
  file_name <- sprintf("%s/%s/pag_%02d.html", path, doc, pag)
  if (!file.exists(file_name)) {
    httr::GET(
      cpopg_url_pag(),
      query = cpopg_query(doc = doc, pag = pag, foros = foros),
      httr::config(ssl_verifypeer = FALSE),
      httr::write_disk(file_name, overwrite = TRUE)
    )
    if (file.size(file_name) < 10) {
      file.remove(file_name)
      stop("File too small.")
    }
  }
  file_name
}

#' Baixa os processos a partir de um CPF ou CNPJ
#'
#' Atualmente não faz o tratamento de foros. Recomenda-se manter a opção "-1"
#' para baixar de todos os foros. Se o documento estiver relacionado a mais
#' de 1000 processos, o e-saj mostrará apenas os primeiros 1000.
#'
#' @param doc número do documento (CPF ou CNPJ)
#' @param foros código dos foros a serem pesquisados. O padrão é "-1": todos
#' @param path caminho onde os arquivos HTML serão salvos. Uma pasta com o
#'   número do documento será gerada automaticamente.
#'
#' @return `tibble` com o resultado dos downloads. Se o número de processos
#'   relacionado ao documento for zero, não retorna nada.
#'
#' @examples
#'
#' \dontrun{
#' cpopg_doc("96926147868")
#' # resultado: arquivo data-raw/cpopg/96926147868/pag_01.html
#' }
#'
#' @export
cpopg_doc <- function(doc, foros = "-1", path = "data-raw/cpopg") {
  doc <- abjutils::clean_cnj(doc)
  fs::dir_create(paste0(path, "/", doc))
  r0 <- cpopg_doc_(1L, doc, foros, path)
  npags <- cpopg_npags(r0)
  if (!is.na(npags)) {
    rate <- purrr::rate_delay(1, 10)
    insist_fun <- purrr::insistently(cpopg_doc_, rate, quiet = FALSE)
    abjutils::pvec(
      .x = seq_len(npags),
      .f = insist_fun,
      doc = doc,
      foros = foros,
      path = path
    )
  }
}
