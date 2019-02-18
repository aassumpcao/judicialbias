# use case
devtools::load_all()

cpopg_cnpj("02.558.157/0001-62")

res <- "data-raw/cpopg/02558157000162" %>%
  fs::dir_ls() %>%
  cpopg_cnpj_parse()

res %>%
  tidyr::unnest(output) %>%
  dplyr::filter(!key %in% c("n_processo", "Recebido em")) %>%
  dplyr::count(value, sort = TRUE)


# analise aasumpcao

cpfs <- "~/Documents/judicialbias/candidateSP.feather" %>%
  feather::read_feather() %>%
  dplyr::distinct(candidate.ssn) %>%
  dplyr::pull(candidate.ssn)

pb <- progress::progress_bar$new(total = length(cpfs))
cpfs %>%
  purrr::walk(~{
    pb$tick()
    cpopg_cnpj(.x)
  })

