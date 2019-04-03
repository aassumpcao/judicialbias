
#' Log into ESAJ system
#' @param login ESAJ system login (if left `NULL`, will ask for it)
#' @param password login ESAJ system login (if left `NULL`, will ask for it)
#' @export
login_esaj <- function(login = NULL, password = NULL) {

  # Check if isn't already logged in
  if (check_login()) { return(TRUE) }

  # Prompt for information if necessary
  if (is.null(login) || is.null(password)) {
    login <- as.character(readline(prompt = "Enter your login: "))
    password <- as.character(readline(prompt = "Enter your password: "))
  }

  # Initial access
  base <- "https://esaj.tjsp.jus.br/"
  httr::GET(str_c(base, "esaj/portal.do?servico=740000"), vfpr_f)

  # Get login page file
  f_login <- str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE)) %>%
    httr::GET(vfpr_f)

  # Get parameters for POST
  lt <- f_login %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//input[@name='lt']") %>%
    rvest::html_attr("value")
  e2 <- f_login %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//input[@name='execution']") %>%
    rvest::html_attr("value")

  # Create POST quert
  query_post <- list(
    username = login,
    password = password,
    lt = lt,
    execution = e2,
    "_eventId" = "submit",
    pbEntrar = "Entrar",
    signature = "")

  # Try to login
  str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE)) %>%
    httr::POST(body = query_post, vfpr_f, encode = "form")

  # Message
  flag <- check_login()
  if (flag) { message("You're logged in") }
  else { message("Login failed") }

  return(flag)
}

# Check if user is logged into ESAJ
check_login <- function() {
  flag <- "https://esaj.tjsp.jus.br/" %>%
    str_c("sajcas/verificarLogin.js") %>%
    httr::GET(vfpr_f) %>%
    httr::content("text") %>%
    detect("true")
}

#' Vizualize web page from httr::GET
#' @param response Response from an HTTP request
#' @export
visualize_page <- function (response) {
  ct <- response[["headers"]][["content-type"]]
  if (!grepl("html", ct)) {
    stop("Not a HTML file")
  }
  if (interactive()) {
    dir <- tempfile()
    dir.create(dir)
    html_file <- file.path(dir, "index.html")
    h <- httr::content(response, "text")
    enc <- stringi::stri_enc_detect(h)[[1]]$Encoding[1]
    cat(h, file = html_file)
    rstudioapi::viewer(html_file)
  }
}
