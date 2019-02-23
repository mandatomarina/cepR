#' @title Busca por CEP
#' @description Busca por CEP (search for information by postal code).
#' @importFrom purrr map_chr
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom tibble tibble
#' @param cep CEP (postal code), \code{character}.
#' @param token Token de autorização. Veja <http://cepaberto.com/users/register>.
#' @export
busca_cep <- function(cep = NULL, token = NULL){

  if(is.null(cep)){
    stop("Um CEP \u00e9 preciso")
  }
  if(nchar(cep) != 8){
    stop("O cep deve ter 8 digitos.")
  }
  if(is.null(token)){
    stop("Um token \u00e9 preciso")
  }

  url <- paste0("http://www.cepaberto.com/api/v3/cep?cep=", cep)

  auth <- paste0("Token token=", token)
  r <- httr::GET(url, httr::add_headers(Authorization = auth)) %>%
    httr::content("parsed") %>% null_check()

  CEP <- tibble::tibble(
    estado = strip_names(r, "sigla"),
    cidade = strip_names(r, "nome"),
    bairro = r$bairro,
    cep = r$cep,
    logradouro = r$logradouro,
    latitude = r$latitude,
    longitude = r$longitude,
    altitude = r$altitude,
    ddd = strip_names(r, "ddd"),
    cod_IBGE = strip_names(r, "ibge")
  )

  return(CEP)
}

