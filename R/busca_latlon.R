#' @title Busca Bairros por Latitude e Longitude
#' @description Busca bairros por latitude e longitude.
#' @importFrom purrr map_chr
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @param lat \code{numerico}. Latitude
#' @param long \code{numerico}. Longitude.
#' @param token Token de autorização. Veja <http://cepaberto.com/users/register>
#' @examples
#' \dontrun{
#' busca_latlon(lat = -20.55, long = -43.63, token = XXXXXX)
#' }
#' @export
busca_latlon <- function(lat = NULL, long = NULL, token = NULL){

  if(is.null(lat)){
    stop("latitude \u00e9 preciso")
  }
  if(is.null(long)){
    stop("longitude \u00e9 preciso")
  }

  if(is.null(token)){
    stop("Um token \u00e9 preciso")
  }

  url <- paste0("http://www.cepaberto.com/api/v3/nearest?lat=",
                lat, "&lng=", long)

  auth <- paste0("Token token=", token)
  r <- httr::GET(url, httr::add_headers(Authorization = auth)) %>%
    httr::content("parsed") %>% null_check()


  result <- tibble::tibble(
    cidade = strip_names(r, "nome"),
    latitude = r$latitude,
    longitude = r$longitude,
    altitude = r$altitude,
    cep = r$cep,
    estado = strip_names(r, "sigla"),
    logradouro = r$logradouro,
    bairro = r$bairro,
    ddd = strip_names(r, "ddd"),
    cod_IBGE = strip_names(r, "ibge")
  )

  return(result)
}
