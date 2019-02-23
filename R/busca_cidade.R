#' @title Busca Bairros por Estado
#' @description Busca bairros por estado.
#' @importFrom purrr map_chr
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom tibble tibble
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @param estado sigla do estado (acronym of the state).
#' @param cidade \code{character}. Nome da cidade, por exemplo, \code{"Salvador"}. (Name of the city.)
#' @param logradouro street address.
#' @param token Token de autorização. Veja <http://cepaberto.com/users/register>.
#' @examples
#' \dontrun{
#' ubatuba <- busca_cidade(estado = "SP", cidade = "Ubatuba", token = token)
#' }
#' @export
busca_cidade <- function(estado = c("AC", "AL", "AP", "AM", "BA", "CE",
                                    "DF", "ES", "GO", "MA", "MT", "MS",
                                    "MG", "PA", "PB", "PR", "PE", "PI",
                                    "RJ", "RN", "RS", "RO", "RR", "SC",
                                    "SP", "SE", "TO"),
                         cidade = "", logradouro = "", token = NULL){

  estado <- match.arg(estado, choices = c("AC", "AL", "AP", "AM", "BA", "CE",
                                          "DF", "ES", "GO", "MA", "MT", "MS",
                                          "MG", "PA", "PB", "PR", "PE", "PI",
                                          "RJ", "RN", "RS", "RO", "RR", "SC",
                                          "SP", "SE", "TO"))
  if(is.null(token)){
    stop("Um token \u00e9 preciso")
  }

  if(nchar(cidade) == 0){
    url <- paste0("http://www.cepaberto.com/api/v3/address?estado=", estado)
  } else{
    url <- paste0("http://www.cepaberto.com/api/v3/address?estado=",
                  estado, "&cidade=", cidade)
  }

  auth <- paste0("Token token=", token)
  r <- httr::GET(url, httr::add_headers(Authorization = auth)) %>%
    httr::content("parsed") %>% null_check()

  depth <- list_depth(r)
  if(depth == 1){
    r <- list(r)
  }

  df <- setNames(object = data.frame(do.call(rbind, lapply(r, as.character, unlist)),
                                     stringsAsFactors = FALSE),
                 nm = names(r[[1]]))
  df <- replace(df, df == "NULL", NA) %>% tibble::as.tibble()


  N <- NA_character_

  df <- df %>%
    dplyr::mutate(latitude = as.numeric(latitude),
                  longitude = as.numeric(longitude),
                  altitude = as.numeric(altitude)) %>%
    dplyr::select(estado, cidade, bairro, cep, logradouro,
                  longitude, latitude, altitude, ddd, cod_IBGE = ibge)
  return(df)
}

