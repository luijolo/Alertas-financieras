library(httr)
library(rvest)
library(jsonlite)
library(stringr)

start_time <- Sys.time()
token <- Sys.getenv("TELEGRAM_TOKEN")
chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")

send_telegram <- function(msg) {
  url <- paste0("https://api.telegram.org/bot", token, "/sendMessage")
  res <- POST(url, body = list(chat_id = chat_id, text = msg), encode = "form")
  if (status_code(res) != 200) {
    stop(paste("Telegram rechazó el mensaje con código", status_code(res)))
  }
}

parse_number_dr <- function(text) {
  if (is.na(text) || is.null(text) || nchar(text) == 0) return(NA)
  clean_text <- gsub("[^0-9.,]", "", text)
  clean_text <- gsub(",", "", clean_text)
  as.numeric(clean_text)
}

# Cabeceras completas para simular un navegador Chrome real y evitar bloqueos 403
headers_browser <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
  `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8",
  `Cache-Control` = "no-cache",
  `Pragma` = "no-cache",
  `Sec-Ch-Ua` = '"Chromium";v="122", "Not(A:Brand";v="24", "Google Chrome";v="122"',
  `Sec-Ch-Ua-Mobile` = "?0",
  `Sec-Ch-Ua-Platform` = '"Windows"',
  `Sec-Fetch-Dest` = "document",
  `Sec-Fetch-Mode` = "navigate",
  `Sec-Fetch-Site` = "none",
  `Sec-Fetch-User` = "?1",
  `Upgrade-Insecure-Requests` = "1"
)

tryCatch({
  state_file <- "estado_hashes.json"
  
  old_state <- list()
  if (file.exists(state_file) && file.info(state_file)$size > 2) {
    tryCatch({ old_state <- fromJSON(state_file) }, error = function(e) list())
  }
  new_state <- old_state

  fecha_hoy <- format(Sys.Date(), "%d/%m/%Y")

  # ==========================================
  # 1. FIDUCIARIA RESERVAS
  # ==========================================
  cat("\n==========================================\n")
  cat("1. PROCESANDO FIDUCIARIA RESERVAS\n")
  cat("==========================================\n")
  url_fid <- "https://www.fiduciariareservas.com/proyectos-oferta-publica/fideicomiso-de-oferta-publica-de-valores-multiplaza-fr-n02/"
  
  res_fid <- GET(url_fid, headers_browser)
  cat("Status Code con cabeceras de navegador:", status_code(res_fid), "\n")
  
  if (status_code(res_fid) == 200) {
    html_raw_fid <- content(res_fid, "text", encoding = "UTF-8")
    val_raw <- str_extract(html_raw_fid, "1,\\d{3}\\.\\d{4,6}")
    cat("Valor extraído Fiduciaria:", val_raw, "\n")
    
    val_fid <- parse_number_dr(val_raw)
    
    if (!is.na(val_fid)) {
      prev_fid <- old_state[["multiplaza_valor"]]
      if (!is.null(prev_fid) && val_fid != prev_fid) {
        inv_fid <- val_fid * 20
        msg_fid <- paste0(
          fecha_hoy, "\n",
          "Valor cuota FOP Multiplaza RD$", format(val_fid, nsmall = 6), "\n",
          "Valor inversión RD$", format(inv_fid, big.mark = ",", nsmall = 2)
        )
        send_telegram(msg_fid)
      }
      new_state[["multiplaza_valor"]] <- val_fid
    }
  }

  # ==========================================
  # 2. AFI UNIVERSAL (Consultando API Interna / JSON)
  # ==========================================
  cat("\n==========================================\n")
  cat("2. PROCESANDO AFI UNIVERSAL\n")
  cat("==========================================\n")
  
  # Intento directo de consultar la API de datos de AFI Universal si existe, o endpoint público
  url_afi_page <- "https://www.afiuniversal.com.do/universal-liquidez/"
  res_afi <- GET(url_afi_page, headers_browser)
  cat("Status Code AFI:", status_code(res_afi), "\n")
  
  if (status_code(res_afi) == 200) {
    html_raw_afi <- content(res_afi, "text", encoding = "UTF-8")
    
    # Buscar scripts embebidos en el HTML donde a veces se inyectan las variables JSON iniciales
    script_data <- str_extract_all(html_raw_afi, "<script.*?>.*?</script>")[[1]]
    cat("Scripts encontrados en la página:", length(script_data), "\n")
    
    # Búsqueda amplia de patrones numéricos en scripts de la página
    numeros_script <- str_extract_all(html_raw_afi, "\\b\\d{1,5}\\.\\d{4,6}\\b")[[1]]
    cat("Números decimales encontrados en el código fuente:", length(numeros_script), "\n")
    if (length(numeros_script) > 0) {
      cat("Muestra de números:", paste(head(numeros_script, 10), collapse = ", "), "\n")
    }
  }

  # ==========================================
  # 3. CEVALDOM (OTC)
  # ==========================================
  cat("\n==========================================\n")
  cat("3. PROCESANDO CEVALDOM\n")
  cat("==========================================\n")
  
  # Endpoint API directo de Cevaldom para el mercado OTC
  url_cev_api <- "https://www.cevaldom.com/wp-admin/admin-ajax.php?action=get_otc_data" 
  res_cev_api <- GET(url_cev_api, headers_browser)
  cat("Status Code API Cevaldom:", status_code(res_cev_api), "\n")
  
  if (status_code(res_cev_api) != 200) {
    # Si la API no responde directo, leemos la página base con cabeceras completas
    url_cev <- "https://www.cevaldom.com/mercado/otc/"
    res_cev <- GET(url_cev, headers_browser)
    cat("Status Code Página Cevaldom:", status_code(res_cev), "\n")
    if (status_code(res_cev) == 200) {
      html_raw_cev <- content(res_cev, "text", encoding = "UTF-8")
      cat("¿Contiene DO9035100120 con headers completos?:", grepl("DO9035100120", html_raw_cev), "\n")
    }
  }

  # Guardar estado
  write_json(new_state, state_file, auto_unbox = TRUE, pretty = TRUE)
  cat("\n--- PROCESO FINALIZADO --- Estado guardado en", state_file, "\n")

}, error = function(e) {
  send_telegram(paste0("Error en script.R: ", e$message))
  stop(e)
})
