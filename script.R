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

Sys.setenv(CHROMOTE_CHROME_ARGS = "--no-sandbox --disable-dev-shm-usage --disable-gpu")

parse_number_dr <- function(text) {
  if (is.na(text) || is.null(text) || nchar(as.character(text)) == 0) return(NA)
  clean_text <- gsub("[^0-9.,]", "", as.character(text))
  clean_text <- gsub(",", "", clean_text)
  as.numeric(clean_text)
}

headers_browser <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36",
  `Accept` = "application/json, text/plain, */*",
  `Accept-Language` = "es-ES,es;q=0.9,en;q=0.8"
)

tryCatch({
  state_file <- "estado_hashes.json"
  
  old_state <- list()
  if (file.exists(state_file) && file.info(state_file)$size > 2) {
    tryCatch({ old_state <- fromJSON(state_file) }, error = function(e) list())
  }
  new_state <- old_state

  fecha_hoy <- format(Sys.Date(), "%d/%m/%Y")
  
  # Registra la fecha/hora actual para forzar cambio en Git en cada corrida
  new_state[["ultima_ejecucion"]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S AST")

  # ==========================================
  # 1. FIDUCIARIA RESERVAS (Vía Headless Chrome)
  # ==========================================
  cat("\n==========================================\n")
  cat("1. PROCESANDO FIDUCIARIA RESERVAS (Headless)\n")
  cat("==========================================\n")
  
  # Configurar Chrome para evitar que colapse por falta de permisos en GitHub Actions
  Sys.setenv(CHROMOTE_EXTRA_ARGS = "--no-sandbox --disable-dev-shm-usage")
  
  url_fid <- "https://www.fiduciariareservas.com/proyectos-oferta-publica/fideicomiso-de-oferta-publica-de-valores-multiplaza-fr-n02/"
  
  tryCatch({
    cat("Iniciando navegador Chrome headless...\n")
    # Inicializar la sesión del navegador
    b <- chromote::ChromoteSession$new()
    
    # Navegar a la página
    cat("Navegando a Fiduciaria Reservas...\n")
    b$Page$navigate(url_fid)
    
    # Esperamos 8 segundos. Esto permite que los scripts de protección (Cloudflare/Akamai)
    # terminen de validar el navegador y el DOM cargue completamente los precios.
    cat("Esperando 8s a que la página renderice y pase los firewalls...\n")
    Sys.sleep(8)
    
    # Extraer todo el código HTML de la página ya procesada
    doc <- b$Runtime$evaluate("document.documentElement.outerHTML")
    html_raw_fid <- doc$result$value
    
    # Cerrar el navegador para liberar memoria
    b$close()
    
    # Usamos la misma expresión regular de antes, que ignora el ruido y atrapa el número
    patron <- "(?si)Valor patrimonial de los valores.*?([0-9]{1,3}(?:,[0-9]{3})*\\.[0-9]{2,6})"
    coincidencia <- str_match(html_raw_fid, patron)
    
    if (!is.na(coincidencia[1, 2])) {
      val_raw <- coincidencia[1, 2]
      val_fid <- parse_number_dr(val_raw)
      
      cat("Valor cuota Fiduciaria detectado:", val_fid, "\n")
      
      if (!is.na(val_fid)) {
        prev_fid <- old_state[["multiplaza_valor"]]
        if (is.null(prev_fid) || val_fid != prev_fid) {
          inv_fid <- val_fid * 20
          msg_fid <- paste0(
            fecha_hoy, "\n",
            "Valor cuota FOP Multiplaza RD$", format(val_fid, nsmall = 6), "\n",
            "Valor inversión RD$", format(inv_fid, big.mark = ",", nsmall = 2)
          )
          send_telegram(msg_fid)
          cat("Notificación enviada a Telegram para Fiduciaria Reservas.\n")
        } else {
          cat("Sin cambios en Fiduciaria Reservas.\n")
        }
        new_state[["multiplaza_valor"]] <- val_fid 
      }
    } else {
      cat("Advertencia: La página cargó con éxito, pero no se encontró la frase 'Valor patrimonial de los valores'.\n")
    }
    
  }, error = function(e) {
    cat("Error al ejecutar Chrome Headless:", e$message, "\n")
  })

  # ==========================================
  # 2. AFI UNIVERSAL
  # ==========================================
  cat("\n==========================================\n")
  cat("2. PROCESANDO AFI UNIVERSAL\n")
  cat("==========================================\n")

  # Capturar las variables de entorno de GitHub, con validación de seguridad (fallback a valores por defecto)
  m_liq <- as.numeric(Sys.getenv("MULT_LIQ"))
  if (is.na(m_liq)) m_liq <- 58.291955
  
  m_flex <- as.numeric(Sys.getenv("MULT_FLEX"))
  if (is.na(m_flex)) m_flex <- 1
  
  m_dolar <- as.numeric(Sys.getenv("MULT_DOLAR"))
  if (is.na(m_dolar)) m_dolar <- 1

  fondos_afi <- list(
    list(code = "LIQUID", key = "uni_liq", name = "Cuota Universal Liquidez", mult = m_liq),
    list(code = "FLEX", key = "dep_flex", name = "Cuota Dep. Financiero Flexible", mult = m_flex),
    list(code = "DOLR", key = "plazo_dol", name = "Cuota Plazo mensual dólar", mult = m_dolar)
  )

  for (f in fondos_afi) {
    url_afi_code <- paste0("https://www.afiuniversal.com.do/funds/QuotaValues/", f$code)
    res_afi <- tryCatch(GET(url_afi_code, headers_browser), error = function(e) NULL)
    
    if (!is.null(res_afi) && status_code(res_afi) == 200) {
      txt_afi <- content(res_afi, "text", encoding = "UTF-8")
      json_parsed <- tryCatch({ fromJSON(txt_afi) }, error = function(e) NULL)
      
      val_num <- NA
      
      if (!is.null(json_parsed)) {
        col_names <- tolower(names(json_parsed))
        pos_valor <- match(TRUE, col_names %in% c("valor", "valorcuota", "value"))
        
        if (!is.na(pos_valor)) {
          campo_exacto <- names(json_parsed)[pos_valor]
          vector_valores <- json_parsed[[campo_exacto]]
          val_raw <- tail(vector_valores, 1)
          val_num <- parse_number_dr(val_raw)
        } else if (is.numeric(json_parsed)) {
          val_num <- json_parsed[1]
        }
      }
      
      if (!is.na(val_num)) {
        prev_val <- old_state[[f$key]]
        
        if (is.null(prev_val) || val_num != prev_val) {
          val_inv <- val_num * f$mult
          simbolo <- if (f$code == "DOLR") "US$" else "RD$"
          
          msg_afi <- paste0(
            fecha_hoy, "\n",
            f$name, " ", simbolo, format(val_num, nsmall = 6), "\n",
            "Valor inversion ", simbolo, format(val_inv, big.mark = ",", nsmall = 2)
          )
          send_telegram(msg_afi)
          cat("Notificación enviada para", f$name, ":", val_num, " (Multiplicador usado:", f$mult, ")\n")
        } else {
          cat("Sin cambios para", f$name, "\n")
        }
        new_state[[f$key]] <- val_num
      }
    }
  }

  # ==========================================
  # 3. CEVALDOM (API de Precios OTC)
  # ==========================================
  cat("\n==========================================\n")
  cat("3. PROCESANDO CEVALDOM API\n")
  cat("==========================================\n")
  
  url_cev_api <- "https://www.cevaldom.com/api/cevaldom/fetch-prices"
  
  res_cev_get <- tryCatch(GET(url_cev_api, headers_browser), error = function(e) NULL)
  res_cev_post <- tryCatch(POST(url_cev_api, headers_browser), error = function(e) NULL)
  
  res_cev_active <- if (!is.null(res_cev_get) && status_code(res_cev_get) == 200) res_cev_get else res_cev_post
  
  if (!is.null(res_cev_active) && status_code(res_cev_active) == 200) {
    txt_cev <- content(res_cev_active, "text", encoding = "UTF-8")
    
    isin_objetivo <- "DO9035100120"
    trades_vistos <- old_state[["otc_trades_vistos"]]
    if (is.null(trades_vistos)) trades_vistos <- c()
    
    if (grepl(isin_objetivo, txt_cev)) {
      cat("¡ISIN", isin_objetivo, "detectado en la API de CEVALDOM!\n")
      
      json_cev <- tryCatch({ fromJSON(txt_cev) }, error = function(e) NULL)
      
      if (!is.null(json_cev)) {
        df_cev <- as.data.frame(json_cev)
        
        filas_coincidentes <- which(apply(df_cev, 1, function(row) any(grepl(isin_objetivo, row, ignore.case = TRUE))))
        
        if (length(filas_coincidentes) > 0) {
          for (idx in filas_coincidentes) {
            fila <- df_cev[idx, ]
            row_str <- paste(unname(unlist(fila)), collapse = "_")
            trade_id <- paste0(isin_objetivo, "_", gsub("[^a-zA-Z0-9_]", "", row_str))
            
            if (!(trade_id %in% trades_vistos)) {
              precio_limpio <- if (!is.null(fila$precioLimpio)) fila$precioLimpio else if (!is.null(fila$precio)) fila$precio else "Consultar"
              hora_pacto <- if (!is.null(fila$horaPacto)) fila$horaPacto else if (!is.null(fila$hora)) fila$hora else fecha_hoy
              
              msg_otc <- paste0(
                "FOP Multiplaza OTC (CEVALDOM)\n",
                "ISIN: ", isin_objetivo, "\n",
                "Hora pacto: ", hora_pacto, "\n",
                "Precio limpio: ", precio_limpio
              )
              send_telegram(msg_otc)
              cat("Notificación enviada a Telegram para CEVALDOM.\n")
              trades_vistos <- c(trades_vistos, trade_id)
            } else {
              cat("La transacción OTC ya fue notificada previamente.\n")
            }
          }
        }
      } else {
        trade_id <- paste0(isin_objetivo, "_", fecha_hoy)
        if (!(trade_id %in% trades_vistos)) {
          msg_otc <- paste0(
            "FOP Multiplaza OTC (CEVALDOM)\n",
            "Actividad detectada para el ISIN ", isin_objetivo, " el ", fecha_hoy
          )
          send_telegram(msg_otc)
          cat("Notificación enviada a Telegram para CEVALDOM (Modo Respaldo).\n")
          trades_vistos <- c(trades_vistos, trade_id)
        }
      }
      
      new_state[["otc_trades_vistos"]] <- tail(trades_vistos, 50)
    } else {
      cat("No se encontraron transacciones hoy para el ISIN", isin_objetivo, "\n")
    }
  }

  # Guardar estado persistente en el archivo JSON
  write_json(new_state, state_file, auto_unbox = TRUE, pretty = TRUE)
  cat("\n--- PROCESO FINALIZADO --- Estado guardado en", state_file, "\n")

}, error = function(e) {
  send_telegram(paste0("Error en script.R: ", e$message))
  stop(e)
})
