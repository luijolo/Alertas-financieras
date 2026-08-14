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

  # ==========================================
  # 1. FIDUCIARIA RESERVAS
  # ==========================================
  cat("\n==========================================\n")
  cat("1. PROCESANDO FIDUCIARIA RESERVAS\n")
  cat("==========================================\n")
  url_fid <- "https://www.fiduciariareservas.com/proyectos-oferta-publica/fideicomiso-de-oferta-publica-de-valores-multiplaza-fr-n02/"
  
  res_fid <- GET(url_fid, add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"))
  cat("Status Code:", status_code(res_fid), "\n")
  
  if (status_code(res_fid) == 200) {
    html_raw_fid <- content(res_fid, "text", encoding = "UTF-8")
    val_raw <- str_extract(html_raw_fid, "1,[0-9]{3}\\.[0-9]{4,6}")
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
        cat("Notificación Telegram enviada para Fiduciaria Reservas.\n")
      }
      new_state[["multiplaza_valor"]] <- val_fid
    }
  }

  # ==========================================
  # 2. AFI UNIVERSAL (Endpoints QuotaValues)
  # ==========================================
  cat("\n==========================================\n")
  cat("2. PROCESANDO AFI UNIVERSAL\n")
  cat("==========================================\n")

  fondos_afi <- list(
    list(code = "LIQUID", key = "uni_liq", name = "Cuota Universal Liquidez", mult = 57),
    list(code = "FLEX", key = "dep_flex", name = "Cuota Dep. Financiero Flexible", mult = 1),
    list(code = "DOLR", key = "plazo_dol", name = "Cuota Plazo mensual dólar", mult = 1)
  )

  for (f in fondos_afi) {
    url_afi_code <- paste0("https://www.afiuniversal.com.do/funds/QuotaValues/", f$code)
    res_afi <- GET(url_afi_code, headers_browser)
    cat("\n--- Fondo:", f$name, "(", f$code, ") ---\n")
    cat("Status Code:", status_code(res_afi), "\n")
    
    if (status_code(res_afi) == 200) {
      txt_afi <- content(res_afi, "text", encoding = "UTF-8")
      cat("Respuesta cruda (primeros 150 caracteres):", substr(txt_afi, 1, 150), "\n")
      
      val_num <- NA
      json_parsed <- tryCatch({ fromJSON(txt_afi) }, error = function(e) NULL)
      
      if (!is.null(json_parsed)) {
        if (is.data.frame(json_parsed)) {
          num_cols <- sapply(json_parsed, is.numeric)
          if (any(num_cols)) {
            col_name <- names(num_cols[num_cols])[1]
            val_num <- tail(json_parsed[[col_name]], 1)
          }
        } else if (is.numeric(json_parsed)) {
          val_num <- json_parsed[1]
        } else if (is.list(json_parsed)) {
          nums <- unlist(json_parsed)[sapply(unlist(json_parsed), function(x) !is.na(as.numeric(x)))]
          if (length(nums) > 0) val_num <- as.numeric(nums[1])
        }
      }
      
      # Si la respuesta era texto/HTML plano, extraer con Expresiones Regulares
      if (is.na(val_num)) {
        matches <- str_extract_all(txt_afi, "\\b\\d{1,5}(,\\d{3})*(\\.\\d{2,6})?\\b")[[1]]
        nums <- suppressWarnings(as.numeric(gsub(",", "", matches)))
        nums <- nums[!is.na(nums) & nums > 0]
        if (length(nums) > 0) {
          val_num <- nums[1]
        }
      }
      
      cat("Valor extraído:", val_num, "\n")
      
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
          cat("Notificación Telegram enviada para", f$name, "\n")
        }
        new_state[[f$key]] <- val_num
      }
    }
  }

  # ==========================================
  # 3. CEVALDOM (API de Precios)
  # ==========================================
  cat("\n==========================================\n")
  cat("3. PROCESANDO CEVALDOM API\n")
  cat("==========================================\n")
  
  url_cev_api <- "https://www.cevaldom.com/api/cevaldom/fetch-prices"
  
  res_cev_get <- GET(url_cev_api, headers_browser)
  res_cev_post <- POST(url_cev_api, headers_browser)
  res_cev_active <- if (status_code(res_cev_get) == 200) res_cev_get else res_cev_post
  
  cat("Status Code CEVALDOM API:", status_code(res_cev_active), "\n")
  
  if (status_code(res_cev_active) == 200) {
    txt_cev <- content(res_cev_active, "text", encoding = "UTF-8")
    cat("Respuesta CEVALDOM API (primeros 200 caracteres):\n", substr(txt_cev, 1, 200), "\n")
    
    if (grepl("DO9035100120", txt_cev)) {
      cat("¡ISIN DO9035100120 detectado en transacciones OTC de hoy!\n")
    } else {
      cat("API CEVALDOM activa. No hubo transacciones para el ISIN DO9035100120 hoy.\n")
    }
  }

  # Guardar estado actualizado en el JSON
  write_json(new_state, state_file, auto_unbox = TRUE, pretty = TRUE)
  cat("\n--- PROCESO FINALIZADO --- Estado guardado en", state_file, "\n")

}, error = function(e) {
  send_telegram(paste0("Error en script.R: ", e$message))
  stop(e)
})
