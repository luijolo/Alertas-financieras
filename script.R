library(httr)
library(readxl)
library(digest)
library(jsonlite)
send_telegram("Prueba exitosa: El bot de Telegram está conectado a GitHub")
start_time <- Sys.time()

token <- Sys.getenv("TELEGRAM_TOKEN")
chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")

send_telegram <- function(msg) {
  url <- paste0("https://api.telegram.org/bot", token, "/sendMessage")
  POST(url, body = list(chat_id = chat_id, text = msg), encode = "form")
}

tryCatch({
  state_file <- "estado_hashes.json"
  old_hashes <- if (file.exists(state_file)) fromJSON(state_file) else list()
  new_hashes <- old_hashes
  
  df <- read_excel("Links_alertas.xlsx")
  archivos_actualizados <- c()
  
  if (!dir.exists("temp_downloads")) dir.create("temp_downloads")
  
  for (i in seq_len(nrow(df))) {
    file_name <- df$nombre_archivo[i]
    file_url  <- df$link[i]
    dest_path <- file.path("temp_downloads", paste0("temp_", i, ".xlsx"))
    
    res <- GET(file_url, write_disk(dest_path, overwrite = TRUE))
    
    if (status_code(res) == 200) {
      current_hash <- digest(dest_path, algo = "md5", file = TRUE)
      previous_hash <- old_hashes[[file_name]]
      
      if (!is.null(previous_hash) && current_hash != previous_hash) {
        archivos_actualizados <- c(archivos_actualizados, file_name)
      }
      
      new_hashes[[file_name]] <- current_hash
    }
  }
  
  # Guardar el nuevo estado
  write_json(new_hashes, state_file, pretty = TRUE)
  
  # Si cambió algún archivo, enviar alerta
  if (length(archivos_actualizados) > 0) {
    end_time <- Sys.time()
    duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")))
    hora_str <- format(Sys.time(), "%I:%M%p", tz = "America/Santo_Domingo")
    
    for (arch in archivos_actualizados) {
      msg <- paste0(
        "Hora revisión ", hora_str, "\n",
        "Actualizacion \"", arch, "\"\n",
        "Tiempo de ejecución: ", duration, "segundos"
      )
      send_telegram(msg)
    }
  }
  
}, error = function(e) {
  send_telegram("Error revisando actualizaciones")
  stop(e)
})
