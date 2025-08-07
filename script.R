library(httr)      # Gestão de requisições HTTP
library(jsonlite)  # Processamento de dados JSON
library(lubridate) # Tratamento de dados temporais

# Configuração dos endpoints oficiais
endpoints_ipma <- list(
  d0 = list(
    url = "https://api.ipma.pt/open-data/forecast/meteorology/rcm/rcm-d0.json",
    ficheiro = "rcm-d0.json",
    descricao = "Previsão meteorológica para o dia atual"
  ),
  d1 = list(
    url = "https://api.ipma.pt/open-data/forecast/meteorology/rcm/rcm-d1.json", 
    ficheiro = "rcm-d1.json",
    descricao = "Previsão meteorológica para o dia seguinte"
  )
)

# Função de download individual com validação completa
download_ficheiro_ipma <- function(url, nome_ficheiro, descricao) {
  
  cat("Processando:", descricao, "\n")
  cat("Fonte:", url, "\n")
  
  # Requisição HTTP com configurações de segurança
  resposta <- tryCatch({
    GET(url, 
        timeout(45),
        add_headers(
          "User-Agent" = "R Script - Análise Meteorológica Governamental",
          "Accept" = "application/json"
        ))
  }, error = function(e) {
    cat("ERRO - Falha na conexão:", e$message, "\n")
    return(NULL)
  })
  
  # Validação do estado da resposta
  if (is.null(resposta)) {
    cat("FALHA: Impossível estabelecer conexão com servidor IPMA\n")
    return(FALSE)
  }
  
  codigo_estado <- status_code(resposta)
  if (codigo_estado != 200) {
    cat("ERRO HTTP:", codigo_estado, "-", http_status(resposta)$message, "\n")
    return(FALSE)
  }
  
  # Extração e validação do conteúdo JSON
  conteudo_bruto <- content(resposta, "text", encoding = "UTF-8")
  
  # Verificação da integridade dos dados
  dados_estruturados <- tryCatch({
    fromJSON(conteudo_bruto)
  }, error = function(e) {
    cat("ERRO - Dados JSON corrompidos:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(dados_estruturados)) {
    cat("FALHA: Estrutura de dados inválida recebida\n")
    return(FALSE)
  }
  
  # Gravação do ficheiro oficial
  write(conteudo_bruto, file = nome_ficheiro)
  
  # Relatório de operação
  info_ficheiro <- file.info(nome_ficheiro)
  cat("SUCESSO: Ficheiro", nome_ficheiro, "criado\n")
  cat("Tamanho:", info_ficheiro$size, "bytes\n")
  cat("Timestamp:", format(info_ficheiro$mtime, "%d/%m/%Y %H:%M:%S"), "\n\n")
  
  return(TRUE)
}

# Procedimento principal de recolha
executar_recolha_dados <- function() {
  
  cat("=== RECOLHA DE DADOS METEOROLÓGICOS IPMA ===\n")
  cat("Início da operação:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n")
  cat("Modelo: RCM (Regional Climate Model)\n")
  cat("Cobertura: Território nacional português\n\n")
  
  resultados <- list()
  sucessos <- 0
  falhas <- 0
  
  # Processamento sequencial dos endpoints
  for (periodo in names(endpoints_ipma)) {
    
    endpoint <- endpoints_ipma[[periodo]]
    
    cat("--- Processamento", toupper(periodo), "---\n")
    
    resultado <- download_ficheiro_ipma(
      endpoint$url, 
      endpoint$ficheiro, 
      endpoint$descricao
    )
    
    resultados[[periodo]] <- resultado
    
    if (resultado) {
      sucessos <- sucessos + 1
    } else {
      falhas <- falhas + 1
    }
    
    # Pausa operacional entre requisições
    Sys.sleep(2)
  }
  
  # Relatório Final de Operação
  cat("=== RELATÓRIO FINAL ===\n")
  cat("Operações concluídas:", sucessos + falhas, "\n")
  cat("Sucessos:", sucessos, "\n")
  cat("Falhas:", falhas, "\n")
  cat("Taxa de sucesso:", round((sucessos / (sucessos + falhas)) * 100, 1), "%\n")
  cat("Conclusão:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n")
  
  # Verificação da disponibilidade dos ficheiros
  if (sucessos > 0) {
    cat("\n=== FICHEIROS DISPONÍVEIS ===\n")
    
    for (periodo in names(endpoints_ipma)) {
      ficheiro <- endpoints_ipma[[periodo]]$ficheiro
      
      if (file.exists(ficheiro) && resultados[[periodo]]) {
        info <- file.info(ficheiro)
        cat("✓", ficheiro, "- Disponível (", info$size, "bytes )\n")
      } else {
        cat("✗", ficheiro, "- Indisponível\n")
      }
    }
  }
  
  return(resultados)
}


# Execução principal
cat("Iniciando recolha sistemática de dados meteorológicos...\n\n")

resultado_operacao <- executar_recolha_dados()