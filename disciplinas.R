#'Análise de aprovação das disciplinas
#'Universidade Federal de Campina Gradne
#'Luciano Barosi de Lemos
#'07/Mar/2017

suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(ggplot2))

preparaDados <- function(){
  
  #Arquivos no Diretório Dados que representam períodos
  arquivos <- list.files("./DADOS")
  arquivos <- arquivos[grep("^[0-9]", arquivos)]
  
  #' Lê o arquivo e insere coluna com periodo
  disciplinasAprovacao <- function(arquivo) {
    file            <- paste("./DADOS/",arquivo, sep = "")
    per             <- substr(arquivo,1,5)
    GETdata         <- read.csv(file, skip = 1)
    GETdata$PERIODO <- per
    
  return(GETdata)
  }
  
  #Lê todos os arquivos e consolida ewm dataframe
  GETfiles <- lapply(arquivos, function(x) {
    disciplinasAprovacao(x)
    })
  
  aprovDisciplinas <- do.call(rbind, GETfiles)
  
  aprovDisciplinas$Matriculados %<>% as.numeric(as.character())
  aprovDisciplinas$Reprovados   %<>% as.numeric(as.character())

  #validações
  aprovDisciplinas %<>% filter(., Matriculados > 0 & Reprovados >= 0)
  
  aprovDisciplinas$CENTRO <- as.numeric(as.character(aprovDisciplinas$Cód..UA)) %/% 1000000
  aprovDisciplinas %<>% filter(., !is.na(CENTRO))
  
  aprovDisciplinas$CENTRO %<>% as.factor()                              
  aprovDisciplinas$UA %<>% as.factor()
  
  return(aprovDisciplinas)
}

dados   <- preparaDados()