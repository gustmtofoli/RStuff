library(plyr)

#L_procedures <- c()
#L_values <- c()
#L_freq <- c()
#L_id_guia <- c()

procedures.id <- c()
procedures.data <- c()
procedures.codigo_procedimento <- c()
procedures.valor_total <- c()
procedures.id_guia <- c()
procedures.id_tabela <- c()

start.time <- Sys.time()
procedures <- read.csv('exportar_procedimento_executado.csv', sep = '\t')
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

start.time <- Sys.time()
for(cp in sort(unique(procedures$CODIGO_PROCEDIMENTO))) {
  procedures_subset <- subset(procedures, procedures$CODIGO_PROCEDIMENTO == cp)
  outliers <- boxplot.stats(procedures_subset$VALOR_TOTAL)$out
  if (length(outliers) != 0) {
    df_codproc_outliers <- data.frame(CODIGO_PROCEDIMENTO = c(cp), OUTLIERS = c(outliers))
    
    df_test <- procedures_subset[procedures_subset$VALOR_TOTAL %in% unique(df_codproc_outliers$OUTLIERS), ]
    
    procedures.id <- c(procedures.id, df_test$ID)
    procedures.data <- c(procedures.data, df_test$DATA)
    procedures.codigo_procedimento <- c(procedures.codigo_procedimento, df_test$CODIGO_PROCEDIMENTO)
    procedures.valor_total <- c(procedures.valor_total, df_test$VALOR_TOTAL)
    procedures.id_guia <- c(procedures.id_guia, df_test$ID_GUIA)
    procedures.id_tabela <- c(procedures.id_tabela, df_test$ID_TABELA)
  }
}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

# TABELA OUTLIERS - todas as ocorrências que contém outliers
df_test_2 <- data.frame(ID = procedures.id, 
                        DATA = procedures.data, 
                        CODIGO_PROCEDIMENTO =  procedures.codigo_procedimento, 
                        VALOR_TOTAL = procedures.valor_total, 
                        ID_GUIA = procedures.id_guia, 
                        ID_TABELA = procedures.id_tabela)

# TABELA OUTLIERS - calcula o total de outliers para cada codigo de procedimento e valor total
df_codproc_outliers_count <- count(df_test_2, c("CODIGO_PROCEDIMENTO", "VALOR_TOTAL"))

# TABELA OUTLIERS - calcula o total de outliers para cada codigo de procedimento
df_codproc_outliers_aggregate <- aggregate(df_codproc_outliers_count$freq, by = list(df_codproc_outliers_count$CODIGO_PROCEDIMENTO), FUN = sum)
df_codproc_outliers_aggregate[order(df_codproc_outliers_aggregate$x, decreasing = TRUE), ]
