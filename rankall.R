rankall <- function(outcome, num = "best"){
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Check that state and outcome are valid
  outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  
  ##las funciones any y %in% logran lo mismo en este caso, 
  ##las dejo asi para saber que se pueden usar así  
  if (!(outcome %in% names(outcomes))) {
    stop("invalid outcome")
    
  }
  
  #cambio el nombre las columnas por facilidad
  colnames(df)[c(2, 7,outcomes[outcome])] <- c('hospital', 'state', 'outcome')
  
  #filtro el dataframe original para obtener uno con solo las variables que necesito
  df_outcome <- na.omit(df[, c('hospital', 'state', 'outcome')])
  
  #ordeno el df por la columna 'hospital' alfabeticamente, suponiendo que
  #la funcion min() va extraer el primero que sea el minimo
  df_outcome <- df_outcome[order(df_outcome$state, df_outcome$outcome, df_outcome$hospital), ]
  
  
  #hace una lista de dataframes separados por 'state'
  df_list <- split(df_outcome, df_outcome$state)
  
  #funcion que aplica a cada df de la lista una func anonima para extraer
  #la info que necesito
  hospital_names <- sapply(df_list, function(x){

    if (num == 'best'){
      num <- 1
      x[num, 'hospital']
      
    } else if (num == 'worst'){
        #nrow(x) va ser el ultimo elemento de cada dataframe
        num_wor <- nrow(x)
        x[num_wor, 'hospital']
    } else{
      x[num, 'hospital']
    }})
  
  
  state_names <- names(hospital_names)
  
  #construyo un df con vectores correspondientes
  output_df <- data.frame(hospital=hospital_names, state = state_names, row.names = state_names)
  
  output_df

  

  
  

  
  

  
  
}