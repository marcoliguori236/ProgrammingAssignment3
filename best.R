best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  
  ## Check that state and outcome are valid
  states <- unique(df$State)
  outcomes <- c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  
  ##las funciones any y %in% logran lo mismo en este caso, 
  ##las dejo asi para saber que se pueden usar así  
  if (!any(state == states)){
      stop("invalid state")
  
  } else if (!(outcome %in% names(outcomes))) {
      stop("invalid outcome")
    
    }
      
    
  #cambio el nombre las columnas por facilidad
  colnames(df)[c(2, 7,outcomes[outcome])] <- c('hospital', 'state', 'outcome')
  
  #filtro el dataframe original para obtener uno con solo las variables que necesito
  df_outcome <- na.omit(df[df$state == state, c('hospital', 'state', 'outcome')])
  
  #ordeno el df por la columna 'hospital' alfabeticamente, suponiendo que
  #la funcion min() va extraer el primero que sea el minimo
  df_outcome <- df_outcome[order(df_outcome$hospital), ]
  
  
  best <- df_outcome[df_outcome$outcome == min(df_outcome$outcome), 'hospital']

  best
  
}