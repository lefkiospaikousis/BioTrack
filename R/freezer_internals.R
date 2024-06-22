
freezer_internals <- function(freezer){
  
  if(freezer == freezer_80_big){
    
    return( list( rack = LETTERS[1:19], drawer = c(1:6), box = c(1:5) ) )
    
  }
  
  if(freezer == freezer_80_small){
    
    return(
      list( rack = LETTERS[1:4], drawer = c(1:5), box = c(1:3) )
    )
  }
  
  if(freezer == freezer_20){
    return(
      list( rack = c('A1', 'A2', 'B1', 'B2'), drawer = NULL, box = NULL )
    )
  }
  
  if(freezer == freezer_04){
    return(
      list( rack = NULL, drawer = NULL, box = NULL )
    )
  }
  
  return(NULL)
}
