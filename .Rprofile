# In production on the Shiny Server
if(grepl('srv', getwd())){
  
  print('Loading R profile In production')
  Sys.setenv(RENV_PATHS_CACHE = "/opt/renv/cache")
  wd <- getwd()
  lib <- file.path(wd, "renv", "library")
  Sys.setenv("RENV_PATHS_LIBRARY" = lib)
  print(paste("RENV_PATHS_LIBRARY=" , lib))
  print(paste("Working directory=" , wd))
  
} else {
  
  lib = 'renv/library'
  print('In Dev on my machine')
  Sys.setenv("RENV_PATHS_LIBRARY" = lib)
  print(paste("RENV_PATHS_LIBRARY=" , lib))
}

source("renv/activate.R")
