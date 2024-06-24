setup <- function() { 
  have_python <- py_available(initialize = TRUE)
  version <- "3.12.3"
  
  if (!have_python) {
    user_choice_python <- menu(c("Yes", "No"), 
                               title = paste("Python is not installed. Do you want to install Python version ", version, "?", sep = ""))
    if (user_choice_python == 1) {
      path <- install_python(version)
      Sys.setenv(RETICULATE_PYTHON = path)
    } 
    
    else {
      stop("Python installation is required for this application to run.")
    }
  }
  
  virtualenv_create("r-reticulate", version = version)
  use_virtualenv("r-reticulate", required = TRUE)
  
  modules <- c("opencv-python", "numpy", "scikit-image", 
               "scipy", "matplotlib")
  have_modules <- py_module_available(modules)
  
  if (!have_modules) {
    
    # user_choice_modules <- menu(c("Yes", "No"), 
                                # title = paste("Do you want to install the following Python modules in the virtual environment 'r-reticulate': ", 
                                              # paste(modules, collapse = ", "), "?", sep = ""))
    # if (user_choice_modules == 1) {  
      py_install(envname = "r-reticulate", modules)
    # } 
    
    # else {  
      # message("Skipping Python modules installation.")
    # }
  }
}