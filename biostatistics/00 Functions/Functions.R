# Functions

# Cleanup

clean_up <- function(){
  gc()
  rm(list = setdiff(ls(envir = .GlobalEnv), "clean_up"), envir = .GlobalEnv) 
  cat('\014')
  graphics.off()
}


# Negative Binomial - Build Formula

build_formula <- function(vars, data, response = "AVAL") {
  selected_vars <- c()
  
  for (v in 1:length(vars)) {
    var <- vars[v]
    if (length(unique(data[[var]])) == 2) {
      selected_vars <- c(selected_vars, var)
    }
  }
  
  rhs <- paste(selected_vars, collapse = " + ")
  formula_text <- paste("AVAL", "~",  "Treatment +", rhs, "- 1")
  formula <- as.formula(formula_text)
  return(formula)
}

