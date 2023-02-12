load_data <- function(data) {
  vroom(data) %>% clean_names()
}

clean_data <- function()
  
