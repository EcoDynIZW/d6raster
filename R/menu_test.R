data_df <- dplyr::tibble(re_1 = 1, re_2 = 1, re_3 = 1)

meta_df <- dplyr::tibble(dat = 1)

meta_df <- meta_df %>%
  dplyr::mutate(col_ch = names(data_df)[utils::menu(c(paste(names(data_df), lapply(names(data_df), class), sep = " - ")))])

let <- c("yes", "no")
let[menu(let)]
cat("Choice: ", let[menu(let)], "\n")

if(print("add another column?")
   c("yes", "no")[utils::menu(c("yes", "no"))] == "yes"){
  print("works")
  # meta_df <- meta_df %>%
  # dplyr::mutate(col_ch = names(data_df)[utils::menu(c(paste(names(data_df), lapply(names(data_df), class), sep = " - ")))])
  }

col_choose <- names(request)[utils::menu(c(paste(names(request), lapply(request, class), sep = " - ")))]
rast_col <- if(rlang::is_empty(col_choose) == TRUE){
  #readline("Set name: ")
  c("area", "distance", "custom_categorical", "several columns")[utils::menu(c("area", "distance", "custom_categorical", "several columns"),
                                                                             title="Choose one?")]

} else{
  col_choose
}
