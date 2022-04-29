

table_mc <- function(data_mc,
                    caption = TRUE,
                    reference_category = 0,
                    first = 1, 
                    second = 2) {

  # extract data
  df <- sapply(data_mc, function(x) unlist(attr(x, "iter"))) %>%
    t() %>%
    as.data.frame()

  rn <- function(df, ...) {
    vars <- c(...)
    for(i in seq_along(vars)) {
      id <- which(names(df) %in% vars[i])
      if (length(id) > 0) names(df)[id] <- names(vars[i])
    }
    df
  }

  # reorganize data
  df <- df %>%
    rn(
      "Measurement times" = "length",
      "A" = "A_length",
      "B" = "B_length",
      "Trend effect" =  "trend_effect",
      "Intervention effect" =  "level_effect"
    ) %>%
    mutate(
      'Tau<sub>AB</sub>' = sapply(data_mc, function(x) x$Power[1]),
      'Tau<sub>trendA</sub>' = sapply(data_mc, function(x) x$Power[2]),
      'Tau<sub>trendA+B</sub>' = sapply(data_mc, function(x) x$Power[3]),
      'Tau<sub>adj</sub>' = sapply(data_mc, function(x) x$Power[4]),
      'Tau<sub>AB</sub> ' = sapply(data_mc, function(x) x$`Alpha Error`[1]),
      'Tau<sub>trendA</sub> ' = sapply(data_mc, function(x) x$`Alpha Error`[2]),
      'Tau<sub>trendA+B</sub> ' = sapply(data_mc, function(x) x$`Alpha Error`[3]),
      'Tau<sub>adj</sub> ' = sapply(data_mc, function(x) x$`Alpha Error`[4]),
      ' Tau<sub>AB</sub> ' = sapply(data_mc, function(x) x$Correct[1]),
      ' Tau<sub>trendA</sub> ' = sapply(data_mc, function(x) x$Correct[2]),
      ' Tau<sub>trendA+B</sub> ' = sapply(data_mc, function(x) x$Correct[3]),
      ' Tau<sub>adj</sub> ' = sapply(data_mc, function(x) x$Correct[4])
    ) %>%
    select(-n_sims)
  
  df <- df[order(df[,first], df[,second]),]

  if (reference_category == 0) {
    
    for(i in 11:14)
      df[[i]] = cell_spec(
        df[[i]], 
        color = ifelse(df[[i-8]] >= 80 & df[[i-4]] <= 5, "green", "black"), 
        bold = ifelse(df[[i-8]] >= 80 & df[[i-4]] <= 5, TRUE, FALSE))
    
    
    for(i in 3:6)
      df[[i]] = cell_spec(
        df[[i]], 
        color = ifelse(df[[i]] >= 80, "green", "black"), 
        bold = ifelse(df[[i]] >= 80, TRUE, FALSE)
      )
    
    for(i in 7:10)
      df[[i]] = cell_spec(
        df[[i]], 
        color = ifelse(df[[i]] <= 5, "green", "black"), 
        bold = ifelse(df[[i]] <= 5, TRUE, FALSE)
      )


    
  }

  if (reference_category == 1) {
    
    for(i in 4:6) df[[i]] <- round(df[[i]] - df[[2+reference_category]], 1)
    for(i in 8:10) df[[i]] <- round(df[[i]] - df[[6+reference_category]], 1)
    for(i in 12:14) df[[i]] <- round(df[[i]] - df[[10+reference_category]], 1)
    
    for(i in c(4:6, 12:14))
      df[[i]] = cell_spec(
        df[[i]], 
        color = ifelse(df[[i]] > 2, "green", ifelse(df[[i]] < -2, "red", "black" ))
      )
    
    for(i in c(8:10))
      df[[i]] = cell_spec(
        df[[i]], 
        color = ifelse(df[[i]] > 2, "red", ifelse(df[[i]] < -2, "green", "black" ))
      )
    
    
    
  }
  
  kbl(df, escape = F, row.names = FALSE) %>% 
    kable_classic() %>%
    add_header_above(
      c(" " = 2, "Power" = 4, "Alpha error" = 4, "Correct" = 4)
    )
  
}
