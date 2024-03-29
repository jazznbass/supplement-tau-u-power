

plot_mc <- function(data_mc,
                    line_curved = FALSE,
                    add_points = TRUE,
                    caption = FALSE,
                    var_x = 1,
                    var_shape = 2,
                    var_facet = "Method",
                    ncol = 2,
                    reverse = FALSE, 
                    marks = c(5, 80),
                    ylim = c(0, 100),
                    ylab = "Percentage",
                    statistic_label = c("Alpha", "Power")) {

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
      "Total number of measurement times" = "length",
      "Number of measurement times phase A" = "A_length",
      "Number of measurement\ntimes phase B" = "B_length",
      "Trend effect" =  "trend_effect",
      "Intervention effect" =  "level_effect",
      "Initial behavior frequency" = "problemintensity"
    ) %>%
    mutate(
      PowerTau_AB = sapply(data_mc, function(x) x$Power[1]),
      PowerTau_trendA = sapply(data_mc, function(x) x$Power[2]),
      PowerTau_trendAB = sapply(data_mc, function(x) x$Power[3]),
      PowerTau_adj = sapply(data_mc, function(x) x$Power[4]),
      'AlphaTau_AB' = sapply(data_mc, function(x) x$`Alpha Error`[1]),
      'AlphaTau_trendA' = sapply(data_mc, function(x) x$`Alpha Error`[2]),
      'AlphaTau_trendAB' = sapply(data_mc, function(x) x$`Alpha Error`[3]),
      'AlphaTau_adj' = sapply(data_mc, function(x) x$`Alpha Error`[4])
    ) %>%
    pivot_longer(cols = c(starts_with("Power"), starts_with("Alpha")),
                 names_to = c("Statistic", "Method"),
                 names_sep = 5,
                 values_to = "y")
  
  #df <- df %>% filter(Statistic == "Power")
  
  

  if (is.numeric(var_x)) var_x <- names(df)[var_x]
  if (is.numeric(var_shape)) var_shape <- names(df)[var_shape]
  if (is.numeric(var_facet)) var_facet <- names(df)[var_facet]


  df[[var_shape]] <- factor(df[[var_shape]])
  df[[var_facet]] <- factor(df[[var_facet]])

  if (reverse) {
    levels(df[[var_facet]]) <- rev(levels(df[[var_facet]]))

  }

  p <- ggplot(
    df,
    aes(
      x = !!sym(var_x),
      y = y,
      color = Statistic,
      shape = !!sym(var_shape)
    )
  )

  if (line_curved) p <- p + geom_smooth(method = "loess", se = FALSE, size = 0.5)
  if (!line_curved) p <- p + geom_line()
  if (add_points) p <- p + geom_point()

  if (!isTRUE(is.na(marks))) 
    p <- p + geom_hline(yintercept = marks, size = 0.3, colour = "grey50")
  
  if (!isTRUE(is.na(ylim))) 
    p <- p + ylim(ylim[1], ylim[2])
  
  p <- p + theme_bw() +
    scale_color_brewer(palette = "Dark2", labels = statistic_label)

  p <- p + scale_x_continuous(
    breaks = unique(df[[var_x]]),
    limits = c(min(df[[var_x]]), max(df[[var_x]]))
  )

  if (var_facet %in% names(df)) {
    id <- which(names(df) == var_facet)
    var_facet <- paste0("`", var_facet, "`")
    names(df)[id] <- var_facet
    p <- p + facet_wrap(var_facet, ncol = ncol,labeller = .label_both)
  }

  p <- p + ylab(ylab)
  
  #p <- p + scale_color_manual(labels = statistic_label, values = c("red", "blue")) 
  
  if (isTRUE(caption)) {
    design <- attr(data_mc, "design")
    design <- paste0(names(design), " = ", design, collapse = "\n")
    iterations <- attr(data_mc, "iterations")
    iterations <- paste0(names(iterations), " = ", iterations, collapse = "\n")
    caption <- paste0(design, "\n", iterations, collape = "")
    p <- p + labs(caption = caption)
  }

  p
}

.label_both <- function (labels, multi_line = FALSE, sep = ": ")
{
  value <- label_value(labels, multi_line = multi_line)
  variable <- ggplot2:::label_variable(labels, multi_line = multi_line)
  if (multi_line) {
    out <- vector("list", length(value))
    for (i in seq_along(out)) {
      out[[i]] <- paste(variable[[i]], value[[i]], sep = sep)
    }
  }
  else {
    value <- do.call("paste", c(value, sep = ", "))
    variable <- do.call("paste", c(variable, sep = ", "))
    out <- Map(paste, variable, value, sep = sep)
    out <- list(unname(unlist(out)))
  }


  for (i in seq_along(out)) {
    out[[i]] <- paste0(letters[1:length(out[[i]])], ") ", out[[i]])
  }

  out
}
