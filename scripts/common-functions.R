# print names and associated lables of variables (if attr(.,"label)) is present
names_labels <- function(ds){
  dd <- as.data.frame(ds)

  nl <- data.frame(matrix(NA, nrow=ncol(dd), ncol=2))
  names(nl) <- c("name","label")
  for (i in seq_along(names(dd))){
    # i = 2
    nl[i,"name"] <- attr(dd[i], "names")
    if(is.null(attr(dd[[i]], "label")) ){
      nl[i,"label"] <- NA}else{
        nl[i,"label"] <- attr(dd[,i], "label")
      }
  }
  return(nl)
}
# names_labels(ds=oneFile)

# adds neat styling to your knitr table
neat <- function(x, output_format = "html"){
  # knitr.table.format = output_format
  if(output_format == "pandoc"){
    x_t <- knitr::kable(x, format = "pandoc")
  }else{
    x_t <- x %>%
      # x %>%
      # knitr::kable() %>%
      knitr::kable(format=output_format) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed","responsive"),
        # bootstrap_options = c( "condensed"),
        full_width = F,
        position = "left"
      )
  }
  return(x_t)
}
# ds %>% distinct(id) %>% count() %>% neat(10)

# adds a formated datatable
neat_DT <- function(x, filter_="top",...){

  xt <- x %>%
    as.data.frame() %>%
    DT::datatable(
      class   = 'cell-border stripe'
      ,filter  = filter_
      ,options = list(
        pageLength = 6,
        autoWidth  = FALSE
      )
      , ...
    )
  return(xt)
}

dt <- neat_DT



# ---- histogram-discrete -----------------------
# function to create discrete histogram. taken from RAnalysisSkeleton
# histogram_discrete <- function(
#   d_observed,
#   variable_name,
#   levels_to_exclude   = character(0),
#   main_title          = variable_name,
#   x_title             = NULL,
#   y_title             = "Number of Included Records",
#   text_size_percentage= 6,
#   bin_width           = 1L) {
#   # d_observed <- ds %>% select(id, Q9)
#   # variable_name <- "Q9"
#
#   d_observed <- as.data.frame(d_observed) #Hack so dplyr datasets don't mess up things
#   if( !base::is.factor(d_observed[, variable_name]) )
#     d_observed[, variable_name] <- base::factor(d_observed[, variable_name])
#
#   d_observed$iv <- base::ordered(d_observed[, variable_name], levels=rev(levels(d_observed[, variable_name])))
#
#   ds_count <- plyr::count(d_observed, vars=c("iv"))
#   # if( base::length(levels_to_exclude)>0 ) { }
#   ds_count <- ds_count[!(ds_count$iv %in% levels_to_exclude), ]
#
#   ds_summary <- plyr::ddply(ds_count, .variables=NULL, transform, count=freq, proportion = freq/sum(freq) )
#   ds_summary$percentage <- base::paste0(base::round(ds_summary$proportion*100), "%")
#   ds_summary$display_text <- ds_summary$percentage
#   # ds_summary$display_text <- paste0(ds_summary$percentage," / ", ds_summary$count)
#   ds_summary$display_text <- paste0(ds_summary$count, " (",ds_summary$percentage,")")
#
#   y_title <- base::paste0(y_title, " (n=", scales::comma(base::sum(ds_summary$freq)), ")")
#
#   g <- ggplot(ds_summary, aes_string(x="iv", y="count", fill="iv", label="display_text")) +
#     geom_bar(stat="identity") +
#     geom_text(stat="identity", size=text_size_percentage, hjust=.8) +
#     scale_y_continuous(labels=scales::comma_format()) +
#     labs(title=main_title, x=x_title, y=y_title) +
#     coord_flip()
#
#   theme  <- theme_light(base_size=14) +
#     theme(legend.position = "none") +
#     theme(panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank()) +
#     theme(axis.text.x=element_text(colour="gray40")) +
#     theme(axis.title.x=element_text(colour="gray40")) +
#     theme(axis.text.y=element_text(size=14)) +
#     theme(panel.border = element_rect(colour="gray80")) +
#     theme(axis.ticks.length = grid::unit(0, "cm"))
#
#   return( g + theme )
# }

histogram_discrete <- function (d_observed, variable_name, levels_to_exclude = character(0),
          main_title = base::gsub("_", " ", variable_name, perl = TRUE),
          x_title = NULL, y_title = "Number of Included Records", text_size_percentage = 6,
          bin_width = 1L, font_base_size = 12)
{
  # d_observed <- ds
  # variable_name <- "Q9"
  # levels_to_exclude = character(0)
  # main_title = base::gsub("_", " ", variable_name, perl = TRUE)
  # x_title = NULL
  # y_title = "Number of Included Records"
  # text_size_percentage = 6
  # bin_width = 1L
  # font_base_size = 12

  if (!inherits(d_observed, "data.frame"))
    stop("`d_observed` should inherit from the data.frame class.")
  if (!base::is.factor(d_observed[[variable_name]]))
    d_observed[[variable_name]] <-
      base::factor(d_observed[[variable_name]])
  d_observed$iv <- base::ordered(d_observed[[variable_name]],
                                 levels = rev(levels(d_observed[[variable_name]])))
  count_record_start <- nrow(d_observed)
  d_observed <- d_observed[!(d_observed$iv %in% levels_to_exclude),]
  count_record_stop <- nrow(d_observed)
  d_summary <- d_observed %>%
    dplyr::count_("iv") %>%
    # dplyr::mutate_(count = "n",
    #                proportion = "n / sum(n)",
    #                percent_pretty = "base::paste0(base::round(proportion*100), '%')",
    #                # display_text = percent_pretty,
    #                display_text = paste0(count, " (",percent_pretty,")")
    #                )
  dplyr::mutate(count = n,
                 proportion = n / sum(n),
                 percent_pretty = base::paste0(base::round(proportion*100), '%'),
                 # display_text = percent_pretty,
                 display_text = paste0(count, " (",percent_pretty,")")
  )


  y_title <-
    base::paste0(
      y_title,
      ", n=",
      scales::comma(base::sum(d_summary$n)),
      "\n(Excluded records, m=",
      scales::comma(count_record_start -
                      count_record_stop),
      ")"
    )
  g <- ggplot2::ggplot(
    d_summary,
    ggplot2::aes_string(
      x = "iv",
      y = "count",
      fill = "iv",
      label = "display_text"
    )
  ) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.4) + ggplot2::geom_text(stat = "identity",
                                                                           size = text_size_percentage, hjust = 0.5) + ggplot2::scale_y_continuous(labels = scales::comma_format())
  g <-
    g + ggplot2::labs(title = main_title, x = x_title, y = y_title) +
    ggplot2::coord_flip()
  theme <- ggplot2::theme_light(base_size = font_base_size) +
    ggplot2::theme(legend.position = "none") + ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    ) + ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "gray40")) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(colour = "gray40")) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = font_base_size +
                                                         2L)) + ggplot2::theme(panel.border = ggplot2::element_rect(colour = "gray80")) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank())
  # g+theme
  return(g + theme)
}

# ----- histogram-continuous -----------------------
# function to create continuous histogram. taken from RAnalysisSkeleton
# histogram_continuous <- function(
#   d_observed,
#   variable_name,
#   bin_width      = NULL,
#   main_title     = variable_name,
#   x_title        = paste0(variable_name, " (each bin is ", scales::comma(bin_width), " units wide)"),
#   y_title        = "Frequency",
#   rounded_digits = 0L
# ) {
#
#   d_observed <- as.data.frame(d_observed) #Hack so dplyr datasets don't mess up things
#   d_observed <- d_observed[!base::is.na(d_observed[, variable_name]), ]
#
#   ds_mid_points <- base::data.frame(label=c("italic(X)[50]", "bar(italic(X))"), stringsAsFactors=FALSE)
#   ds_mid_points$value <- c(stats::median(d_observed[, variable_name]), base::mean(d_observed[, variable_name]))
#   ds_mid_points$value_rounded <- base::round(ds_mid_points$value, rounded_digits)
#
#   g <- ggplot(d_observed, aes_string(x=variable_name)) +
#     geom_histogram(binwidth=bin_width, fill="gray70", color="gray90", position=position_identity()) +
#     geom_vline(xintercept=ds_mid_points$value, color="gray30") +
#     geom_text(data=ds_mid_points, aes_string(x="value", y=0, label="value_rounded"), color="tomato", hjust=c(1, 0), vjust=.5) +
#     scale_x_continuous(labels=scales::comma_format()) +
#     scale_y_continuous(labels=scales::comma_format()) +
#     labs(title=main_title, x=x_title, y=y_title) +
#     theme_light() +
#     theme(axis.ticks.length = grid::unit(0, "cm"))
#
#   ds_mid_points$top <- stats::quantile(ggplot2::ggplot_build(g)$panel$ranges[[1]]$y.range, .8)
#   g <- g + ggplot2::geom_text(data=ds_mid_points, ggplot2::aes_string(x="value", y="top", label="label"), color="tomato", hjust=c(1, 0), parse=TRUE)
#   return( g )
# }


histogram_continuous <- function (d_observed, variable_name, bin_width = NULL, main_title = base::gsub("_",
                                                                               " ", variable_name, perl = TRUE), sub_title = NULL, caption = paste0("each bin is ",
                                                                                                                                                    scales::comma(bin_width), " units wide"), x_title = variable_name,
          y_title = "Frequency", x_axis_format = scales::comma_format(),
          rounded_digits = 0L, font_base_size = 12)
{
  if (!inherits(d_observed, "data.frame"))
    stop("`d_observed` should inherit from the data.frame class.")
  d_observed <- d_observed[!base::is.na(d_observed[[variable_name]]),
                           ]
  non_empty <- (nrow(d_observed) >= 1L)
  if (non_empty) {
    ds_mid_points <- base::data.frame(label = c("italic(X)[50]",
                                                "bar(italic(X))"), stringsAsFactors = FALSE)
    ds_mid_points$value <- c(stats::median(d_observed[[variable_name]]),
                             base::mean(d_observed[[variable_name]]))
    ds_mid_points$value_rounded <- sprintf("%.*f", rounded_digits,
                                           ds_mid_points$value)
    if (ds_mid_points$value[1] < ds_mid_points$value[2]) {
      h_just <- c(1.1, -0.1)
    }
    else {
      h_just <- c(-0.1, 1.1)
    }
  }
  else {
    main_title <- paste0("Empty: ", main_title)
    caption <- "The variable contains only missing values.\nThere is nothing to graph."
    ds_mid_points <- tibble::tribble(~label, ~value, ~value_rounded,
                                     "italic(X)[50]", NA_real_, NA_character_, "bar(italic(X))",
                                     NA_real_, NA_character_)
    h_just <- c(1.1, -0.1)
  }
  palette_midpoint <- c("#2274A5", "#32936F")
  g <- ggplot2::ggplot(d_observed, ggplot2::aes_string(x = variable_name)) +
    ggplot2::geom_histogram(binwidth = bin_width, position = ggplot2::position_identity(),
                            fill = "gray92", color = "gray80", size = 1, alpha = 0.7) +
    ggplot2::geom_vline(xintercept = ds_mid_points$value,
                        color = palette_midpoint, na.rm = T) + ggplot2::geom_text(data = ds_mid_points,
                                                                                  ggplot2::aes_string(x = "value", y = -Inf, label = "value_rounded"),
                                                                                  color = palette_midpoint, hjust = h_just, vjust = -0.2,
                                                                                  na.rm = T) + ggplot2::geom_text(data = ds_mid_points,
                                                                                                                  ggplot2::aes_string(x = "value", y = Inf, label = "label"),
                                                                                                                  color = palette_midpoint, hjust = h_just, vjust = 1.2,
                                                                                                                  parse = TRUE, na.rm = T) + ggplot2::scale_x_continuous(labels = x_axis_format) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::labs(title = main_title, subtitle = sub_title,
                  caption = caption, x = x_title, y = y_title)
  g <- g + ggplot2::theme_light(base_size = font_base_size) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = "gray90")) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = "gray94")) +
    ggplot2::theme(plot.caption = ggplot2::element_text(color = "gray60")) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(color = "gray60"))
  return(g)
}
# ---- make-correlation-matrix ---------------------
make_corr_matrix <- function(ds,metaData,items){

  # d <- ds %>% dplyr::select(foc_01:foc_49)
  d <- ds %>% dplyr::select_(.dots=items)

  rownames <- metaData %>%
    dplyr::filter(name_new %in% items) %>%
    # dplyr::mutate(name_ = paste0(gsub("foc_", "", items),"---",domain, "---", label_graph))
    dplyr::mutate(name_ = paste0(gsub("foc_", "", items),"-", label_graph))
  # dplyr::mutate(name_ = paste0(gsub("foc_", "", vars_49),"-",domain, "-",label_graph))
  # dplyr::mutate(name_ = paste0(gsub("foc_", "", vars_49),"-",label_graph))
  rownames <- rownames[,"name_"]

  d <- sapply(d, as.numeric)
  cormat <- cor(d)
  colnames(cormat) <- rownames; rownames(cormat) <- rownames
  return(cormat)
}
# ---- make-correlation-graph ----------------------
make_corr_plot <- function (
  corr,
  lower="number",
  upper="circle",
  tl.pos=c("d","lt", "n"),
  diag=c("n", "l", "u"),
  bg="white",
  addgrid.col="gray", ...
){

  diag <- match.arg(diag)
  tl.pos <- match.arg(tl.pos)
  n <- nrow(corr)
  # corrplot::corrplot(corr, type="upper", method=upper, diag=TRUE, tl.pos=tl.pos, ...)
  corrplot::corrplot(corr, type="upper", method=upper, diag=TRUE, tl.pos=tl.pos)
  # corrplot::corrplot(corr, add=TRUE, type="lower", method=lower, diag=(diag == "l"), tl.pos="n", cl.pos="n", ...)
  corrplot::corrplot(corr, add=TRUE, type="lower", method=lower, diag=(diag == "l"), tl.pos="n", cl.pos="n")
  if (diag == "n" & tl.pos != "d") {
    symbols(1:n, n:1, add=TRUE, bg=bg, fg=addgrid.col,  inches=FALSE, squares=rep(1, n))
  }
}
