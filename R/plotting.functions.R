#' Render a scatter plot
#'
#' @export
#'
#' @importFrom ggplot2 aes coord_flip element_blank element_text geom_bar geom_point geom_text ggplot scale_x_continuous scale_y_continuous theme theme_minimal xlab ylab
#' @importFrom plotly ggplotly
#' @importFrom scales label_comma
#'
#' @param x a numeric vector.
#' @param y a numeric vector.
#' @param x_label_text a character element (by default, the vector name).
#' @param y_label_text a character element (by default, the vector name).
#' @param geom_point_size a numeric element (by default, 2).
#' @param element_text_size a numeric value (by default, 12).
#' @param height an integer element representing the height of the plot in pixels.
#' @param width an integer element representing the width of the plot in pixels.
#'
#' @examples
#' # Load demo data
#' data("EpiSim.demo")
#'
#' # Define results
#' outcomes.summary.df <- EpiSim.demo$results
#'
#' get.scatter.plot(
#'   x = outcomes.summary.df$delta.overwrite,
#'   y = outcomes.summary.df$maxInc,
#'   height = 500,
#'   width = 756
#' )
#'
#' @return none.

get.scatter.plot <- function(x, y, x_label_text = deparse(substitute(x)), y_label_text = deparse(substitute(y)), geom_point_size = 2, element_text_size = 12, height = NULL, width = NULL) {
  if(is.null(x) | is.null(y)) {
    return()
  } else {
    df <- data.frame(x, y)
    ggplotly(
      ggplot(data = df, aes(x, y)) +
        geom_point(color = "#428bca", size = geom_point_size) +
        xlab(x_label_text) +
        ylab(y_label_text) +
        scale_y_continuous(labels = label_comma()) +
        scale_x_continuous(labels = label_comma()) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = element_text_size),
          axis.title.x = element_text(size = element_text_size),
          axis.title.y = element_text(size = element_text_size),
          legend.text = element_blank(),
          legend.title = element_blank(),
          legend.position = "none"
        ),
      width = width,
      height = height
    )
  }
}

#' Render a tornado plot
#'
#' @export
#'
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes coord_flip element_blank element_text geom_bar geom_point geom_text ggplot scale_x_continuous scale_y_continuous theme theme_minimal xlab ylab
#' @importFrom plotly ggplotly
#' @importFrom dplyr tibble
#'
#' @param outcome_variable a character element.
#' @param parameters the parms.tried.df data frame.
#' @param outcomes the outcomes.summary.df data frame.
#' @param method a character element ("kendall-partial-correlation-slow", "pearson-partial-correlation-fast", #' "pearson-partial-correlation-slow", "spearman-partial-correlation-slow", "negative-log-p-value", "t-test").
#' @param bin_width a numeric element representing the width of the bars (by default, 0.5).
#' @param element_text_size a numeric element (by default, 12).
#' @param order_by_absolute_value a logical element representing whether to order the bars by absolute value (by default, FALSE).
#' @param add_label a logical element representing whether to add labels to the bars (by default, FALSE).
#' @param height an integer element representing the height of the plot in pixels.
#' @param width an integer element representing the width of the plot in pixels.
#' @param parameter_labels a character vector representing custom labels for all parameters.
#'
#' @examples
#' # Load demo data
#' data("EpiSim.demo")
#'
#' # Define results
#' parameters.swept <- EpiSim.demo$parameters
#' outcomes.summary.df <- EpiSim.demo$results
#'
#' get.tornado.plot(
#'   outcome_variable = "maxInc",
#'   parameters = parameters.swept,
#'   outcomes = outcomes.summary.df,
#'   height = 500,
#'   width = 756
#' )
#'
#' @return none.

get.tornado.plot <- function(outcome_variable, parameters = parms.tried.df, outcomes = outcomes.summary.df, method = "kendall-partial-correlation-slow", bin_width = 0.5, element_text_size = 12, order_by_absolute_value = FALSE, add_label = FALSE, height = NULL, width = NULL, parameter_labels = NULL) {
  if(is.null(outcome_variable) | is.null(parameters) | is.null(outcomes) | is.null(method)) {
    return()
  } else {
    what.matters = assess.parameter.importance(outcomes,names(parameters), outcome_variable, method)
    if(! is.null(parameter_labels) & length(parameter_labels) == length(names(what.matters))) {
      names(what.matters) <- parameter_labels
    }
    correlations <- tibble(variable = names(what.matters), coefficient = what.matters)
    correlations$variable <- factor(correlations$variable)
    if(isTRUE(order_by_absolute_value)) {
      correlations$variable <- fct_reorder(correlations$variable, abs(correlations$coefficient), .desc = FALSE)
    } else {
      correlations$variable <- fct_reorder(correlations$variable, correlations$coefficient, .desc = FALSE)
    }
    if(isTRUE(add_label)) {
      label_content <- round(correlations$coefficient, 3)
    } else {
      label_content <- ""
    }
    ggplotly(ggplot(correlations, aes(x = variable, y = coefficient)) +
     geom_bar(color = "#428bca", fill = "#428bca", stat = "identity", width = bin_width, aes()) +
     geom_text(label = label_content, size = 3.5, hjust = -3) +
     coord_flip() +
     theme_minimal() +
     ylab(paste0("Strength of correlation with ", outcome_variable)) +
     theme(
       plot.title = element_text(size = element_text_size),
       axis.title.y = element_blank(),
       axis.title.x = element_text(size = element_text_size),
       legend.text = element_blank(),
       legend.title = element_blank(),
       legend.position = "none"
     ),
     tooltip = "text",
     width = width,
     height = height
    )
  }
}

#' Render a tornado table
#'
#' @export
#'
#' @importFrom dplyr tibble
#' @importFrom DT datatable
#' @importFrom htmlwidgets JS
#'
#' @param outcome.variable a character element.
#' @param parameters the parms.tried.df data frame.
#' @param outcomes the outcomes.summary.df data frame.
#' @param method a character element ("kendall-partial-correlation-slow", "pearson-partial-correlation-fast", #' "pearson-partial-correlation-slow", "spearman-partial-correlation-slow", "negative-log-p-value", "t-test").
#'
#' @examples
#' # Load demo data
#' data("EpiSim.demo")
#'
#' # Define results
#' parameters.swept <- EpiSim.demo$parameters
#' outcomes.summary.df <- EpiSim.demo$results
#'
#' get.tornado.table(
#'   outcome.variable = "maxInc",
#'   parameters = parameters.swept,
#'   outcomes = outcomes.summary.df
#' )
#'
#' @return none.

get.tornado.table <- function(outcome.variable, parameters = parms.tried.df, outcomes = outcomes.summary.df, method = "kendall-partial-correlation-slow") {
  if(is.null(outcome.variable) | is.null(parameters) | is.null(outcomes) | is.null(method)) {
    return()
  } else {
    what.matters = assess.parameter.importance(outcomes, names(parameters), outcome.variable, method)
    correlations <- tibble(variable = names(what.matters), coefficient = what.matters)
    correlations$variable <- factor(correlations$variable)
    tab <- tibble(Variable = correlations$variable, Method = rep(method, nrow(correlations)), Coefficient = round(correlations$coefficient, 3))
    datatable(
      tab,
      extensions = c("Buttons", "Scroller"),
      rownames = FALSE,
      options = list(
        columnDefs = list(list(visible = FALSE, targets = c())),
        pageLength = 50,
        dom = "Bfrtip",
        buttons = c("colvis", "copy", "csv", "excel", "pdf"),
        deferRender = TRUE,
        searchDelay = 500,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#111'});",
          "}"
        )
      )
    )
  }
}
