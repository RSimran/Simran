#' XGB Importance Function
#'
#' This function allows you to graph the gain, cover and frequency of an XGB model.
#' @param Model Type: XGBoost Model. Must be defined.
#' @param features Type: character. Vector. Must be defined.
#' @param gainColor Type: character. Defines the display color for Gain column.
#' @param digits Type: integer. The length of decimal place to display results.
#' @param coverColor Type: character. Defines the display color of cover Column.
#' @param freqColor Type: character. Defines the display color for frequency column.
#' @param pageLength Type: integer. Defines the number of rows per page.
#' @param menuLength Type: integer. Changes the option in the page length menu.
#' 
#' 
#' 
#' @keywords xgb.importance
#' @export
#' @examples
#' cat_function()


xgb.importance.table <- function(model, feature_names = NULL, gainColor = 'lightgreen', digits = 6, coverColor = 'lightblue', freqColor = 'lightgrey', pageLength = 20, lengthMenu = c(5, 10, 15,20, 25, 50, 100, 250, 500)) {
  if(!is.data.frame(importance)){
    importance <- as.data.frame(xgb.importance(model = model, feature_names = feature_names))
  }
  
  datatable(importance,
            filter = 'top',
            class = 'hover stripe',
            options = list(pageLength = pageLength,
                           lengthMenu = lengthMenu)
            ) %>% formatStyle('Gain',
                              background = styleColorBar(range(importance$Gain, na.rm = TRUE, finite = TRUE), gainColor),
                              backgroundSize = '100% 90%',
                              backgroundRepeat = 'no-repeat',
                              backgroundPosition = 'center') %>%
              formatStyle('Cover',
                          background = styleColorBar(range(importance$Cover, na.rm = TRUE, finite = TRUE), coverColor),
                          backgroundSize = '100% 90%',
                          backgroundRepeat = 'no-repeat',
                          backgroundPosition = 'center') %>%
              formatStyle('Frequency',
                          background = styleColorBar(range(importance$Frequency, na.rm = TRUE, finite = TRUE), freqColor),
                          backgroundSize = '100% 90%',
                          backgroundRepeat = 'no-repeat',
                          backgroundPosition = 'center') %>%
              formatPercentage(columns = c('Gain'),
                               digits = digits) %>%
              formatPercentage(columns = c('Cover'),
                               digits = digits) %>%
              formatPercentage(columns = c('Frequency'),
                               digits = digits)
      
}