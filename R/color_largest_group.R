

# Function to color the largest group
#' Title
#'
#' @param df the dataframe used for plotting
#' @param group_col the categorical value that contains the largest group
#' @param num_col the numeric value that defines what is the largest group
#' @param color1 the color you want the largest bar to be
#' @param color2 the color you want all other bars to be
#'
#' @return A scale_fill_manual argument to be used inside a ggplot
#'
#' @examples
#' # Example usage
#' clean_df <- data.frame(
#'   group = c("A", "B", "C", "D"),
#'   value = c(10, 20, 5, 15)
#' )
#'
#' # Plot using the function
#' ggplot(df, aes(x = group, y = value, fill = group)) +
#'   geom_col() +
#'   color_largest_group(df, "group", "value")
#'


color_largest_group <- function(df,
                                group_col,
                                num_col,
                                color1 = 'steelblue',
                                color2 = 'gray') {
  # Identify the group with the largest value in num_col
  # group_col <- enquo(group_col)
  # num_col <- enquo(num_col)
  group_col <-as.name(group_col)
  num_col <- as.name(num_col)

  largest_group <- df %>%
    filter((!!num_col) == max((!!num_col))) %>%
    pull(!!group_col)

  # Create a named vector of colors
  group_levels <- unique(df[[group_col]])
  colors <- setNames(rep(color2, length(group_levels)), group_levels)
  colors[largest_group] <- color1

  # Return the custom color scale
  return(scale_fill_manual(values = colors))
}


