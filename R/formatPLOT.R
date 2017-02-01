#' formatPLOT
#'
#' Just a constant with my default plot format
#' @keywords plot, ggplot2
#' @export
#' @examples
#' # Load packages
#' library(mahPackage)
#'
#' # Add example

# Constants
formatPLOT <-   theme_bw() +
  theme(plot.title = element_text(size=24, color="gray31")) +
  theme(legend.key = element_blank()) +
  theme(panel.border= element_blank()) +
  theme(axis.line = element_line(color = "grey")) +
  theme(axis.text = element_text(color="grey31")) +
  theme(axis.ticks = element_line(color="grey")) +
  theme(legend.text = element_text(color="gray31")) +
  theme(legend.title = element_text(color="gray31")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())
