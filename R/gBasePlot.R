#' @title Base plot.
#' @description Base function for ggplot.
#' @param ... List of additional parameters:\cr
#' - title Character. Main title of the plot.\cr
#' - facet Character: "" (standard ggplot), "grid" (matrix of panels) or "wrap" (wrap panel)\cr
#' - rows Character: if facet = "grid", rows of the matrix of panels\cr
#' - cols Character: if facet = "grid", columns of the matrix of panels\cr
#' - wrap Character: if facet = "wrap", name of the wraping variable\cr
#' - labX Character: X lab title\cr
#' - labY Character: Y lab title\cr
#' - grid Logical: if TRUE, display grid\cr
#' - labels Character\cr
#' - colors Character\cr
#' - scales Character\cr
#' - vnames Friendly names of the variable.
#' @return An object of class ggplot
#' @importFrom grDevices hcl
#' @importFrom ggplot2 labeller facet_wrap guide_legend element_blank scale_color_manual element_text element_line ggplot theme_bw theme ggtitle labs
#' @importFrom stats reformulate
#' @importFrom stringr str_pad
#' @keywords internal
#gBasePlot=function(title="", facet="grid", rows=".", cols=".", wrap=NULL, labX = NULL, labY=NULL, grid=FALSE, labels=NULL, colors=NULL,scales="fixed",vnames=NULL) {
gBasePlot=function(...) {

  parameters=list(...)


  if (is.null(parameters$fontSizeCex)) { fontSizeCex = 1 } else { fontSizeCex = parameters$fontSizeCex }
  if (is.null(parameters$title)) { title = "" } else { title = parameters$title }
  if (is.null(parameters$facet)) { facet = "grid" } else { facet = parameters$facet }
  if (is.null(parameters$rows)) { rows = "." } else { rows = parameters$rows }
  if (is.null(parameters$cols)) { cols = "." } else { cols = parameters$cols }
  if (is.null(parameters$wrap)) { wrap = NULL } else { wrap = parameters$wrap }
  if (is.null(parameters$labX)) { labX = NULL } else { labX = parameters$labX }
  if (is.null(parameters$labY)) { labY = NULL } else { labY = parameters$labY }
  if (is.null(parameters$grid)) { grid = FALSE } else { grid = parameters$grid }
  if (is.null(parameters$labels)) { labels = NULL } else { labels = parameters$labels }
  if (is.null(parameters$colors)) { colors = NULL } else { colors = parameters$colors }
  if (is.null(parameters$vnames)) { vnames = NULL } else { vnames = parameters$vnames }
  if (is.null(parameters$scales)) { scales = "fixed" } else { scales = parameters$scales }
  if (is.null(parameters$legendDirection)) { legendDirection = 'horizontal' } else { legendDirection = parameters$legendDirection }
  if (is.null(parameters$legendPosition)) { legendPosition = 'bottom' } else { legendPosition = parameters$legendPosition }
  if (is.null(parameters$labeller)) { labeller = NULL } else { labeller = parameters$labeller }

  baseSize=11*fontSizeCex

  # Elements ggplot communs à tous les graphiques

  # Couleurs par défaut
  ggColor <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }

  # Affichage de la grille
  panelGridMinor =  element_blank()
  panelGridMajor =  element_blank()
  if (grid==TRUE) {
    panelGridMajor =element_line(size=0.75)
  }

  bPlot=ggplot()  +
    theme_bw(base_size = baseSize) +
    theme(legend.direction = legendDirection, legend.position=legendPosition, legend.title=element_blank(),panel.grid.major = panelGridMajor, panel.grid.minor = panelGridMinor, plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5), strip.text.y = element_text(angle=180)) +
    guides(fill = guide_legend(nrow = 1)) +
    ggtitle(title) +
    labs(x = labX, y=labY)

  if (facet=="wrap")   {
    if (is.null(vnames)==FALSE) {
      bPlot = bPlot +
        facet_wrap(facets = as.formula(paste("~", wrap)), scales=scales, labeller = labeller(variable=vnames))
    } else {
      bPlot = bPlot + facet_wrap(facets = as.formula(paste("~", wrap)), scales=scales)
    }
  }
  if (facet=="grid")   {

    if (is.null(vnames)==FALSE)     {
      bPlot = bPlot +
        facet_grid(reformulate(cols,rows), switch = "y", scales=scales, labeller = labeller(variable=vnames))
    } else {
      bPlot = bPlot +
        facet_grid(reformulate(cols,rows), switch = "y", scales=scales)
    }
  }

  if (is.null(labels)==FALSE) {

    # Mets les labels à la même longueur
    labels=str_pad(labels, max(nchar(labels)), 'right')

    if (is.null(colors)) {
      colors=ggColor(length(labels))
    }

    bPlot = bPlot +
      scale_color_manual(labels = labels, values=colors)

  }

  return (bPlot)
}