# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @title A demonstration of basic network analysis and visualization
#' @description This function allows you explore / quantify / visualize...
#' @param d species interaction matrix data set
#' @param alternative network metric to produce, defaults to "all".
#' @param plot produce network visualization, defaults to TRUE
#' @keywords power
#' @export
#' @examples
#' network(d = NULL, alternative = "all", plot = TRUE)


network <- function(d, plot = TRUE, ) {

}

load_matrix <- function(x) {
  hummingbird.comm <- read.csv("hummingbirdmatrix.csv", header = T)
}

create_matrix <- function(x) {
  uw.comm <- read.csv("medgarden.csv", header = T) %>%
    group_by(plant, pollinator) %>%
    summarise(count = sum(count)) %>%
    pivot_wider(names_from = "pollinator",
                values_from = "count",
                values_fill = 0) %>%
    column_to_rownames("plant")
}

create_network <- function(d, ){}

network_metrics <- function(d) {

  if (metric == "C") {
    conn <- bipartite::networklevel(d, index = "connectance")
    cat("Connectance: percent of the possible interactions (or node-to-node connections)
    that actually occur in the network."\n"Calculated by counting the total number
    of cells with non-zero values in the matrix and dividing by the total number of
    matrix cells.")

    cat("The connectance value for this network is ", conn)
  }

  if (metric == "C") {
  networklevel(hummingbird.comm, index = "weighted NODF")
  cat("`weightedNODF`: nestedness, or the extent to which more specialized species
  interact with a subset of more generalized species. A high nestedness value
  implies that there is a strong non-random structure to this network, while a
  low nestedness value implies that interactions are essentially randomly
  distributed. Because this is a weighted value, it accounts for the interaction
  strength (how many times does that interaction occur).")

  }

  if (metric == "C") {
    cat("`robustness`: robustness of the network to removal of species, calculated in
    terms of removal of higher trophic level (HL) and lower trophic level (LL)
    species. This is calculated as the area under the curve, which means that a
    higher robustness value means a more gradual negative slope (removal of one
    species does not generally cause too many secondary extinctions), and a low
    robustness value means a steep negative slope (removal of one species generally
    causes many secondary extinctions).")
    networklevel(hummingbird.comm, index = "robustness")

  }

  if (metric == "C") {
  cat("Specialization (H'): robustness of the network to removal of species, calculated in
    terms of removal of higher trophic level (HL) and lower trophic level (LL)
    species. This is calculated as the area under the curve, which means that a
    higher robustness value means a more gradual negative slope (removal of one
    species does not generally cause too many secondary extinctions), and a low
    robustness value means a steep negative slope (removal of one species generally
    causes many secondary extinctions).")



}

