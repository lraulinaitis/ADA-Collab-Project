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


usethis::use_package_doc()

#' @title Import example interaction matrix
#' @description Imports a matrix of plant-bird interactions that can be saved to the user environment

usethis::use_package("tibble")

example_data <- function(x) {

  # Function to shorten column names V2
  shorten_column_name <- function(long_name) {

    parts <- unlist(strsplit(long_name, "\\.")) # Split the long name by periods
    short_name <- paste0(toupper(substr(parts[-length(parts)],1,1)), collapse = "") # Extract the first character of each part except the last one and concatenate them

    if (length(parts) > 1) { # Add the last part of the name if it exists
      short_name <- paste0(short_name, ".", parts[length(parts)])
    }
    return(short_name)
  }

  shorten_row_name <- function(long_name) {

    parts <- unlist(strsplit(long_name, " ")) # Split the long name by periods
    short_name <- paste0(toupper(substr(parts[-length(parts)],1,1)), collapse = "") # Extract the first character of each part except the last one and concatenate them

    if (length(parts) > 1) { # Add the last part of the name if it exists
      short_name <- paste0(short_name, ".", parts[length(parts)])
    }
    return(short_name)
  }

  d <- read.csv("https://raw.githubusercontent.com/lraulinaitis/ADA-Collab-Project/main/hummingbirdmatrix.csv", header = T) |>
    tibble::column_to_rownames("Column1")
  rownames(d) <- sapply(rownames(d), shorten_row_name)
  colnames(d) <- sapply(colnames(d), shorten_column_name)
  return(d)
}

d <- example_data()

#' @title Basic visualization & analysis network-level interaction webs
#' @description Calculates five major network indices and visualizes network interactions
#' @param d species interaction matrix data set
#' @param alternative network metric to produce, defaults to "all"
#' @param plot produce network visualization, defaults to TRUE
#' @param weighted includes interaction strength in network metrics, defaults to FALSE
#' @keywords networks
#' @export
#' @examples
#' network(d = NULL, alternative = "all", plot = TRUE, weighted = FALSE)

usethis::use_package("bipartite")

network <- function(d, alternative = "all", plot = TRUE, weighted = FALSE) {

  # 2. produce visualization

  #plotweb(sortweb(d), method = "normal")
  #visweb(as.matrix(d), labsize = 0.5)

  bipartite::plotweb(sortweb(d),
          method = "normal",
          labsize = .75,
          text.rot = 90,
          y.lim = c(-3, 4),
          col.interaction = "black",
          col.high = "maroon",
          col.low = "darkgreen")


  bipartite::visweb(as.matrix(d),
         preynames = F,
         labsize = 2)

  # 3. text strings with definitions

  conn_txt <- c("Connectance: percent of the possible interactions (node-to-node links) that actually occur in the network.
  Calculated as the total number of non-zero matrix cells divided by the total matrix cells.")

  nest_txt <- c("Nestedness: a measure of the two-way interaction between specialist of level A with generalists of level B (or vice versa).", '\n',
  "High nestedness indicates that level A specialists interact primarily with level B generalists (or v.v.).", '\n',
  "This indicates a non-random network structure.", '\n',
  "Low nestedness indicates that level A specialists interact randomly with level B indiscriminately (or v.v.).", '\n',
  "This indicates a random network structure.")
  # need to determine scale of this metric...

  robust_txt <- c("Robustness: vulnerability of the network level to secondary extinction events when one species is removed from the network. Calculated as the integral of the network curve.", '\n',
                  "HL: higher trophic level", '\n', "LL: lower trophic level", '\n',
                  "0 = Network is completely vulnerable to extinction events", '\n',
                  "1 = Network is completely resilient to extinction events")

  spec_txt <- c("Network-Level Specialization (H2'): The extent of interaction specialization within the network. Specialists are those species that interact with one or very few other species.", '\n',
                     "0 = Network is absent of specialists", '\n',
                     "1 = Network is entirely composed of specialists")

  # 4. produce network metrics

  #if (weighted == TRUE) {
    # not sure I'm actually going to follow this thru...

  if (alternative == "all") {

    conn <- bipartite::networklevel(d, index = "connectance")
    cat(conn_txt)
    cat("Of all possible node-to-node connections,", (conn*100), "% are actually connected by observed interactions in this network.")

    robust <- bipartite::networklevel(d, index = "robustness")
    cat(robust_txt)
    cat("The higher trophic level (HL) in this matrix has a robustness index of", robust[1], '\n', "The lower trophic level's (LL) robustness index is ", robust[2])

    spec <- bipartite::networklevel(d, index = "H2")
    cat(spec_txt)
    cat("The interactions of this network have a specialization index of", spec[1])

    nest <- bipartite::networklevel(d, index = "nestedness") # doesn't work yet
    cat(nest_txt)
    cat("The interactions of this network have a nestedness index of", nest[1])

  }


  if (alternative == "connectedness") {
    conn <- bipartite::networklevel(d, index = "connectance")
    cat(conn_txt)
    cat("Of all possible node-to-node connections, ", (conn*100), "% are actually connected by observed interactions in this network.")
  }

  if (alternative == "nestedness") {
    nested <- bipartite::networklevel(d, index = "NODF") # doesn't work yet
    cat(nest_txt)
    # output here...
  }

  if (alternative == "robustness") {
    robust <- bipartite::networklevel(d, index = "robustness")
    cat(robust_txt)
    cat("The higher trophic level (HL) in this matrix has a robustness index of ", robust[1], '\n', "The lower trophic level's (LL) robustness index is ", robust[2])
  }

  if (alternative == "specialization") {
    spec <- bipartite::networklevel(d, index = "H2")
    cat(spec_txt)
    cat("The interactions of this network have a specialization index of ", spec[1])
  }

  if (alternative == "modularity") {
    spec <- bipartite::networklevel(d, index = "H2")
    cat(spec_txt)
    cat("The interactions of this network have a specialization index of ", spec[1])
  }

}

# 2nd function ----

#' @title Generate & visualize an equivalent null model for your interaction matrix
#' @description Generates, analyzes, and visualizes a random (null-model) matrix based on the number of higher- and lower-level species and the average number of node-to-node links in the original matrix.
#' @param d species interaction matrix data set
#' @param alternative network metric to produce, defaults to "all"
#' @param plot produce network visualization, defaults to TRUE
#' @param weighted includes interaction strength in network metrics, defaults to FALSE
#' @keywords networks
#' @export
#' @examples
#' network(d = NULL, alternative = "all", plot = TRUE)

null_network <- function(d, alternative = "all", plot = TRUE) {

  # 1. generate null model
  n_LL <- length(d)
  n_HL <- nrow(d)

  # calculate mean number of links
  d$link_ave <- rowSums(d) / n_HL
  total_linkave <- round(mean(d$link_ave))
  d <- d |> dplyr::mutate(link_ave = NULL)

  d_null <- bipartite::genweb(N1 = n_LL, N2 = n_HL, dens = total_linkave)

  bipartite::plotweb(bipartite::sortweb(d_null),
          method = "normal",
          labsize = .75,
          text.rot = 90,
          y.lim = c(-3, 4),
          col.interaction = "black",
          col.high = "maroon",
          col.low = "darkgreen")

  if (alternative == "all") {

    matrix <- matrix(NA, nrow = 2, ncol = 0)
    rownames(matrix) <- c("Observed Matrix", "Null Matrix")

    conn <- bipartite::networklevel(d, index = "connectance")
    conn_null <- bipartite::networklevel(d_null, index = "connectance")

    robust <- bipartite::networklevel(d, index = "robustness")
    robust_null <- bipartite::networklevel(d_null, index = "robustness")

    spec <- bipartite::networklevel(d, index = "H2")
    spec_null <- bipartite::networklevel(d_null, index = "H2")

    nest <- bipartite::networklevel(d, index = "nestedness") # doesn't work yet
    nest_null <- bipartite::networklevel(d_null, index = "nestedness") # doesn't work yet

    Connectance <- c(conn, conn_null)
    Robustness <- c(robust, robust_null)
    Specialization <- c(spec, spec_null)
    Nestedness <- c(nest, nest_null)


    matrix <- cbind(matrix, Connectance, Robustness, Nestedness, Specialization)

    broom::tidy(matrix)

  }

}
null_network(d)
