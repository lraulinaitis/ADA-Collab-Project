

#' @name example_data
#' @title Import example interaction matrix
#' @description Imports a matrix of plant-bird interactions that can be saved to the user environment
#' @param abbrev_names abbreviates names for clearer display in network plots, defaults to FALSE

usethis::use_package("tibble")

example_data <- function(x, abbrev_names = FALSE) {

  shorten_column_name <- function(long_name) {
    parts <- unlist(strsplit(long_name, "\\."))
    short_name <- paste0(toupper(substr(parts[-length(parts)],1,1)), collapse = "")

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

  if (abbrev_names == TRUE) {
    rownames(d) <- sapply(rownames(d), shorten_row_name)
    colnames(d) <- sapply(colnames(d), shorten_column_name)
  }

  return(d)
}

#' @name network
#' @title Basic visualization & network-level analysis of interaction webs
#' @description Calculates five major network indices and visualizes network interactions.
#' @param d species interaction matrix data set
#' @param alternative network metric to produce, defaults to "all"
#' @param plot produce network visualization, defaults to TRUE
#' @keywords networks
#' @export
#' @examples
#' network(d = NULL, alternative = "all", plot = TRUE)

usethis::use_package("bipartite")

network <- function(d, alternative = "all", plot = TRUE) {

  if (plot == TRUE) {
  bipartite::plotweb(bipartite::sortweb(d),
          method = "normal",
          labsize = .75,
          text.rot = 90,
          y.lim = c(-3, 4),
          col.interaction = "black",
          col.high = "maroon",
          col.low = "darkgreen")
  }

  if (alternative == "all") {

    conn <- bipartite::networklevel(d, index = "connectance")
    cat("Of all possible node-to-node connections,", (conn*100), "% are actually connected by observed interactions in this network.", '\n')

    robust <- bipartite::networklevel(d, index = "robustness")
    cat("The higher trophic level (HL) in this matrix has a robustness index of", robust[1], '\n', "The lower trophic level's (LL) robustness index is ", robust[2], '\n')

    spec <- bipartite::networklevel(d, index = "H2")
    cat("The interactions of this network have a specialization index of", spec[1], '\n')

    nest <- bipartite::networklevel(d, index = "NODF")
    cat("The interactions of this network have a nestedness index of", nest[1], '\n')

    mod <- bipartite::networklevel(d, index = "modularity")
    cat("The interactions of this network have a modularity index of", mod[1])
  }

  if (alternative == "connectance") {
    conn <- bipartite::networklevel(d, index = "connectance")
    cat("Of all possible node-to-node connections, ", (conn*100), "% are actually connected by observed interactions in this network.")
  }

  if (alternative == "nestedness") {
    nested <- bipartite::networklevel(d, index = "NODF") # doesn't work yet
    cat("The interactions of this network have a nestedness index of", nest[1])
  }

  if (alternative == "robustness") {
    robust <- bipartite::networklevel(d, index = "robustness")
    cat("The higher trophic level (HL) in this matrix has a robustness index of", robust[1], '\n', "The lower trophic level's (LL) robustness index is", robust[2])
  }

  if (alternative == "specialization") {
    spec <- bipartite::networklevel(d, index = "H2")
    cat("The interactions of this network have a specialization index of", spec[1])
  }

  if (alternative == "modularity") {
    mod <- bipartite::networklevel(d, index = "modularity")
    cat("The interactions of this network have a modularity index of", mod[1])
  }

}

#' @name null_network
#' @title Generate & visualize an equivalent null model for your interaction matrix
#' @description Generates, analyzes, and visualizes a random (null-model) matrix based on the number of higher- and lower-level species and the average number of node-to-node links in the original matrix.
#' @param d species interaction matrix data set
#' @param alternative network metric to produce, defaults to "all"
#' @param plot produce network visualization, defaults to TRUE
#' @keywords networks
#' @examples
#' null_network(d = NULL, alternative = "all", plot = TRUE)

null_network <- function(d, alternative = "all", plot = TRUE) {

  n_LL <- length(d)
  n_HL <- nrow(d)

  d$link_ave <- rowSums(d) / n_HL
  total_linkave <- round(mean(d$link_ave))
  d <- d |> dplyr::mutate(link_ave = NULL)

  d_null <- bipartite::genweb(N1 = n_HL, N2 = n_LL, dens = total_linkave)
  d_null <- as.data.frame(d_null)

  if (plot == TRUE) {
    bipartite::plotweb(bipartite::sortweb(d_null),
            method = "normal",
            labsize = .75,
            text.rot = 90,
            y.lim = c(-3, 4),
            col.interaction = "black",
            col.high = "maroon",
            col.low = "darkgreen")
  }

  if (alternative == "all") {

    matrix <- matrix(NA, nrow = 2, ncol = 0)
    rownames(matrix) <- c("Observed Matrix", "Null Matrix")

    conn <- bipartite::networklevel(d, index = "connectance")
    conn_null <- bipartite::networklevel(d_null, index = "connectance")

    robust <- bipartite::networklevel(d, index = "robustness")
    robust_null <- bipartite::networklevel(d_null, index = "robustness")

    robust.HL <- robust[1]
    robust_null.HL <- robust_null[1]
    robust.LL <- robust[2]
    robust_null.LL <- robust_null[2]

    spec <- bipartite::networklevel(d, index = "H2")
    spec_null <- bipartite::networklevel(d_null, index = "H2")

    nest <- bipartite::networklevel(d, index = "NODF")
    nest_null <- bipartite::networklevel(d_null, index = "NODF")

    mod <- bipartite::networklevel(d, index = "modularity")
    mod_null <- bipartite::networklevel(d_null, index = "modularity")

    Connectance <- c(conn, conn_null)
    Robustness.HL<- c(robust.HL, robust_null.HL)
    Robustness.LL <- c(robust.LL, robust_null.LL)
    Specialization <- c(spec, spec_null)
    Nestedness <- c(nest, nest_null)
    Modularity <- c(mod, mod_null)

    matrix <- cbind(matrix, Connectance, Robustness.HL, Robustness.LL, Nestedness, Specialization, Modularity)
    print(matrix)
  }

  if (alternative == "connectance") {

    conn <- bipartite::networklevel(d, index = "connectance")
    conn_null <- bipartite::networklevel(d_null, index = "connectance")
    cat("Observed connectance:", (conn*100), '\n',
        "Null model connectance:", (conn_null*100))
}

  if (alternative == "robustness") {
    robust <- bipartite::networklevel(d, index = "robustness")
    robust_null <- bipartite::networklevel(d_null, index = "robustness")
    cat("Observed higher-trophic-level (HL) robustness:", robust[1], " | Obs. lower-level (LL) robustness:", robust[2], '\n',
        "Null model HL robustness:", robust_null[1], " | Null model LL robustness:", robust_null[2])
  }

  if (alternative == "specialization") {
    spec <- bipartite::networklevel(d, index = "H2")
    spec_null <- bipartite::networklevel(d_null, index = "H2")
    cat("Observed specialization:", spec[1], '\n',
        "Null model specialization:", spec_null[1])
  }

  if (alternative == "nestedness") {
    nest <- bipartite::networklevel(d, index = "NODF")
    nest_null <- bipartite::networklevel(d_null, index = "NODF")
    cat("Observed nestedness:", nest[1], '\n',
        "Null model nestedness:", nest_null[1])
  }

  if (alternative == "modularity") {
      mod <- bipartite::networklevel(d, index = "modularity")
      mod_null <- bipartite::networklevel(d_null, index = "modularity")
      cat("Observed modularity:", mod[1], '\n',
          "Null model modularity:", mod_null[1])
    }

}

