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
#' @param weighted includes interaction strength in network metrics, defaults to FALSE
#' @keywords networks
#' @export
#' @examples
#' network(d = NULL, alternative = "all", plot = TRUE, weighted = FALSE)



usethis::use_package("bipartite")
usethis::use_package("tibble")


usethis::use_package_doc()



example_data <- function(x) {
  d <- read.csv("https://raw.githubusercontent.com/lraulinaitis/ADA-Collab-Project/main/EXAMPLE%20MATRIX_hummingbirdmatrix.csv?token=GHSAT0AAAAAACNJ5YXMLHROXXEAH4RZ6E26ZROQQTA")
}

#### TRASH CODE ####
# Function to shorten column names V2
shorten_column_name <- function(long_name) {
  # Split the long name by periods
  parts <- unlist(strsplit(long_name, "\\."))

  # Extract the first character of each part except the last one and concatenate them
  short_name <- paste0(toupper(substr(parts[-length(parts)],1,1)), collapse = "")

  # Add the last part of the name if it exists
  if (length(parts) > 1) {
    short_name <- paste0(short_name, ".", parts[length(parts)])
  }

  return(short_name)
}

shorten_row_name <- function(long_name) {
  # Split the long name by periods
  parts <- unlist(strsplit(long_name, " "))

  # Extract the first character of each part except the last one and concatenate them
  short_name <- paste0(toupper(substr(parts[-length(parts)],1,1)), collapse = "")

  # Add the last part of the name if it exists
  if (length(parts) > 1) {
    short_name <- paste0(short_name, ".", parts[length(parts)])
  }

  return(short_name)
}


# Shorten column names
d1<-d
rownames(d) <- sapply(rownames(d), shorten_row_name)
colnames(d) <- sapply(colnames(d), shorten_column_name)


#### ACTUAL CODE ####

network <- function(d, alternative = "all", plot = TRUE, weighted = FALSE) {

  # 1. read in matrix
  d <- read.csv("hummingbirdmatrix.csv", header = T) |>
    column_to_rownames("Column1")

  # 2. produce visualization

  #plotweb(sortweb(d), method = "normal")
  #visweb(as.matrix(d), labsize = 0.5)

  plotweb(sortweb(d),
          method = "normal",
          labsize = .75,
          text.rot = 90,
          y.lim = c(-3, 4),
          col.interaction = "black",
          col.high = "maroon",
          col.low = "darkgreen")


  visweb(as.matrix(d1),
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
    nested <- bipartite::networklevel(d, index = "weightedNODF") # doesn't work yet
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

}



