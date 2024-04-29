## ----setup--------------------------------------------------------------------
library(ADANetworks)
d<-example_data(abbrev_names=TRUE)

## ----full network, fig.width=10, fig.height=10--------------------------------
network(d)

## ----connectance--------------------------------------------------------------
network(d, alternative = "connectance", plot=FALSE)

## ----nestedness---------------------------------------------------------------
network(d, alternative = "nestedness", plot=FALSE)

## ----robustness---------------------------------------------------------------
network(d, alternative = "robustness", plot=FALSE)

## ----specialization-----------------------------------------------------------
network(d, alternative = "specialization", plot=FALSE)

## ----modularity---------------------------------------------------------------
network(d, alternative = "modularity", plot=FALSE)

## ----null, fig.width=10, fig.height=10----------------------------------------
null_network(d)

