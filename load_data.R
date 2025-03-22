#### Loading the data from plot_data folder ####
tmt.PDC000125 <- read.csv("plot_data/tmt.PDC000125.csv", row.names=1)
tmt.PDC000127 <- read.csv("plot_data/tmt.PDC000127.csv", row.names=1)
tmt.PDC000153 <- read.csv("plot_data/tmt.PDC000153.csv", row.names=1)
tmt.PDC000198 <- read.csv("plot_data/tmt.PDC000198.csv", row.names=1)
tmt.PDC000219 <- read.csv("plot_data/tmt.PDC000219.csv", row.names=1)
tmt.PDC000234 <- read.csv("plot_data/tmt.PDC000234.csv", row.names=1)
tmt.PDC000270 <- read.csv("plot_data/tmt.PDC000270.csv", row.names=1)

#### Define data set names ####
dataset_names <- c(
  "PDC000125 - Uterine" = "tmt.PDC000125",
  "PDC000127 - Renal" = "tmt.PDC000127",
  "PDC000153 - Lung" = "tmt.PDC000153",
  "PDC000198 - Hepatic" = "tmt.PDC000198",
  "PDC000219 - Lung" = "tmt.PDC000219",
  "PDC000234 - Lung" = "tmt.PDC000234",
  "PDC000270 - Pancreas" = "tmt.PDC000270"
)