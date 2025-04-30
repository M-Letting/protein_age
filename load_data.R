library(readr)

#### Loading the data from plot_data folder ####
#tmt.PDC000125 <- read.csv("plot_data/tmt.PDC000125.csv", row.names=1)
#tmt.PDC000127 <- read.csv("plot_data/tmt.PDC000127.csv", row.names=1)
#tmt.PDC000153 <- read.csv("plot_data/tmt.PDC000153.csv", row.names=1)
#tmt.PDC000198 <- read.csv("plot_data/tmt.PDC000198.csv", row.names=1)
#tmt.PDC000219 <- read.csv("plot_data/tmt.PDC000219.csv", row.names=1)
#tmt.PDC000234 <- read.csv("plot_data/tmt.PDC000234.csv", row.names=1)
#tmt.PDC000270 <- read.csv("plot_data/tmt.PDC000270.csv", row.names=1)

#### Loading the data from plot_data folder using readr ###
tmt.PDC000125 <- read_csv("plot_data/tmt.PDC000125.csv")
tmt.PDC000125 <- as.data.frame(tmt.PDC000125)
rownames(tmt.PDC000125) <- tmt.PDC000125$...1
tmt.PDC000125 <- tmt.PDC000125[,-1]

tmt.PDC000127 <- read_csv("plot_data/tmt.PDC000127.csv")
tmt.PDC000127 <- as.data.frame(tmt.PDC000127)
rownames(tmt.PDC000127) <- tmt.PDC000127$...1
tmt.PDC000127 <- tmt.PDC000127[,-1]

tmt.PDC000153 <- read_csv("plot_data/tmt.PDC000153.csv")
tmt.PDC000153 <- as.data.frame(tmt.PDC000153)
rownames(tmt.PDC000153) <- tmt.PDC000153$...1
tmt.PDC000153 <- tmt.PDC000153[,-1]

tmt.PDC000198 <- read_csv("plot_data/tmt.PDC000198.csv")
tmt.PDC000198 <- as.data.frame(tmt.PDC000198)
rownames(tmt.PDC000198) <- tmt.PDC000198$...1
tmt.PDC000198 <- tmt.PDC000198[,-1]

tmt.PDC000219 <- read_csv("plot_data/tmt.PDC000219.csv")
tmt.PDC000219 <- as.data.frame(tmt.PDC000219)
rownames(tmt.PDC000219) <- tmt.PDC000219$...1
tmt.PDC000219 <- tmt.PDC000219[,-1]

tmt.PDC000234 <- read_csv("plot_data/tmt.PDC000234.csv")
tmt.PDC000234 <- as.data.frame(tmt.PDC000234)
rownames(tmt.PDC000234) <- tmt.PDC000234$...1
tmt.PDC000234 <- tmt.PDC000234[,-1]

tmt.PDC000234_peptides <- read_csv("plot_data/tmt.PDC000234_peptides.csv")

tmt.PDC000270 <- read_csv("plot_data/tmt.PDC000270.csv")
tmt.PDC000270 <- as.data.frame(tmt.PDC000270)
rownames(tmt.PDC000270) <- tmt.PDC000270$...1
tmt.PDC000270 <- tmt.PDC000270[,-1]

#### Define data set names ####
dataset_names <- c(
  "PDC000125 - Uterine" = "tmt.PDC000125",
  "PDC000127 - Renal" = "tmt.PDC000127",
  "PDC000153 - Lung" = "tmt.PDC000153",
  "PDC000198 - Hepatic" = "tmt.PDC000198",
  "PDC000219 - Lung" = "tmt.PDC000219",
  "PDC000234 - Lung" = "tmt.PDC000234",
  "PDC000234 - Peptides (Only H3.1 + H3.3)" = "tmt.PDC000234_peptides",
  "PDC000270 - Pancreas" = "tmt.PDC000270"
)