###############################################################################
# Functions used in ISA
###############################################################################

# Required pachages
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Create a dataframe with sd, median, mean age for each gene for each age bin
# Input: Dataframe with gene expression data, age bins
create_EB_df <- function(df){
  sd_df <- data.frame()
  for (gene in colnames(df[,-c(ncol(df),ncol(df)-1)])){
    for (age_bin in unique(df$AgeBin)){
      sd_df <- rbind(sd_df, data.frame(
        Gene = gene,
        AgeBin = age_bin,
        MeanAge = mean(as.numeric(unlist(regmatches(age_bin, 
                                                    gregexpr("[0-9]+", 
                                                             age_bin))))),
        Median = median(df[df$AgeBin == age_bin, gene],
                        na.rm = TRUE),
        SD = sd(df[df$AgeBin == age_bin, gene],
                na.rm = TRUE)
      ))
    }
  }
  return(sd_df)
}

# Create ggplot object given a tmt. dataframe, an EB dataframe, and a list
# of genes
# Input: TMT dataframe, EB dataframe, list of genes
create_age_EB_plot <- function(tmt_df, EB_df, genes, alfa = 0.5,
                               scatter = TRUE,
                               line = TRUE, 
                               EB = TRUE,
                               point = TRUE) {
  # Create an empty ggplot object
  p <- ggplot()
  
  for (gene in genes) {
    # Create a temporary dataframe with the gene label
    tmt_df_gene <- tmt_df %>% mutate(Gene = gene)
    EB_df_gene <- EB_df[EB_df$Gene == gene, ] %>% mutate(Gene = gene)
    
    if (scatter == TRUE) {
      p <- p + geom_point(data = tmt_df_gene, aes(x = Age, 
                                                  y = .data[[gene]], 
                                                  color = Gene),
                          size = 1, 
                          alpha = alfa)
    }
    
    if (point == TRUE) {
      p <- p + stat_summary_bin(data = tmt_df_gene, aes(x = Age, 
                                                        y = .data[[gene]], 
                                                        color = Gene),
                                fun = "median", 
                                geom = "point", 
                                binwidth = 10,
                                size = 1.5)
    }
    
    if (line == TRUE) {
      p <- p + stat_summary_bin(data = tmt_df_gene, aes(x = Age, 
                                                        y = .data[[gene]], 
                                                        color = Gene),
                                fun = "median", 
                                geom = "line", 
                                binwidth = 10)
    }
    
    if (EB == TRUE) {
      p <- p + geom_errorbar(data = EB_df_gene, aes(x = MeanAge, 
                                                    ymin = Median - SD, 
                                                    ymax = Median + SD, 
                                                    color = Gene),
                             width = 2)
    }
  }
  
  p <- p + scale_color_brewer(palette = "Set1") +
    labs(x = "Age", 
         y = "Log Ratio")
  
  return(p)
}

# Create rolling plot (mean of i - h to i + h) for a list of genes
# Input: TMT dataframe, list of genes, h (window size)
# Options: return_df (return a dataframe), create_plot (create a plot)
create_incremental_plot <- function(tmt_df, 
                                    genes, 
                                    h = 2,
                                    return_df = FALSE,
                                    create_plot = TRUE) {
  # Error handling
  if (return_df == TRUE & create_plot == TRUE) {
    stop("You can only return a dataframe or create a plot, not both")
  }
  
  # Order tmt data based on age (low to high)
  tmt_df <- tmt_df[order(tmt_df$Age),]
  
  # Create an empty dataframe
  res_df <- data.frame(matrix(ncol = length(genes) + 1, 
                              nrow = nrow(tmt_df) - 2 * h))
  colnames(res_df) <- c("Age", genes)
  
  # Fill df with the incremental values
  for (i in (1 + h):(nrow(tmt_df) - h)){
    res_df$Age[i - h] <- mean(tmt_df$Age[(i - h):(i + h)])
  }
  
  for (gene in genes) {
    for (i in (1 + h):(nrow(tmt_df) - h)){
      res_df[i - h, gene] <- mean(tmt_df[[gene]][(i - h):(i + h)])
    }
  }
  
  if (return_df == TRUE) {
    return(res_df)
  } 
  
  if (create_plot == TRUE) {
    # Reshape data to long format (so ggplot can correctly assign colors)
    res_df_long <- res_df %>%
      pivot_longer(cols = -Age, names_to = "Gene", values_to = "Value")
    
    # Create plot
    p <- ggplot(res_df_long, aes(x = Age, y = Value, color = Gene)) + 
      geom_point(alpha = 0.6) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Age", y = "Log Ratio")
    
    return(p)
  }
}

# Create a geom hex plot for a single gene
# Input: TMT dataframe, gene
create_hex_plot <- function(tmt_df, 
                            gene, 
                            n_bins = 30) {
  p <- ggplot(tmt_df, aes(x = Age, y = .data[[gene]])) + 
    geom_hex(bins = n_bins) +
    scale_fill_viridis_c() +
    labs(x = "Age", y = "Log Ratio")
  
  return(p)
}
