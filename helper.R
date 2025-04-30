###############################################################################
# Functions used in ISA
###############################################################################

# Required pachages
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(hexbin)

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
                                    create_plot = TRUE,
                                    method = "mean") {
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
  
  if (method == "Mean") {
    for (gene in genes) {
      for (i in (1 + h):(nrow(tmt_df) - h)){
        res_df[i - h, gene] <- mean(tmt_df[[gene]][(i - h):(i + h)])
      }
    }
  } else if (method == "Median") {
    for (gene in genes) {
      for (i in (1 + h):(nrow(tmt_df) - h)){
        res_df[i - h, gene] <- median(tmt_df[[gene]][(i - h):(i + h)],
                                      na.rm = TRUE)
      }
    }
  } else if (method == "Sum"){
    for (gene in genes) {
      for (i in (1 + h):(nrow(tmt_df) - h)){
        res_df[i - h, gene] <- sum(tmt_df[[gene]][(i - h):(i + h)],
                                   na.rm = TRUE)
      }
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

# Create a scatter plot PDC000234 H3.3 and H3.1 Peptides
create_peptide_scatter <- function(){
  # Create a scatter plot with linear regression lines for each protein
   p <- ggplot(tmt.PDC000234_peptides, aes(x = Age, y = Log2Ratio, color = Protein)) +
    geom_point(alpha = 0.5) +
    labs(title = "Log2 Ratio vs Age for Normal Tissue",
         x = "Age",
         y = "Log2 Ratio") +
    theme(legend.title = element_blank()) +
    stat_summary_bin(fun = "median", 
                     geom = "line", 
                     binwidth = 10) +
    stat_summary_bin(fun = "median", 
                     geom = "point", 
                     binwidth = 10,
                     size = 1.5) +
    scale_color_brewer(palette = "Set1")
  
  return(p)
}

# Create a rolling plot for H3.3 and H3.1 Peptides from PDC000234
create_peptides_rolling <- function(func = "Mean"){
  # Load the data
  normal_tissue_long <- tmt.PDC000234_peptides
  
  # Create rolling plot (mean of i - h to i + h) with h = 5
  histone33 <- normal_tissue_long[normal_tissue_long$Protein == "H3.3", ] 
  histone33 <- histone33[complete.cases(histone33$Log2Ratio), ]
  histone33 <- histone33[order(histone33$Age), ]
  
  histone31 <- normal_tissue_long[normal_tissue_long$Protein == "H3.1", ]
  histone31 <- histone31[complete.cases(histone31$Log2Ratio), ]
  histone31 <- histone31[order(histone31$Age), ]
  
  # Create an empty dataframe
  H33_df <- data.frame(matrix(ncol = 2, nrow = nrow(histone33) - 10))
  colnames(H33_df) <- c("Age", "H3.3")
  
  for (i in 6:(nrow(histone33) - 5)) {
    # Calculate the mean of the Log2Ratio for the current window
    H33_df$H3.3_mean[i - 5] <- mean(histone33$Log2Ratio[(i - 5):(i + 5)], 
                                    na.rm = TRUE)
    H33_df$H3.3_median[i - 5] <- median(histone33$Log2Ratio[(i - 5):(i + 5)], 
                                        na.rm = TRUE)
    H33_df$Age[i - 5] <- mean(histone33$Age[(i - 5):(i + 5)], na.rm = TRUE)
  }
  
  # Create an empty dataframe
  H31_df <- data.frame(matrix(ncol = 2, nrow = nrow(histone31) - 10))
  colnames(H31_df) <- c("Age", "H3.1")
  
  for (i in 6:(nrow(histone31) - 5)) {
    # Calculate the mean of the Log2Ratio for the current window
    H31_df$H3.1_mean[i - 5] <- mean(histone31$Log2Ratio[(i - 5):(i + 5)], 
                                    na.rm = TRUE)
    H31_df$H3.1_median[i - 5] <- median(histone31$Log2Ratio[(i - 5):(i + 5)], 
                                        na.rm = TRUE)
    H31_df$Age[i - 5] <- mean(histone31$Age[(i - 5):(i + 5)], na.rm = TRUE)
  }
  
  # Setting -Inf values to NA
  H33_df[H33_df == -Inf] <- NA
  H31_df[H31_df == -Inf] <- NA

  if (func == "Mean"){
    # Create a scatter plot with linear regression lines for each protein (mean)
    p <- ggplot() + 
      geom_point(data = H33_df, aes(x = Age, y = H3.3_mean, color = "H3.3"), 
                 alpha = 0.5) +
      geom_point(data = H31_df, aes(x = Age, y = H3.1_mean, color = "H3.1"), 
                 alpha = 0.5) +
      labs(title = "Log2 Ratio vs Age for Normal Tissue (Mean)",
           x = "Age",
           y = "Log2 Ratio") +
      theme(legend.title = element_blank()) +
      scale_color_brewer(palette = "Set1")
  } else if (func == "Median"){
    # Create a scatter plot with linear regression lines for each protein (median)
    p <- ggplot() + 
      geom_point(data = H33_df, aes(x = Age, y = H3.3_median, color = "H3.3"), 
                 alpha = 0.5) +
      geom_point(data = H31_df, aes(x = Age, y = H3.1_median, color = "H3.1"), 
                 alpha = 0.5) +
      labs(title = "Log2 Ratio vs Age for Normal Tissue (Median)",
           x = "Age",
           y = "Log2 Ratio") +
      theme(legend.title = element_blank()) +
      scale_color_brewer(palette = "Set1") 
  } else {
    return(NULL)
  }
  return(p)
}
