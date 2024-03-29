## Plot Figure 3: age distribution of vaccinated cases per simulation set
plot_figure_vax_distrib <- function(list_output, data_anoun, age, regions){
  ## Compute the number of cases across all years in each simulation / strata (vaccine/ age/ region)
  list_output <- lapply(list_output, function(X) return(apply(X, c(1,2), sum)))
  
  ## First panel: number of double vaccinated as a function of number of single vaccinated
  ## cases in each simulation
  par(mfrow = c(1,1), mar = c(3, 4, 1, 0), oma = c(3,1,0,1), bty = "l")
  layout(mat = matrix(c(1,1,1,
                        2,3,4,
                        5,6,7), 
                      nrow = 3, ncol = 3, byrow = T))
  plot_first_sec(list_output, data_anoun, age, regions)
  title(ylab = "Number of\n double-vaccinated cases", line = 2.2)#, outer = T, line = 0)
  title(xlab = "Number of single-vaccinated cases", line = 2)#, outer = T, line = 0)
  title(main = "A", adj = 0)
  
  ### Number and proportion of single and double vaccinated cases per age group 
  ## Overall number 
  plot_vax(output = list_output, data = data_anoun, prop = F, legend = F)
  title(ylab = "Number of cases", outer = F, line = 3, cex.lab = 1)
  title(main = "Whole population")
  title(main = "B", adj = 0)
  
  ## By age group of interest
  # Cases 5 to 15
  age_interest_y <- c("[5,6)", "[6,10)", "[10,15)")
  # Find which values in age contain the group listed in age_interest and age_interest_y
  index_age_y <- paste0("age", which(age %in% age_interest_y))
  # Select only rows containing index_age and index_age_y in elements of list_output
  list_output_y <- lapply(
    list_output, function(X) 
      return(X[grep(paste(index_age_y,collapse="|"), rownames(X)),]))
  
  plot_vax(output = list_output_y, data = data_anoun[age_groups %in% age_interest_y], 
           prop = F, legend = F, ymax = 300)
  title(main = "5 to 15 year-old")
  title(main = "C", adj = 0)

  
  # Cases 15+
  age_interest <- c("[15,20)", "[20,30)", "[30,40)", "[40,100]")
  # Find which values in age contain the group listed in age_interest and age_interest_y
  index_age <- paste0("age", which(age %in% age_interest))
  # Select only rows containing index_age and index_age_y in elements of list_output
  list_output_age <- lapply(
    list_output, function(X) 
      return(X[grep(paste(index_age,collapse="|"), rownames(X)),]))
  
  
  ### Same as before, but with proportions
  plot_vax(output = list_output_age, data = data_anoun[age_groups %in% age_interest], 
           prop = F, legend = F, ymax = 300)
  title(main = "15 year-old and above")
  title(main = "D", adj = 0)
  
  plot_vax(output = list_output, data = data_anoun, prop = T, legend = F)
  title(ylab = "Percentage of total \nnumber of cases", outer = F, 
        line = 2.2, cex.lab = 1)
  title(main = "E", adj = 0)
  plot_vax(output = list_output_y, data = data_anoun[age_groups %in% age_interest_y], 
           prop = T, legend = F)
  title(main = "F", adj = 0)
  plot_vax(output = list_output_age, data = data_anoun[age_groups %in% age_interest], 
           prop = T, legend = F)
  title(xlab = "Number of doses", outer = T, line = 0, cex.lab = 1)
  title(main = "G", adj = 0)
  
}

## Plot Figure 4: distribution of vaccinated cases by year in each simulation set
plot_figure_vax_year <- function(list_output, data_anoun){
  # Plot proportion of single vaccinated cases each year
  par(mfrow = c(2, 1), mar = c(2, 4, 1, 0), oma = c(3,1,0,1), bty = "l")
  plot_year(output = list_output, data_anoun, prop = "IV1", legend = T, 
            ymax = 17)
  title(ylab = "% of single-vaccinated cases", outer = F, line = 2.5, cex.lab = 1)
  # Plot proportion of double vaccinated cases each year
  plot_year(output = list_output, data_anoun, prop = "IV2", legend = F, 
            ymax = 10)
  title(ylab = "% of double-vaccinated cases", outer = F, line = 2.5, cex.lab = 1)
  title(xlab = "year", outer = T, line = 1, cex.lab = 1)
  
}

## Plot supplement: impact of removing waning from the since_elimination scenario
plot_figure_nowane <- function(list_output, data_anoun){
  par(mfrow = c(1, 1), mar = c(2, 4, 1, 0), oma = c(3,1,0,1), bty = "l")
  plot_year(output = list_output_nowane, data_anoun, with_data = FALSE)
  title(ylab = "Number of cases", outer = F, line = 2.5, cex.lab = 1)
  title(xlab = "year", outer = T, line = 1, cex.lab = 1)
}

# Plot supplement: Distribution of cases by year, region and age group (all cases)
plot_figure_region_age <- function(list_output, data_anoun, age, regions){
  par(mfrow = c(1, 2), mar = c(3, 4, 1, 0), oma = c(4,1,0,0),
      las = 1, bty = "l", cex.axis = .8)
  
  ## Panel A: overall number of cases per year in each model
  layout(mat = matrix(c(1,1,2,3,4,5), nrow = 3, ncol = 2, byrow = T))
  plot_year(output = list_output, data_anoun)
  title(main = "A", adj = 0)
  title(ylab = "Number of cases", outer = F)
  
  # Compute overall number of cases in each element of list_output
  list_output <- lapply(list_output, function(X) return(apply(X, c(1,2), sum)))
  
  ## Panel B: Number of cases per age group in each scenario
  plot_age(output = list_output, data = data_anoun, age_group = age, prop = F,
           legend = F)
  title(main = "B", adj = 0)
  title(ylab = "Number of cases", outer = F)
  ## Panel C: Number of cases per region in each scenario
  plot_reg(output = list_output, data = data_anoun, regions = regions, prop = F,
           legend = F)
  title(main = "C", adj = 0)
  ## Panel D: Proportion of cases per age group in each scenario
  plot_age(output = list_output, data = data_anoun, age_group = age, prop = T,
           legend = F)
  title(main = "D", adj = 0)
  title(xlab = "Age group", outer = T, adj = .28, line = 2)
  title(ylab = "Percentage of \ntotal number of cases", outer = F, line = 2)#, adj = .12)
  ## Panel D: Proportion of cases per region in each scenario
  plot_reg(output = list_output, data = data_anoun, regions = regions, prop = T,
           legend = F)
  title(main = "E", adj = 0)
  title(xlab = "Region", outer = T, adj = .82, line = 2)
  
}

# Plot supplement: Parameter estimates in the model
plot_figure_parameters <- function(list_pmcmc_run, specs, burnin, thin, vax, sec, 
                                   distance){
  ## Import list containing all data
  all_data <- import_all_data(
    year_start = list_specs_run$year_start, N_year = list_specs_run$N_year,
    regions = list_specs_run$regions, age = list_specs_run$age, 
    scenario = list_specs_run$scenario, vax = vax)
  ## Number of inhabitants per age group
  N <- import_pop_data(list_specs_run$year_start)
  ## Compute 50 and 95% credible intervals for each parameter
  list_quant <- lapply(list_pmcmc_run, function(X){
    ## Extract samples: remove the burnin phase
    samples_x <- X$pars[-c(1:burnin),]
    ## Thinning: remove a proportion of samples (based on thin)
    iter <- seq(1, nrow(samples_x), thin)
    samples_x <- (samples_x[iter,])
    
    ## Compute r0 and add r0 to samples_x as a new column
    r0_x <- compute_r0(samples = samples_x, N, distance, all_data)
    samples_x <- cbind(samples_x, "r0" = r0_x)
    ## Compute median, 50% and 95% credible intervals
    quant_x <- apply(samples_x, 2, function(X) 
      quantile(X, c(0.025, 0.25, 0.5, 0.75, 0.975)))
    
    ## Rename vacc into v_protect
    colnames(quant_x)[colnames(quant_x) == "vacc"] <- "v_protect"
    ## Add v_leak = 0 to model without waning
    if(!any(colnames(quant_x) == "v_leak")) {
      quant_x <- cbind(quant_x, v_leak = 0) 
    } else quant_x[, "v_leak"] <- quant_x[, "v_leak"] * 100
    quant_x[, "delta"] <- 1/quant_x[, "delta"] 
    
    return(quant_x)
  })
  if(length(list_quant) == 2) col <- c("#d73027", "#bf812d") else
    col <- c("#4575b4", "#d73027", "#bf812d")
  
  ## Plot proportion of recovered at t=0 per age group
  n_recov <- sum(grepl("recov", colnames(list_quant[[1]])))
  if (!sec){
    mat_lay <- matrix(c(1, 1, 1, 2, 2, 2,
                        3, 3, 4, 4, 5, 5,
                        6, 6, 7, 7, 8, 8), 
                      nrow = 3, ncol = 6, byrow = T)
  } else {
    mat_lay <- matrix(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 
                        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5,
                        6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9), 
                      nrow = 3, ncol = 12, byrow = T)
  }
  
  par(mfrow = c(1,1), mar = c(3, 5, 1, 0), oma = c(1,1,0,1), bty = "l")
  layout(mat = mat_lay)
  
  plot_column_sample(list_quant = list_quant, name = "recov", legend = T, col = col)
  axis(side = 1, at = seq_len(n_recov), las = 1,
       labels = c("[10-15)", "[15-20)", "[20-30)", "[30-40)", "[40-100]")
  )
  title(ylab = "Proportion of susceptible\n individuals recovered in 2010", outer = F, line = 3)
  title(xlab = "Age group", outer = F, line = 2)
  title(main = "A", adj = 0.05)
  
  
  
  ## Plot proportion of unvaccinated vaccinated during catchup campaigns
  plot_column_sample(list_quant, "catchup", ymax = 1, col = col)
  axis(side = 1, at = c(1,2), las = 1, labels = c("1996 (20-30 yo)", "2008 (5-10yo)"))
  title(ylab = "Proportion of unvaccinated\n individuals vaccinated in \ncatch-up campaigns", 
        outer = F, line = 2.2)
  title(xlab = "Catchup campaign and target age group", outer = F, line = 2)
  title(main = "B", adj = 0.05)
  
  ## Plot R0
  plot_column_sample(list_quant, "r0", ymin = if(vax == "cprd") 15 else 9, 
                     ymax = if(vax == "cprd") 18 else 12, col = col)
  title(ylab = "R0", outer = F, line = 3)
  title(main = "C", adj = 0.05)
  
  ## Plot delta
  plot_column_sample(list_quant, "delta", ymin = if(vax == "cprd") 170 else 90,
                     ymax = if(vax == "cprd") 185 else 120, col = col)
  title(ylab = "Maternal immunity (days)", outer = F, line = 3)
  title(main = "D", adj = 0.05)
  
  ## Plot report_import
  plot_column_sample(list_quant, "report_import", ymin = 0.45, ymax = 0.65, col = col)
  title(ylab = "Proportion of imports reported", outer = F, line = 3)
  title(main = "E", adj = 0.05)
  
  
  ## Plot vax
  plot_column_sample(list_quant, "v_fail", ymax = .07, col = col)
  title(ylab = "Primary vaccine failure", outer = F, line = 3)
  title(main = "F", adj = 0.05)
  plot_column_sample(list_quant, "v_protect", ymax = 1, col = col)
  title(ylab = "Risk of onward transmission \nfrom vaccinated cases", outer = F, line = 3)
  title(main = "G", adj = 0.05)
  plot_column_sample(list_quant, "v_leak", ymax = .08, col = col)
  title(ylab = "Waning of protection \n(% per year)", outer = F, line = 3)
  title(main = "H", adj = 0.05)
  
  
  if(sec){
    plot_column_sample(list_quant, "v_sec", ymax = .01)
    title(ylab = "Baseline risk of\n secondary vaccine failure", 
          outer = F, line = 3)
    title(main = "I", adj = 0.05)
  }
  
}

# Plot supplement: Plot importation and transmission seasonality
plot_figure_season <- function(list_pmcmc_run){
  ## Extract samples from list_pmcmc_run, after burnin and thinning
  list_samples <- lapply(list_pmcmc_run, function(X){
    samples_x <- X$pars[-c(1:burnin),]
    iter <- seq(1, nrow(samples_x), thin)
    samples_x <- (samples_x[iter,])
    return(samples_x)
  })
  
  col <- c("#4575b4", "#d73027", "#bf812d")
  ## Plot transmission seasonality
  par(mfrow = c(2, 1), mar = c(2, 4, 1, 0), oma = c(3,1,0,1), bty = "l")
  plot_seasonality(list_samples, import = F)
  legend("top", fill = col, legend = names(list_samples), border = NA, bty = "n")
  abline(h = 100, lty = 2)
  title(main = "A", adj = 0.05)
  ## Plot importation seasonality
  plot_seasonality(list_samples, import = T)
  title(main = "B", adj = 0.05)
  abline(h = 100, lty = 2)
  title(ylab = "Percentage of reference level", outer = T, line = -2)
  title(xlab = "Time (Days)", outer = T, line = -1)
}

# Plot supplement: Plot posterior distribution
plot_posterior <- function(list_pmcmc_run, burnin, thin,
                           col = c("#4575b4", "#d73027", "#bf812d")){
  ## Extract posterior from list_pmcmc_run, after burnin and thinning
  list_post <- lapply(list_pmcmc_run, function(X){
    post_x <- X$probabilities[-c(1:burnin),3]
    iter <- seq(1, length(post_x), thin)
    post_x <- (post_x[iter])
    return(post_x)
  })
  
  ## Plot the density of each element in list_post
  par(mfrow = c(1, 1), mar = c(2, 4, 1, 0), oma = c(3,1,0,1), bty = "l")
  list_post[[1]] %>% density %>% 
    plot(main = "Density of posterior distribution per model", 
         type = "h", xlim = c(-36200, -35950), col = col[1])
  if(length(list_post) > 1){
    for(i in seq(2, length(list_post))){
      list_post[[i]] %>% density %>% lines(type = "h", col = col[i])
    }
  }
  title(xlab = "Posterior", outer = T, line = 1)
  legend("center", fill = col, legend = names(list_post), border = NA, bty = "n")
  
}

# Plot supplement: Plot density of the parameter estimates
plot_dens_param <- function(list_pmcmc_run, burnin, thin,
                            col = c("#4575b4", "#d73027", "#bf812d")){
  ## Extract samples from list_pmcmc_run, after burnin and thinning
  list_samples <- lapply(list_pmcmc_run, function(X){
    samples_x <- X$pars[-c(1:burnin),]
    iter <- seq(1, nrow(samples_x), thin)
    samples_x <- (samples_x[iter,])
    if(!any(colnames(samples_x) == "v_leak")) {
      samples_x <- cbind(samples_x, v_leak = 0) 
    } else {
      samples_x[, "v_leak"] <- samples_x[, "v_leak"] * 100
    }
    
    return(samples_x)
  })
  
  ## Add a column to each element of list_samples with the name of the model
  for(i in seq_along(list_samples)){
    list_samples[[i]] <- cbind.data.frame(model = names(list_samples)[i], 
                                          list_samples[[i]])
  }
  
  ## Merge all elements of list_samples into a data frame
  df_samples <- do.call(rbind.data.frame, list_samples)
  
  ## Move df_samples to a long format
  df_samples <- pivot_longer(df_samples, !model)
  
  ## Rename some of the parameters to match the paper
  df_samples[df_samples$name == "vacc",]$name <- "v_onwards"
  df_samples[df_samples$name == "report_import",]$name <- "p_import"
  
  df_samples[df_samples$name == "delta",]$value <- 
    1/df_samples[df_samples$name == "delta",]$value
  
  df_samples$name <- factor(
    df_samples$name, 
    levels = c( "beta", "delta", "X", "Y", 
                "v_fail", "v_onwards", "v_leak", 
                "p_import", "X_import", "Y_import",
                "recov11to15",  "recov16to20", "recov21to30", "recov31to40", "recov40plus", 
                "catchup", "catchup2", 
                "theta", "b", "c"
    ))
  
  ## Generate the plot
  ggplot(df_samples) + 
    geom_density(aes(x = value, fill = model), alpha = .2, col = NA) + 
    facet_wrap(~name, scales = "free") + 
    scale_fill_manual(values = col) + theme_bw()
  
  
}

## Plot number (or proportion) of cases by age group
plot_age <- function(output, data, age_group, prop = T, legend = T,
                     col = c("#4575b4", "#d73027", "#bf812d")){
  # Set output as a list
  if(!is.list(output)){
    output <- list(output)
  }
  # Initialise list of matrices. Each element of the list will contain the 
  # number of cases per age group per simulation
  list_aggreg <- list()
  
  ## Aggregate simulations contained in each element of output by age group
  for(j in seq_along(output)){
    output_j <- output[[j]]
    list_aggreg[[j]] <- matrix(, ncol = ncol(output_j), nrow = length(age_group))
    for(i in 1:length(age_group)){
      # Create unique age group ID by taking the last two characters of paste0("age",i)
      # if i has to digits, group is i, otherwise group paste0("e", i)
      group <- paste0("age",i)
      group <- substr(group, nchar(group) - 1, nchar(group))
      
      # Select rows from output_j corresponding to the age group "group"
      # Use colsums to get the number of cases from age group "group" in each simulation
      if(length(dim(output_j)) == 3){
        new_group <- output_j[substr(rownames(output_j), nchar(rownames(output_j)) - 1, 
                                     nchar(rownames(output_j))) == group,,]
        new_group <- colSums(new_group)
        new_group <- rowSums(new_group)
      } else{
        new_group <- output_j[substr(rownames(output_j), nchar(rownames(output_j)) - 1, 
                                     nchar(rownames(output_j))) == group,]
        new_group <- colSums(new_group)
      }
      list_aggreg[[j]][i, ] <- new_group
    }
    rownames(list_aggreg[[j]]) <- age_group
    
    # If prop, compute the proportion of cases from each age group
    if(prop) {
      list_aggreg[[j]] <- t(t(list_aggreg[[j]]) / colSums(list_aggreg[[j]])) * 100
      data_age <- data[order(age_groups), lapply(.SD, function(X) 
        return(round(sum(X)/sum(data$N), 2))), by = age_groups, .SDcols = "N"]$N * 100
    } else {
      data_age <- data[order(age_groups), lapply(.SD, function(X) 
        return(sum(X))), by = age_groups, .SDcols = "N"]$N
    }
    
    ### Generate figure
    # Compute the position of the simulation set (according to the number of waning statuses in list_output)
    tot_width <- .8
    width_one <- tot_width/length(output)
    low_j <- seq_len(nrow(list_aggreg[[j]])) - tot_width/2 + (j-1)*(width_one)
    high_j <- low_j + width_one
    mid_j <- (high_j + low_j)/2
    
    # Initialise the plot
    if(j == 1){
      plot(apply(list_aggreg[[j]], 1, median)~mid_j, xaxt = "n", pch = 18, ylab = "",
           ylim = c(0, if(prop) 25 else 2500), col = col[j], xlim = c(0, 10), xlab = "")
    }
    
    # Plot the 50% simulation interval
    rect(xleft = low_j, xright = high_j,
         ybottom = apply(list_aggreg[[j]], 1, function(X) quantile(X, .25)), 
         ytop = apply(list_aggreg[[j]], 1, function(X) quantile(X, .75)), angle = 90, 
         col = transp(col[j], .8), border = NA)
    # Plot the 95% simulation interval
    rect(xleft = low_j, xright = high_j,
         ybottom = apply(list_aggreg[[j]], 1, function(X) quantile(X, .025)), 
         ytop = apply(list_aggreg[[j]], 1, function(X) quantile(X, .975)), angle = 90, 
         col = transp(col[j], .3), border = NA)
    
    # show the median value
    points(apply(list_aggreg[[j]], 1, median)~mid_j, pch = 19, col = col[j])
  }
  # Add data points
  points(data_age, col = "black", pch = 3)
  axis(side = 1, at = seq_len(nrow(list_aggreg[[j]])), labels =  rownames(list_aggreg[[j]]),
       las = 2)
  if(length(output) > 1 & legend){
    legend("topleft", fill = col, legend = names(output), border = NA, bty = "n")
  }
}

## Plot number (or proportion) of cases by region
plot_reg <- function(output, data, regions, prop = T, legend = T,
                     col = c("#4575b4", "#d73027", "#bf812d")){
  # Set output as a list
  if(!is.list(output)){
    output <- list(output)
  }
  # Initialise list of matrices. Each element of the list will contain the 
  # number of cases per age group per simulation
  list_aggreg <- list()
  
  ## Aggregate simulations contained in each element of output by region
  for(j in seq_along(output)){
    output_j <- output[[j]]
    list_aggreg[[j]] <- matrix(, ncol = ncol(output_j), nrow = length(regions))
    rownames(list_aggreg[[j]]) <- regions
    for(i in seq_along(regions)){
      ## Compute the number of cases in region i in each simulation 
      reg <- paste0("reg", i)
      if(length(dim(output_j)) == 3){
        list_aggreg[[j]][i,] <- ((output_j %>% apply(1, rowSums))[, grep(reg, rownames(output_j))]) %>% rowSums()
      } else{
        list_aggreg[[j]][i,] <- output_j[grep(reg, rownames(output_j)),] %>% colSums()
      }
    }
    
    # If prop, divide the number of cases in each region by the total number of cases per simulation
    if(prop) {
      list_aggreg[[j]] <- t(t(list_aggreg[[j]]) / colSums(list_aggreg[[j]])) * 100
      data_reg <- data[order(region), lapply(.SD, function(X) return(round(sum(X)/sum(data$N), 2))), 
                       by = region, .SDcols = "N"]$N * 100
    } else {
      data_reg <- data[order(region), lapply(.SD, function(X) 
        return(sum(X))), by = region, .SDcols = "N"]$N
      
    }
    
    
    ### Generate figure
    # Compute the position of the simulation set (according to the number of waning statuses in list_output)
    tot_width <- .8
    width_one <- tot_width/length(output)
    low_j <- seq_len(nrow(list_aggreg[[j]])) - tot_width/2 + (j-1)*(width_one)
    high_j <- low_j + width_one
    mid_j <- (high_j + low_j)/2
    
    # Initialise the plot
    if(j == 1){
      plot(apply(list_aggreg[[j]], 1, median)~mid_j, xaxt = "n", pch = 18, ylab = "",
           ylim = c(0, if(prop) 35 else 3500), col = col[j], xlim = c(0, 10), xlab = "")
    }
    # Plot the 50% simulation interval
    rect(xleft = low_j, xright = high_j,
         ybottom = apply(list_aggreg[[j]], 1, function(X) quantile(X, .25)), 
         ytop = apply(list_aggreg[[j]], 1, function(X) quantile(X, .75)), angle = 90, 
         col = transp(col[j], .8), border = NA)
    # Plot the 95% simulation interval
    rect(xleft = low_j, xright = high_j,
         ybottom = apply(list_aggreg[[j]], 1, function(X) quantile(X, .025)), 
         ytop = apply(list_aggreg[[j]], 1, function(X) quantile(X, .975)), angle = 90, 
         col = transp(col[j], .3), border = NA)
    # add the median value
    points(apply(list_aggreg[[j]], 1, median)~mid_j, pch = 19, col = col[j])
  }
  # Add data points
  points(data_reg, col = "black", pch = 3)
  rownames(list_aggreg[[j]])[rownames(list_aggreg[[j]]) == "Yorkshire and The Humber"] <- 
    "Yorkshire and \nthe Humber"
  rownames(list_aggreg[[j]])[rownames(list_aggreg[[j]]) == "East Midlands"] <- 
    "E. Midlands"
  rownames(list_aggreg[[j]])[rownames(list_aggreg[[j]]) == "West Midlands"] <- 
    "W. Midlands"
  axis(side = 1, at = seq_len(nrow(list_aggreg[[j]])), labels =  rownames(list_aggreg[[j]]),
       las = 2)
  
  if(length(output) > 1 & legend){
    legend("topleft", fill = col, legend = names(output), border = NA, bty = "n")
  }
}

## Plot number (or proportion) of cases by year
plot_year <- function(output, data, prop = "tot", legend = T, ymax = 3500, with_data = T,
                      col = c("#4575b4", "#d73027", "#bf812d")){
  # Set output as a list
  if(!is.list(output)){
    output <- list(output)
  }
  # Initialise list of matrices. Each element of the list will contain the 
  # number of cases per age group per simulation
  list_aggreg <- list()
  
  ## Aggregate simulations contained in each element of output by year
  for(j in seq_along(output)){
    output_j <- output[[j]]
    list_aggreg[[j]] <- matrix(, ncol = ncol(output_j), nrow = dim(output_j)[3])
    rownames(list_aggreg[[j]]) <- seq(2010, 2019)
    for(i in seq_len(nrow(list_aggreg[[j]]))){
      ## Compute the number of cases in year i per simulation
      list_aggreg[[j]][i,] <- output_j[,,i] %>% colSums()
      # if prop, compute the proportion of cases from the vaccine status "prop" in year i
      if(prop != "tot"){
        list_aggreg[[j]][i,] <- 100 * 
          (output_j[grep(prop, rownames(output_j)),,i] %>% colSums()) / 
          list_aggreg[[j]][i,]
        
      }
    }
    
    # if prop, compute the proportion of cases from the vaccine status "prop" per year in the data
    if(prop != "tot") {
      data_year <- 
        (data[as.numeric(date) < 3650 & grepl(prop, vaccinated), lapply(.SD, sum), 
             by = as.numeric(date)%/%365, .SDcols = "N"]$N / 
        data[as.numeric(date) < 3650, lapply(.SD, sum), 
             by = as.numeric(date)%/%365, .SDcols = "N"]$N) * 100
    } else {
      data_year <- 
        data[as.numeric(date) < 3650, 
             lapply(.SD, function(X) return(sum(X))), 
             by = as.numeric(date)%/%365, .SDcols = "N"]$N
      
    }
    
    ### Generate figure
    # Compute the position of the simulation set (according to the number of waning statuses in list_output)
    tot_width <- .8
    width_one <- tot_width/length(output)
    low_j <- seq_len(nrow(list_aggreg[[j]])) - tot_width/2 + (j-1)*(width_one)
    high_j <- low_j + width_one
    mid_j <- (high_j + low_j)/2
    
    # Initialise the plot
    if(j == 1){
      plot(apply(list_aggreg[[j]], 1, median)~mid_j, xaxt = "n", pch = 18, ylab = "",
           ylim = c(0, ymax), col = col[j], xlim = c(0, 11), 
           xlab = "")
    }
    # Plot the 50% simulation interval
    rect(xleft = low_j, xright = high_j,
         ybottom = apply(list_aggreg[[j]], 1, function(X) quantile(X, .25)), 
         ytop = apply(list_aggreg[[j]], 1, function(X) quantile(X, .75)), angle = 90, 
         col = transp(col[j], .8), border = NA)
    # Plot the 95% simulation interval
    rect(xleft = low_j, xright = high_j,
         ybottom = apply(list_aggreg[[j]], 1, function(X) quantile(X, .025)), 
         ytop = apply(list_aggreg[[j]], 1, function(X) quantile(X, .975)), angle = 90, 
         col = transp(col[j], .3), border = NA)
    # show the median value
    points(apply(list_aggreg[[j]], 1, median)~mid_j, pch = 19, col = col[j])
  }
  # Add data points
  if(with_data) points(data_year, col = "black", pch = 3)
  axis(side = 1, at = seq_len(nrow(list_aggreg[[j]])), 
       labels =  rownames(list_aggreg[[j]]),
       las = 2)
  if(length(output) > 1 & legend){
    legend("topright", fill = col, legend = names(output), border = NA, bty = "n")
  }
}

## Plot number (or proportion) of cases by vaccine status
plot_vax <- function(output, data, prop = T, col = c("#4575b4", "#d73027", "#bf812d"),
                     legend = T, ymax = Inf){
  # Set output as a list
  if(!is.list(output)){
    output <- list(output)
  }
  # Initialise list of matrices. Each element of the list will contain the 
  # number of cases per age group per simulation
  list_aggreg <- list()
  
  ## Aggregate simulations contained in each element of output by age group
  for(j in seq_along(output)){
    output_j <- output[[j]]
    list_aggreg[[j]] <- matrix(, ncol = ncol(output_j), nrow = 2)
    rownames(list_aggreg[[j]]) <- c("One dose", "Two doses")
    # Extract the number of cases for each vaccine status
    if(length(dim(output_j)) == 3){
      list_aggreg[[j]][1,] <- 
        ((output_j %>% apply(1, rowSums))[, grep("V1", rownames(output_j))]) %>% rowSums()
      list_aggreg[[j]][2,] <- 
        ((output_j %>% apply(1, rowSums))[, grep("V2", rownames(output_j))]) %>% rowSums()
      output_tot <- apply(output_j, 3, sum)
    } else {
      list_aggreg[[j]][1,] <- output_j[grep("V1", rownames(output_j)),] %>% colSums()
      list_aggreg[[j]][2,] <- output_j[grep("V2", rownames(output_j)),] %>% colSums()
      output_tot <- apply(output_j, 2, sum)
    }
    
    # If prop, compute the proportion of cases from each vaccine status
    if(prop) {
      list_aggreg[[j]] <- t(t(list_aggreg[[j]]) / output_tot) * 100
      data_vax <- data[vaccinated %in% c("new_IV1", "new_IV2"), 
                       lapply(.SD, function(X) return(round(sum(X)/sum(data$N), 2))), 
                       by = vaccinated, .SDcols = "N"]$N * 100
    } else {
      data_vax <- data[vaccinated %in% c("new_IV1", "new_IV2"), 
                       lapply(.SD, function(X) return(sum(X))), 
                       by = vaccinated, .SDcols = "N"]$N
      
    }
    
    ### Generate figure
    # Compute the position of the simulation set (according to the number of waning statuses in list_output)
    tot_width <- .8
    width_one <- tot_width/length(output)
    low_j <- seq_len(nrow(list_aggreg[[j]])) - tot_width/2 + (j-1)*(width_one)
    high_j <- low_j + width_one
    mid_j <- (high_j + low_j)/2
    
    # Initialise the plot
    if(j == 1){
      plot(apply(list_aggreg[[j]], 1, median)~mid_j, xaxt = "n", pch = 18, ylab = "",
           ylim = c(0, min(c(ymax, if(prop) 10 else 1000))), col = col[j], 
           xlim = c(0, 3), xlab = "")
    }
    # Plot the 50% simulation interval
    rect(xleft = low_j, xright = high_j,
         ybottom = apply(list_aggreg[[j]], 1, function(X) quantile(X, .25)), 
         ytop = apply(list_aggreg[[j]], 1, function(X) quantile(X, .75)), angle = 90, 
         col = transp(col[j], .8), border = NA)
    # Plot the 95% simulation interval
    rect(xleft = low_j, xright = high_j,
         ybottom = apply(list_aggreg[[j]], 1, function(X) quantile(X, .025)), 
         ytop = apply(list_aggreg[[j]], 1, function(X) quantile(X, .975)), angle = 90, 
         col = transp(col[j], .3), border = NA)
    # Add the median
    points(apply(list_aggreg[[j]], 1, median)~mid_j, pch = 19, col = col[j], cex = 2)
  }
  # Add data points
  points(data_vax, col = "black", pch = 3)
  axis(side = 1, at = seq_len(nrow(list_aggreg[[j]])), labels =  rownames(list_aggreg[[j]]))
  if(length(output) > 1 & legend){
    legend("topright", fill = col, legend = names(output), border = NA, bty = "n")
  }
}

## Plot number of single and double vaccinated cases in each simulation
plot_first_sec <- function(output, data, age, regions, legend = T,
                           col = c("#4575b4", "#d73027", "#bf812d")){
  # Set output as a list
  if(!is.list(output)){
    output <- list(output)
  }
  ## For each element of output, compute the number of single and double vaccinated cases
  for(i in seq_along(output)){
    output_i <- output[[i]]
    # Compute the number of region-age combinations
    n_reg_age <- length(regions) * length(age)
    # Overall number of cases by vaccination status
    if(length(dim(output_i)) == 3){
      vax_number <- cbind(
        output_i[3:(3+n_reg_age - 1) + n_reg_age,,] %>% colSums() %>% rowSums(),
        output_i[3:(3+n_reg_age - 1) + n_reg_age * 2,,] %>% colSums() %>% rowSums()
      )
    } else
      vax_number <- cbind(
        output_i[1:(1+n_reg_age - 1) + n_reg_age,] %>% colSums(),
        output_i[1:(1+n_reg_age - 1) + n_reg_age * 2,] %>% colSums()
      )
    
    # Initialise the plot
    if(i == 1){
      plot(data[, lapply(.SD, function(X) sum(X)), 
                by = vaccinated, .SDcols = "N"]$N[2],
           data[, lapply(.SD, function(X) sum(X)), 
                by = vaccinated, .SDcols = "N"]$N[3],
           , col = "black", pch = 19, ylab = "", 
           ylim = c(0, 500), xlim = c(0, 1000), xlab = ""
      )
      
    }
    ## Add number of single and double vaccinated cases in each simulation
    points(vax_number[,1], vax_number[,2], pch = 18, 
           col = if(nrow(vax_number) < 1000) transp(col[i], .2) else transp(col[i], .03)
           )
    
  }
  ## Add the data point
  points(data[, lapply(.SD, function(X) sum(X)), 
              by = vaccinated, .SDcols = "N"]$N[2],
         data[, lapply(.SD, function(X) sum(X)), 
              by = vaccinated, .SDcols = "N"]$N[3], col = "black", pch = 19
  )
  if(length(output) > 1 & legend){
    legend("left", fill = col, legend = names(output), border = NA, bty = "n")
  }
  
}

## Plot parameter estimate in column(s) "name"
plot_column_sample <- function(list_quant, name, col = c("#4575b4", "#d73027", "#bf812d"),
                               legend = F, ymax = 1, ymin = 0){
  # number of parameters, corresponding to the number of column names containing the argument "name"
  n_param <- sum(grepl(name, colnames(list_quant[[1]])))
  
  for(j in seq_along(list_quant)){
    ### Generate figure
    # Compute the position of the simulation set (according to the number of waning statuses in list_output)
    tot_width <- .8
    width_one <- tot_width/length(list_quant)
    low_j <- seq_len(n_param) - tot_width/2 + (j-1)*(width_one)
    high_j <- low_j + width_one
    mid_j <- (high_j + low_j)/2
    
    # Initialise the plot
    if(j == 1){
      plot(list_quant[[j]][3, grep(name, colnames(list_quant[[j]]))] ~ mid_j,
           ylim = c(ymin,ymax), col = col[j], pch = 18, xaxt = "n", ylab = "",
           xlim = if(n_param == 1) c(0.5, 1.5) else c(0, n_param + 1), xlab = ""
      )
    }
    # Plot the 50% credible interval
    rect(xleft = low_j, xright = high_j,
         ybottom = list_quant[[j]][2, grep(name, colnames(list_quant[[j]]))], 
         ytop = list_quant[[j]][4, grep(name, colnames(list_quant[[j]]))], 
         angle = 90, col = transp(col[j], .8), border = NA)
    # Plot the 95% credible interval
    arrows(x0 = mid_j, x1 = mid_j,
           y0 = list_quant[[j]][1, grep(name, colnames(list_quant[[j]]))], 
           y1 = list_quant[[j]][5, grep(name, colnames(list_quant[[j]]))],
           angle = 90, code = 3, length = .03, col = col[j])
    # Plot the median
    points(list_quant[[j]][3, grep(name, colnames(list_quant[[j]]))] ~ mid_j,
           col = col[j], pch = 18
    )
  }
  if(legend){
    legend("topleft", fill = col, legend = names(list_quant), border = NA, bty = "n")
  }
  
}

## Plot seasonality of importation (if import == T), or transmission (if import == F)
plot_seasonality <- function(list_sample, import, col = c("#4575b4", "#d73027", "#bf812d")){
  time <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), "day")
  for(i in seq_along(list_sample)){
    ## Extract parameters X and Y
    if(import){
      X <- list_sample[[i]][, "X_import"]
      Y <- list_sample[[i]][, "Y_import"]
    } else {
      X <- list_sample[[i]][, "X"]
      Y <- list_sample[[i]][, "Y"]
    }
    
    ## Initialise matrix containing the seasonality for each day
    season <- matrix(nrow = length(X), ncol = 365)
    ## Compute seasonality
    for(j in seq_along(X)){
      season[j,] <- (1 + X[j] * cos(2 * pi * (1:365) / 365.25 + Y[j])) * 100
    }
    # Compute 95% and 50% quantile at each time step
    quant_day <- apply(season, 2, function(X) 
      return(quantile(X, c(0.025, 0.25, .5, .75, .975))))
    
    # Initialise the plot
    if(i == 1){
      plot(quant_day[3,] ~ time, ylim = (if(!import) c(.75, 1.25) else c(.1, 2)) * 100, 
           type = "l", ylab = "", main = if(import) "Importation" else "Transmission",
           xaxt = "n", xlim = c(as.Date("2023-01-01"), as.Date("2023-12-31")))
    }
    # Plot the 95% credible interval
    polygon(x = c(time, rev(time)), y = c(quant_day[1,], rev(quant_day[5,])), 
            border = NA, col = transp(col[i], .5))
    # Plot the 50% credible interval
    polygon(x = c(time, rev(time)), y = c(quant_day[2,], rev(quant_day[4,])), 
            border = NA, col = transp(col[i], .5))
    # Add median
    lines(quant_day[3,] ~ time, col = col[i])
    
    axis(1, at = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), "month"), 
         labels = format(seq(as.Date("2023-01-01"), as.Date("2023-12-31"), "month"), 
                         format = "%b"))
  }
}

## Compute the value of R0 using the next generation matrix
compute_r0 <- function(samples, N, distance, all_data, infectious_period = 8){
  # initialise the vector containing R0 in each sample
  R0 <- numeric(nrow(samples))
  
  for(i in seq_len(nrow(samples))){
    # Extract beta
    beta <- samples[i, "beta"]
    # If distance is estimated, extract b, c, and theta
    if(distance != "fixed"){
      b <- samples[i, "b"]
      c <- samples[i, "c"]
      theta <- samples[i, "theta"]
    } else b <- c <- theta <- 1
    
    # Compute connectivity matrix between regions using the gravity model
    d_a <- all_data$ref_d * 0
    for(k in seq_len(nrow(d_a))){
      d_a[k,] <- (colSums(N[,])^b) * (theta * ((all_data$ref_d[k,] - 1 )^(-1)) * 
                                        colSums(N)[k]^c) / colSums(N)[k]
    }
    diag(d_a) <- 1
    ## Extract contact matrix between age groups
    m <- all_data$ref_m
    
    ## Merge contact matrix and connectivity matrix
    connect_matrix <- matrix(, nrow = nrow(m) * nrow(d_a),
                             ncol = nrow(m) * nrow(d_a))
    rownames(connect_matrix) <- colnames(connect_matrix) <- 
      apply(expand_grid(rownames(m), rownames(d_a)), 1, paste, collapse = "-")
    ## Compute next generation matrix
    R_start <- connect_matrix
    for(k in seq_len(nrow(d_a))){
      for(j in seq_len(nrow(m))){
        ## Compute connectivity matrix for individuals from region k and age j
        connect_matrix[(k - 1) * nrow(m) + j,] <- 
          outer(d_a[k, ], m[j, ], "*") %>% c
        
        # R_start is each element of connect_matrix * beta * infectious period per 
        # individual in each region/ age group
        R_start[(k - 1) * nrow(m) + j,] <- 
          connect_matrix[(k - 1) * nrow(m) + j,] * beta * infectious_period * 
          c(t(N)) / sum(N)
      }
    }
    # Compute eigen value
    R0[i] <- as.numeric(eigen(R_start)$values[1])
  }
  return(R0)
}

## Generate transparent colour
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) 
    rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}
