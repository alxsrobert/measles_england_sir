stratified_plot <- function(by_age, by_reg, N_reg, N_age, dt_output, cats, 
                            colours, main_lab, y_lab, prop = FALSE, legend = TRUE,
                            outer_y = FALSE, names_reg = NULL, names_age = NULL,
                            aggreg = ""){
  ## Difference process if the figure is stratified by region / age
  if(by_age & by_reg){
    for(j in seq_len(N_age)){
      for(i in seq_len(N_reg)){
        if(is.null(names_reg)) main_lab <- paste0("Region ", i) else 
          main_lab <- names_reg[i]
        if(is.null(names_age)) main_lab <- paste0(main_lab, " Age ", j) else 
          main_lab <- paste(main_lab, names_age[j], sep = " ")
        ## Extract entries from region i and age j
        labs_cats <- lapply(cats, function(X) return(paste0(X, "_reg", i, "_age", j)))
        plot_cats(dt_output = dt_output, labs_cats = labs_cats, colour = colours, 
                  main_lab = main_lab, outer_y = outer_y, legend = (i == 1), 
                  y_lab = if(i == 1) y_lab else "", prop = prop, aggreg = aggreg)
      }
    }
  } else if(by_age){
    for(j in seq_len(N_age)){
      if(is.null(names_age)) main_lab <- paste0("Age ", j) else 
        main_lab <- paste0(names_age[j], " Years old")
      ## Extract entries from region i and age j
      labs_cats <- lapply(cats, function(X) 
        return(do.call(paste0, expand.grid(X, "_reg", seq_len(N_reg), "_age", j))))
      plot_cats(dt_output = dt_output, labs_cats = labs_cats, colour = colours, 
                main_lab = main_lab, outer_y = outer_y, y_lab = y_lab, 
                legend = legend, prop = prop, aggreg = aggreg)
    }
  } else if(by_reg){
    for(i in seq_len(N_reg)){
      if(is.null(names_reg)) main_lab <- paste0("Region ", i) else 
        main_lab <- names_reg[i]
      ## Extract entries from region i and age j
      labs_cats <- lapply(cats, function(X) 
        return(do.call(paste0, expand.grid(X, "_reg", i, "_age", seq_len(N_age)))))
      plot_cats(dt_output = dt_output, labs_cats = labs_cats, colour = colours, 
                main_lab = main_lab, outer_y = outer_y, y_lab = y_lab, 
                legend = legend, prop = prop, aggreg = aggreg)
    }
  } else {
    comb_reg_age <- do.call(paste0, expand.grid("_reg", seq_len(N_reg), "_age", seq_len(N_age)))
    ## Extract entries from region i and age j
    labs_cats <- lapply(cats, function(X) 
      return(do.call(paste0, expand.grid(X, comb_reg_age))))
    
    plot_cats(dt_output = dt_output, labs_cats = labs_cats, colour = colours, 
              main_lab = main_lab, outer_y = outer_y, y_lab = y_lab, 
              legend = legend, prop = prop, aggreg = aggreg)
  }
  
}

plot_cats <- function(dt_output, labs_cats, colours, main_lab, y_lab, prop = FALSE, 
                      legend = TRUE, outer_y = F, aggreg = ""){
  # Extract x axis: time
  time <- dt_output[1, 1, ]
  output <- dt_output[-1, , ]
  if(min(time) != 1) time <- as.Date(time, origin = "1970-01-01")
  
  # Create cats: which rows correspond to labs_cats
  cats <- list()
  for(i in seq_along(labs_cats)){
    cats[[i]] <- which(rownames(output) %in% labs_cats[[i]])
  }
  
  # Create sum of the selected columns per simulation / category
  output_per_cat <- lapply(cats, function(X){
    if(length(X) > 1) return(t(colSums(output[X,,]))) else return(t(output[X,,]))
  })
  
  if(aggreg == "week"){
    if(class(time) == "Date") time <- lubridate::floor_date(time, "week") else
      time <- time %/% 7
  } else if (aggreg == "month"){
    if(class(time) == "Date") time <- lubridate::floor_date(time, "month") else
      time <- time %/% 30
  } else if (aggreg == "year"){
    if(class(time) == "Date") time <- lubridate::floor_date(time, "year") else
      time <- time %/% 365
  }
  
  output_per_cat <- lapply(output_per_cat, function(X) 
    return(aggregate(X, list(time), sum)[,-1]))
  time <- unique(time)
  # If the proportion is plotted: compute the total number of individuals, set ymax to 1
  if(prop){
    N <- 0
    for(i in seq_along(output_per_cat)) N <- N + output_per_cat[[i]]
    output_per_cat <- lapply(output_per_cat, function(X) return(X / N))
    ymax <- 1
  } else{
    ymax <- max(unlist(lapply(output_per_cat, function(X) 
      return(max(X))
    )))
  }
  # Plot the first category
  matplot(time, output_per_cat[[1]], type = "l", xlab = "", ylab = "", yaxt="none", 
          col = colours[[1]], lty = 1, ylim = c(0, ymax), main = main_lab)
  
  # Add the other categories to the plot
  if(length(cats) > 1){
    for(i in seq(2, length(cats))){
      matlines(time, output_per_cat[[i]], col = colours[[i]], lty = 1)
    }
  }
  if(legend == T){
    ## Add the legend in the topleft panel
    legend("right", lwd = 1, col = colours, legend = names(colours), bty = "n")
  }
  axis(2, las = 2)
  
  # # Add axis names
  if(aggreg == "") {
    title(xlab = "Time (days)", outer = T, line = 0, cex.lab = 2)
  } else 
    title(xlab = paste0("Time (", aggreg, ")"), outer = T, line = 0, cex.lab = 2)
  title(ylab = y_lab, outer = outer_y, line = if(outer_y) 0 else 4, 
        cex.lab = if(outer_y) 2 else 1)
  
}

proportion_outbreak <- function(dt_output, names_reg = NA, which_reg = NA){
  
  # Extract time from dt_output, use date if time does not start at 1
  if(min(dt_output["Time",,]) != 1) {
    time <- as.Date(dt_output["Time",1,], origin = "1970-01-01") 
  } else
    time <- dt_output["Time",1,]
  
  # Check whether stratified by region
  if(!all(is.na(which_reg))){
    for(i in which_reg){
      # For each region, extract the rows with the number of new infected per day
      rows_i <- c(grep(paste0("new_IS_reg", i), rownames(dt_output)),
                  grep(paste0("new_IV1_reg", i), rownames(dt_output)),
                  grep(paste0("new_IV2_reg", i), rownames(dt_output)))
      # Aggregate the number of new cases and put it in a matrix called "new_cases"
      if(!exists("new_cases")){
        new_cases <- cbind(reg = i, iter = seq_len(ncol(dt_output)),
                           dt_output[rows_i,,] %>% colSums())
      } else {
        new_cases <- rbind(new_cases, 
                           cbind(reg = i, iter = seq_len(ncol(dt_output)), 
                                 dt_output[rows_i,,] %>% colSums()))
      }
    }
  } else{
    ## If not region-stratified, extract all rows with the number of new infected
    rows_new_cases <- rownames(dt_output)[grep("new_I", rownames(dt_output))]
    ## Sum number of new infected per day
    new_cases <- cbind.data.frame(reg = "National", iter = seq_len(ncol(dt_output)), 
                                  dt_output[rows_new_cases,,] %>% colSums())
  }
  
  ## Set column names
  colnames(new_cases) <- c("reg", "iter", as.character(time))
  
  ## Change new_cases to long format (to then use ggplot) 
  long_new_cases <- pivot_longer(as.data.frame(new_cases), 
                                 cols = c(as.character(time)), 
                                 names_to = "time",
                                 values_to = "new_cases")
  long_new_cases <- as.data.table(long_new_cases)
  ## Aggregate by region / iteration / year
  cases_per_year <- long_new_cases[, lapply(.SD, sum), by = .(iter, year(time), reg)]
  ## Use region name
  if(!all(cases_per_year$reg == "National"))
    cases_per_year[, reg := factor(names_reg, levels = names_reg)[reg]]
  
  ## Aggregate number of annual cases into categories
  vec_cut <- c(-1, 100, 250, 500, 1000, 2000)
  cases_per_year[, cat_cases := cut(new_cases, vec_cut)]
  
  ## Change first and last categories to below / above threshold
  cases_per_year[is.na(cat_cases), cat_cases := paste0(">= ", max(vec_cut))]
  levels(cases_per_year$cat_cases)[1] <- paste0("< ", sort(vec_cut)[2])
  cases_per_year[, cat_cases := factor(cat_cases, levels = rev(levels(cat_cases)))]
  
  ## Plot
  ggplot(cases_per_year, aes(x = year, fill = cat_cases)) + 
    geom_bar(position = "fill") + facet_wrap(.~reg) +
    labs(x = "Year", y = "Proportion of Simulations", 
         fill = "Number of Cases")
  
}
