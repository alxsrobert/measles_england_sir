####function to compare yearly cases for two scenarios

yearly_cases_fig_flexible_new <- function(dt_output1, dt_output2, name1, name2,
                                      color1, color2){
  dt_output1 <- readRDS(paste0("Output/models/", dt_output1))
  
  rows_new_cases <- rownames(dt_output1)[grep("new_I", rownames(dt_output1))]
  ## Sum number of new infected per day
  new_cases <- cbind.data.frame(reg = "National", iter = seq_len(ncol(dt_output1)), 
                                dt_output1[rows_new_cases,,] %>% colSums())
  
  
  time <- dt_output1[1, 1, ]
  colnames(new_cases) <- c("reg", "iter", as.character(time))
  rm(dt_output1)
  gc()
  ## Change new_cases to long format (to then use ggplot) 
  long_new_cases <- pivot_longer(as.data.frame(new_cases), 
                                 cols = c(as.character(time)), 
                                 names_to = "time",
                                 values_to = "new_cases")
  long_new_cases <- as.data.table(long_new_cases)
  long_new_cases$time <- long_new_cases$time %>% as.numeric %>% as.Date(origin = "1970-01-01")
  
  ## Aggregate by region / iteration / year
  cases_per_year <- long_new_cases[, lapply(.SD, sum), by = .(iter, year(time), reg)]
  
  #average number of cases by iteration
  tmp1 <- data.table()
  for(i in 2010:2019){
    
    vec <- quantile(cases_per_year[year == i, new_cases], probs = c(0.025, 0.125, 0.25, 0.5, 0.75, 0.875, 0.975))
    new_row <- data.table(year = i, lb_95 = vec[[1]], lb_75= vec[[2]], lb_50 = vec[[3]], 
                          median = vec[[4]], ub_50 = vec[[5]], ub_75 = vec[[6]], ub_95 = vec[[7]])
    tmp1 <- rbind(tmp1, new_row)
  }
  
  
  #adding second data set
  dt_output2 <- readRDS(paste0("Output/models/", dt_output2))
  rows_new_cases <- rownames(dt_output2)[grep("new_I", rownames(dt_output2))]
  ## Sum number of new infected per day
  new_cases <- cbind.data.frame(reg = "National", iter = seq_len(ncol(dt_output2)), 
                                dt_output2[rows_new_cases,,] %>% colSums())
  
  
  time <- dt_output2[1, 1, ]
  colnames(new_cases) <- c("reg", "iter", as.character(time))
  rm(dt_output2)
  gc()
  ## Change new_cases to long format (to then use ggplot) 
  long_new_cases <- pivot_longer(as.data.frame(new_cases), 
                                 cols = c(as.character(time)), 
                                 names_to = "time",
                                 values_to = "new_cases")
  long_new_cases <- as.data.table(long_new_cases)
  long_new_cases$time <- long_new_cases$time %>% as.numeric %>% as.Date(origin = "1970-01-01")
  
  ## Aggregate by region / iteration / year
  cases_per_year <- long_new_cases[, lapply(.SD, sum), by = .(iter, year(time), reg)]
  
  #average number of cases by iteration
  tmp2 <- data.table()
  for(i in 2010:2019){
    
    vec <- quantile(cases_per_year[year == i, new_cases], probs = c(0.025, 0.125, 0.25, 0.5, 0.75, 0.875, 0.975))
    new_row <- data.table(year = i, lb_95 = vec[[1]], lb_75= vec[[2]], lb_50 = vec[[3]], 
                          median = vec[[4]], ub_50 = vec[[5]], ub_75 = vec[[6]], ub_95 = vec[[7]])
    tmp2 <- rbind(tmp2, new_row)
  }
  
  tmp1[, scenario := name1]
  tmp2[, scenario := name2]
  
  tmp <- rbind(tmp1, tmp2)
  
  tmp <- tmp[, scenario := factor(scenario, levels = c(name1, name2), ordered = T)]
  
  plot <- tmp %>%
    ggplot(aes(x = year, group = scenario))+
    geom_line(aes(y = median, color = scenario))+
    scale_color_manual(values = c(color1, color2))+ 
    geom_ribbon (aes(ymin = lb_50, ymax = ub_50 ,  fill = scenario ),  alpha = 0.3, linetype = 0)+
    scale_fill_manual(values = c(color1, color2))+
    scale_x_continuous(name = "Year", breaks = c(2011, 2013, 2015, 2017, 2019))+
    scale_y_continuous(name = "N measles cases", breaks = seq(0, 3000, by = 500), limits = c(0, 3000))+
    ylab("N measles cases")+
    xlab("Year")+
    theme_classic()+
    theme(legend.position="bottom",
          axis.text.x = element_text(color = "grey20", size = 20, angle = 45, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
          axis.title.x = element_text(color = "grey20", size = 22, angle = 0, hjust = .5, vjust = 0, face = "italic"),
          axis.title.y = element_text(color = "grey20", size = 22, angle = 90, hjust = .5, vjust = 1, face = "italic"),
          legend.text = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          legend.title = element_blank())
  
  return(plot)
}


####function to compare yearly cases for two scenarios with higher y axis
#version with higher y-axis
yearly_cases_fig_flexible_higher_y <- function(dt_output1, dt_output2, name1, name2,
                                               color1, color2){
  
  dt_output1 <- readRDS(paste0("Output/models/", dt_output1))
  rows_new_cases <- rownames(dt_output1)[grep("new_I", rownames(dt_output1))]
  ## Sum number of new infected per day
  new_cases <- cbind.data.frame(reg = "National", iter = seq_len(ncol(dt_output1)), 
                                dt_output1[rows_new_cases,,] %>% colSums())
  
  
  time <- dt_output1[1, 1, ]
  colnames(new_cases) <- c("reg", "iter", as.character(time))
  rm(dt_output1)
  gc()
  ## Change new_cases to long format (to then use ggplot) 
  long_new_cases <- pivot_longer(as.data.frame(new_cases), 
                                 cols = c(as.character(time)), 
                                 names_to = "time",
                                 values_to = "new_cases")
  long_new_cases <- as.data.table(long_new_cases)
  long_new_cases$time <- long_new_cases$time %>% as.numeric %>% as.Date(origin = "1970-01-01")
  
  ## Aggregate by region / iteration / year
  cases_per_year <- long_new_cases[, lapply(.SD, sum), by = .(iter, year(time), reg)]
  
  #average number of cases by iteration
  tmp1 <- data.table()
  for(i in 2010:2019){
    
    vec <- quantile(cases_per_year[year == i, new_cases], probs = c(0.025, 0.125, 0.25, 0.5, 0.75, 0.875, 0.975))
    new_row <- data.table(year = i, lb_95 = vec[[1]], lb_75= vec[[2]], lb_50 = vec[[3]], 
                          median = vec[[4]], ub_50 = vec[[5]], ub_75 = vec[[6]], ub_95 = vec[[7]])
    tmp1 <- rbind(tmp1, new_row)
  }
  
  
  #adding second data set
  dt_output2 <- readRDS(paste0("Output/models/", dt_output2))
  rows_new_cases <- rownames(dt_output2)[grep("new_I", rownames(dt_output2))]
  ## Sum number of new infected per day
  new_cases <- cbind.data.frame(reg = "National", iter = seq_len(ncol(dt_output2)), 
                                dt_output2[rows_new_cases,,] %>% colSums())
  
  
  time <- dt_output2[1, 1, ]
  colnames(new_cases) <- c("reg", "iter", as.character(time))
  rm(dt_output2)
  gc()
  ## Change new_cases to long format (to then use ggplot) 
  long_new_cases <- pivot_longer(as.data.frame(new_cases), 
                                 cols = c(as.character(time)), 
                                 names_to = "time",
                                 values_to = "new_cases")
  long_new_cases <- as.data.table(long_new_cases)
  long_new_cases$time <- long_new_cases$time %>% as.numeric %>% as.Date(origin = "1970-01-01")
  
  ## Aggregate by region / iteration / year
  cases_per_year <- long_new_cases[, lapply(.SD, sum), by = .(iter, year(time), reg)]
  
  #average number of cases by iteration
  tmp2 <- data.table()
  for(i in 2010:2019){
    
    vec <- quantile(cases_per_year[year == i, new_cases], probs = c(0.025, 0.125, 0.25, 0.5, 0.75, 0.875, 0.975))
    new_row <- data.table(year = i, lb_95 = vec[[1]], lb_75= vec[[2]], lb_50 = vec[[3]], 
                          median = vec[[4]], ub_50 = vec[[5]], ub_75 = vec[[6]], ub_95 = vec[[7]])
    tmp2 <- rbind(tmp2, new_row)
  }
  
  tmp1[, scenario := name1]
  tmp2[, scenario := name2]
  
  tmp <- rbind(tmp1, tmp2)
  
  tmp <- tmp[, scenario := factor(scenario, levels = c(name1, name2), ordered = T)]
  
  plot <- tmp %>%
    ggplot(aes(x = year, group = scenario))+
    geom_line(aes(y = median, color = scenario))+
    scale_color_manual(values = c(color1, color2))+ 
    geom_ribbon (aes(ymin = lb_50, ymax = ub_50 ,  fill = scenario ),  alpha = 0.3, linetype = 0)+
    scale_fill_manual(values = c(color1, color2))+
    scale_x_continuous(name = "Year", breaks = c(2011, 2013, 2015, 2017, 2019))+
    scale_y_continuous(name = "N measles cases", breaks = seq(0, 4000, by = 500), limits = c(0, 4000))+
    ylab("N measles cases")+
    xlab("Year")+
    theme_classic()+
    theme(legend.position="bottom",
          axis.text.x = element_text(color = "grey20", size = 20, angle = 45, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "grey20", size = 20, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
          axis.title.x = element_text(color = "grey20", size = 22, angle = 0, hjust = .5, vjust = 0, face = "italic"),
          axis.title.y = element_text(color = "grey20", size = 22, angle = 90, hjust = .5, vjust = 1, face = "italic"),
          legend.text = element_text(color = "grey20", size = 16, angle = 0, hjust = .5, vjust = .5, face = "plain"),
          legend.title = element_blank())
  
  return(plot)
}


####function to present the absolute number of cases per age group 
plot_age_abs <- function(output1, output2,
                         name1, name2, 
                         col1, col2){
  
  age_group <- c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,10)", "[10,15)",
                 "[15,20)", "[20,30)", "[30,40)", "[40,100]")
  
  #first model
  output1 <- readRDS(paste0("Output/models/", output1))
  new_aggreg1 <- matrix(, ncol = ncol(output1), nrow = length(age_group))

  
  for(i in 1:length(age_group)){
    group <- paste0("age",i)
    group <- substr(group, nchar(group) - 1, nchar(group))
    
    new_group1 <- output1[substr(rownames(output1), nchar(rownames(output1)) - 1, 
                                 nchar(rownames(output1))) == group,,]
    new_group1 <- colSums(new_group1)
    new_group1 <- rowSums(new_group1)
    new_aggreg1[i, ] <- new_group1
  }
  
  rm(output1)
  gc()
  
  #second model
  output2 <- readRDS(paste0("Output/models/", output2))
  new_aggreg2 <- matrix(, ncol = ncol(output2), nrow = length(age_group))
  
  for(i in 1:length(age_group)){
    group <- paste0("age",i)
    group <- substr(group, nchar(group) - 1, nchar(group))
        new_group2 <- output2[substr(rownames(output2), nchar(rownames(output2)) - 1, 
                                 nchar(rownames(output2))) == group,,]
        
    new_group2 <- colSums(new_group2)
    new_group2 <- rowSums(new_group2)
    new_aggreg2[i, ] <- new_group2
  }
  
  rm(output2)
  gc()
  
  rownames(new_aggreg1) <- age_group
  rownames(new_aggreg2) <- age_group
  
  
  #change data format
  res1 <- data.table()
  res2 <- data.table()
  for(i in 1:nrow(new_aggreg1)){
    tmp1 <- data.table(lb  = quantile(new_aggreg1[i,], prob = c(0.25)),
                       med = quantile(new_aggreg1[i,], prob = c(0.5)),
                       ub = quantile(new_aggreg1[i,], prob = c(0.75)))
    tmp2 <- data.table(lb  = quantile(new_aggreg2[i,], prob = c(0.25)),
                       med = quantile(new_aggreg2[i,], prob = c(0.5)),
                       ub = quantile(new_aggreg2[i,], prob = c(0.75)))
    res1 <- rbind(res1, tmp1)
    res2 <- rbind(res2, tmp2)
    
  }
  
  res1 <- cbind(age = row.names(new_aggreg1), res1)
  res2 <- cbind(age = row.names(new_aggreg2), res2)
  res1[, cat := name1]
  res2[, cat := name2]
  res <- rbind(res1, res2)
  
  res[, age := factor(age, levels = c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,10)", "[10,15)",
                                      "[15,20)", "[20,30)", "[30,40)", "[40,100]"), ordered = T)]
  res[, cat := factor(cat, levels = c(name1, name2), ordered = T)]
  plot <- res %>%
    ggplot(aes(x = age, y = med, group = cat, color = cat, fill = cat))+
    geom_point(position = position_dodge(width = 0.5))+
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2, 
                  position = position_dodge(width = 0.5))+
    scale_color_manual(name = "Scenario", values = c(col1, col2))+
    ylab("N cases")+
    xlab("Age group")+
    theme_classic()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14, vjust = 1),
          axis.title.x = element_text(size = 14, vjust = 1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1))+
    guides(color = guide_legend(override.aes = list(size = 3)),
           fill = "none")
  
  
  return(plot)
}


###function to present the relative number of cases per age group

plot_age_prop <- function(output1, output2,
                          name1, name2, 
                          col1, col2){
  
  age_group <- c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,10)", "[10,15)",
                 "[15,20)", "[20,30)", "[30,40)", "[40,100]")
  
  #first model
  output1 <- readRDS(paste0("Output/models/", output1))
  new_aggreg1 <- matrix(, ncol = ncol(output1), nrow = length(age_group))
  
  
  for(i in 1:length(age_group)){
    group <- paste0("age",i)
    group <- substr(group, nchar(group) - 1, nchar(group))
    
    new_group1 <- output1[substr(rownames(output1), nchar(rownames(output1)) - 1, 
                                 nchar(rownames(output1))) == group,,]
    new_group1 <- colSums(new_group1)
    new_group1 <- rowSums(new_group1)
    new_aggreg1[i, ] <- new_group1
  }
  
  rm(output1)
  gc()
  
  #second model
  output2 <- readRDS(paste0("Output/models/", output2))
  new_aggreg2 <- matrix(, ncol = ncol(output2), nrow = length(age_group))
  
  for(i in 1:length(age_group)){
    group <- paste0("age",i)
    group <- substr(group, nchar(group) - 1, nchar(group))
    new_group2 <- output2[substr(rownames(output2), nchar(rownames(output2)) - 1, 
                                 nchar(rownames(output2))) == group,,]
    
    new_group2 <- colSums(new_group2)
    new_group2 <- rowSums(new_group2)
    new_aggreg2[i, ] <- new_group2
  }
  
  rm(output2)
  gc()
  
  rownames(new_aggreg1) <- age_group
  rownames(new_aggreg2) <- age_group
  
  new_aggreg_prop1 <- t(t(new_aggreg1) / colSums(new_aggreg1)) * 100
  new_aggreg_prop2 <- t(t(new_aggreg2) / colSums(new_aggreg2)) * 100
  
  #change data format
  res1 <- data.table()
  res2 <- data.table()
  for(i in 1:nrow(new_aggreg_prop1)){
    tmp1 <- data.table(lb  = quantile(new_aggreg_prop1[i,], prob = c(0.25)),
                       med = quantile(new_aggreg_prop1[i,], prob = c(0.5)),
                       ub = quantile(new_aggreg_prop1[i,], prob = c(0.75)))
    tmp2 <- data.table(lb  = quantile(new_aggreg_prop2[i,], prob = c(0.25)),
                       med = quantile(new_aggreg_prop2[i,], prob = c(0.5)),
                       ub = quantile(new_aggreg_prop2[i,], prob = c(0.75)))
    res1 <- rbind(res1, tmp1)
    res2 <- rbind(res2, tmp2)
    
  }
  
  res1 <- cbind(age = row.names(new_aggreg_prop1), res1)
  res2 <- cbind(age = row.names(new_aggreg_prop2), res2)
  res1[, cat := name1]
  res2[, cat := name2]
  res <- rbind(res1, res2)
  
  res[, age := factor(age, levels = c("[0,1)", "[1,2)", "[2,3)", "[3,4)", "[4,5)", "[5,6)", "[6,10)", "[10,15)",
                                      "[15,20)", "[20,30)", "[30,40)", "[40,100]"), ordered = T)]
  res[, cat := factor(cat, levels = c(name1, name2), ordered = T)]
  plot <- res %>%
    ggplot(aes(x = age, y = med, group = cat, color = cat, fill = cat))+
    geom_point(position = position_dodge(width = 0.5))+
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.2, 
                  position = position_dodge(width = 0.5))+
    scale_color_manual(name = "Scenario", values = c(col1, col2))+
    ylab("% of cases")+
    xlab("Age group")+
    theme_classic()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14, vjust = 1),
          axis.title.x = element_text(size = 14, vjust = 1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth=1))+
    guides(color = guide_legend(override.aes = list(size = 3)),
           fill = "none")
  
  return(plot)
}

