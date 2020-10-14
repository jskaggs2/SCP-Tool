cool_curve <- function(temperature_start,cooling_rate,iter){
  temperature_start * ((1-cooling_rate)^iter)
}

evaluate_prioritization <- function(solution_name, field_zone, field_value, df_original, df_result, evalThreshold){
  fields_rep <- names(df_original)[grep(pattern = field_value, x = names(df_original))] 
  
  dat_rank <- merge(
    df_original[,c(field_zone, fields_rep)],
    df_result[,c(field_zone, solution_name)],
    by = field_zone)
  dat_rank <- dat_rank[order(dat_rank[solution_name]),] # insert check to only count rows which have a rank! throw out NA and 0

  return(sapply(X = dat_rank[,fields_rep], FUN = function(x) length(which(cumsum(x) < evalThreshold))))
}

caz <- function(zone_sp,total_sp,weight){
  delta_sp <- zone_sp / total_sp
  delta_sp <- ifelse(is.na(delta_sp), 0, delta_sp)
  delta_sp_w <- delta_sp * weight
  return(delta_sp_w)
  #delta_spp <- ifelse(delta_sp_w > delta_spp, delta_sp_w, delta_spp)
}

abf <- function(zone_sp,total_sp,weight){
  delta_sp <- total_sp - zone_sp
  delta_sp <- ifelse(is.na(delta_sp), 0, delta_sp)
  delta_sp_w <- delta_sp * weight
  return(delta_sp_w)
  #delta_spp <- delta_spp + delta_sp_w
}

map_all_results <- function(result, path_sf_zones){
  nad16n <- "+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" #temporary
  solutions <- result[["settings"]]$solutions
  field_zone <- result[["settings"]]$field_zone
  for(s in 1:solutions){
    main <- names(result[["summary"]])[s+1]
    ranks <- result[["summary"]][c(1,s+1)]
    huc12 <- sf::st_read(path_sf_zones, quiet = TRUE) #temporary
    huc12 <- st_transform(huc12, crs = st_crs(nad16n)) #temporary
    huc12_result <- merge(huc12, ranks, by = field_zone, all.x = TRUE)
    plot(huc12_result[main], main = main)
  }
}

map_results <- function(result, path_sf_zones, s){
  s <- as.numeric(s)
  nad16n <- "+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" #temporary
  #solutions <- result[["settings"]]$solutions
  field_zone <- result[["settings"]]$field_zone
  main <- names(result[["summary"]])[s]
  ranks <- result[["summary"]][c(1,s)]
  huc12 <- sf::st_read(path_sf_zones, quiet = TRUE) #temporary
  huc12 <- st_transform(huc12, crs = st_crs(nad16n)) #temporary
  huc12_result <- merge(huc12, ranks, by = field_zone, all.x = TRUE)
  plot(huc12_result[main], main = main)
}

map_results_leaflet <- function(result, path_sf_zones, s){
  # attach data to sf object
  s <- as.numeric(s)
  nad16n <- "+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" #temporary
  field_zone <- result[["settings"]]$field_zone
  main <- names(result[["summary"]])[s]
  ranks <- result[["summary"]][1:(s+1)]
  huc12 <- sf::st_read(path_sf_zones, quiet = TRUE) #temporary
  huc12 <- st_transform(huc12, crs = st_crs(4326)) #temporary
  huc12_result <- merge(huc12, ranks, by = field_zone, all.x = FALSE)
  
  # define plot attributes
  rank_name <- names(result[["summary"]])[s+1]
  binpal <- colorBin("YlOrRd", huc12_result[[rank_name]], bins = 10, pretty = FALSE, reverse = TRUE)
  label <- paste(
    "Name:", huc12_result$Name, ";",
    "Rank:", huc12_result[[rank_name]])
  
  # plot
  leaflet(huc12_result) %>% 
    addProviderTiles("OpenStreetMap.Mapnik") %>% 
    addPolygons(stroke = NA, fillColor = binpal(huc12_result[[rank_name]]), fillOpacity = 0.7,
                highlight = highlightOptions(color = "white", weight = 1, bringToFront = TRUE), label = label)
}

plot_eval <- function(result, s){
  zones <- length(result[["settings"]]$zones)
  features <- result[["settings"]]$features 
  e <- result[["evals"]][[s]]
  main <- names(result[["summary"]])[s+1]
  etab <- as.data.frame(table(e))
  etab[paste0("cumsum_", s)] <- cumsum(etab$Freq)
  etab <- sapply(etab, function(x) as.numeric(paste(x)))
  evs <- as.data.frame(etab)
  eval_title <- paste("Accumulation curve for", main)
  eval_ylab <- "cumulative count of features represented"
  eval_xlab <- "zones protected"
  eval_ylim <- c(0,length(features)*1.1)
  eval_xlim <- c(0, zones)
  plot(evs[[paste0("cumsum_", s)]] ~ evs$e+1, pch = 1,ylim = eval_ylim, xlim = eval_xlim, main = eval_title , xlab = eval_xlab, ylab = eval_ylab)
  #legend("right", legend = main, col = "black", pch = 1, cex=0.8)
}

plot_eval_all <- function(result){
  features <- result[["settings"]]$features
  zones <- length(result[["settings"]]$zones)
  evs <- data.frame(e = seq(0, zones, 1))
  solutions <- result[["settings"]]$solutions
  if(solutions > 1){
    for(s in 1:solutions){
      e <- result[["evals"]][[s]]
      etab <- as.data.frame(table(e))
      etab[paste0("cumsum_", s)] <- cumsum(etab$Freq)
      etab <- sapply(etab, function(x) as.numeric(paste(x)))
      
      # Plot eval curves together
      evs <- merge(evs, etab[,c(1,3)], by = "e", all.x = TRUE)
    }
    
    # This code is somewhat fragile. Beware.
    plot.new()
    eval_title <- "Accumulation curves for all solutions in batch"
    eval_ylab <- "cumulative count of features represented"
    eval_xlab <- "zones protected"
    plot(evs$cumsum_1 ~ evs$e+1, pch = 1, ylim = c(0,length(features)*1.1), main = eval_title , xlab = eval_xlab, ylab = eval_ylab)
    cols <- c("pink1", "violet", "mediumpurple1", "slateblue1", "purple", "purple3",
              "turquoise2", "skyblue", "steelblue", "blue2", "navyblue",
              "orange", "tomato", "coral2", "palevioletred", "violetred", "red2",
              "springgreen2", "yellowgreen", "palegreen4",
              "wheat2", "tan", "tan2", "tan3", "brown",
              "grey70", "grey50", "grey30")
    eval_col <- c("black")
    eval_sol <- c(names(result[["summary"]])[2])
    for(s in 2:solutions){
      eval_col <- c(eval_col, sample(cols,1))
      eval_sol <- c(eval_sol, names(result[["summary"]])[s+1])
      points(evs[[paste0("cumsum_",s)]] ~ evs$e+1, pch = 1, col = eval_col[s])
    }
    legend("right", legend = eval_sol, col = eval_col, pch = rep(1,length(eval_col)), cex=1.5)
  }else{
    plot_eval(result, s = 1)
  }
}

plot_eval_map <- function(result, path_sf_zones, s){
  nad16n <- "+proj=utm +zone=16 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" #temporary
  solutions <- result[["settings"]]$solutions
  field_zone <- result[["settings"]]$field_zone
  features <- result[["settings"]]$features
  #zones <- length(result[["settings"]]$zones)
  zones <- nrow(result[["settings"]]$df) #temporary
  
  # Map
  main <- names(result[["summary"]])[s+1]
  ranks <- result[["summary"]][c(1,s+1)]
  huc12 <- sf::st_read(path_sf_zones, quiet = TRUE) #temporary
  huc12 <- st_transform(huc12, crs = st_crs(nad16n)) #temporary
  huc12_result <- merge(huc12, ranks, by = field_zone, all.x = TRUE)
  plot(huc12_result[main], main = main)
  
  # Eval
  e <- result[["evals"]][[s]]
  etab <- as.data.frame(table(e))
  etab[paste0("cumsum_", s)] <- cumsum(etab$Freq)
  etab <- sapply(etab, function(x) as.numeric(paste(x)))
  evs <- as.data.frame(etab)
  eval_title <- paste("Accumulation curve for", main)
  eval_ylab <- "cumulative count of features represented"
  eval_xlab <- "zones protected"
  eval_xlim <- c(0, zones)
  eval_ylim <- c(0,length(features)+(0.1*length(features)))
  plot(evs[[paste0("cumsum_", s)]] ~ evs$e+1, pch = 1,ylim = eval_ylim, xlim = eval_xlim, main = eval_title , xlab = eval_xlab, ylab = eval_ylab)
  #legend("right", legend = main, col = "black", pch = 1, cex=0.8)
}

prioritization_shiny <- function(solutions, df, features, algorithm, field_zone, field_value, useCost, field_cost, useFeatureWeight, weight, useProtectedAreas, protectedAreas, useAnnealing, temperature_start, cooling_rate, evalThreshold){
  
  
  ## -------------------------
  ## Check for valid arguments
  ## -------------------------
  
  
  # Solutions
  if (missing(solutions)){
    solutions = 1
  }
  message("Identifying ", solutions, " prioritization solutions.")
  # Algorithm
  if(algorithm != "CAZ" && algorithm != "ABF"){
    stop("Algorithm must be 'ABF' or 'CAZ'.")
  }
  message("Algorithm specified: ", algorithm)
  # Features
  if(missing(features) | length(features) < 1){
    stop("No features specified.")
  }
  message("Features specified: ", length(features))
  # Cost
  if(useCost == FALSE | missing(field_cost) | field_cost == "None"){
    print("applying constant cost")
    #df$TEMP_cost <- 1
    #field_cost <- "TEMP_cost"
    message("Cost specified: None")
  }else{
    message("Cost specified: ", field_cost)
  }
  # Weight
  if(useFeatureWeight == FALSE | missing(weight)){
    weight <- 1
    message("Feature weight specified: None")
  }
  # Protected areas
  if(missing(protectedAreas) | length(protectedAreas) < 1){ #temporary?
  #if(useProtectedAreas == FALSE | length(protectedAreas) < 1){
    protectedAreas <- c()
  }
  message("Protected areas specified: ", length(protectedAreas))
  # Simulated annealing
  if((useAnnealing == TRUE & missing(temperature_start)) | (useAnnealing == TRUE & missing(cooling_rate))){
    stop("Annealing parameters were not specified correctly.")
  }else{
    message("Using simulated annealing:")
    message("  Starting temperature is ", temperature_start)
    message("  Cooling rate is ", cooling_rate)
  }
  # Evaluation
  if(missing(evalThreshold)){
    evalThreshold = 1
  }
  message("Evaluation threshold: 1")
  
  
  ## --------------------
  ## Define output object
  ## --------------------
  
  
  result <- list() 
  sol <- list()
  eval <- list()
  sett <- list(
    solutions = solutions,
    df = df,
    features = features,
    field_zone = field_zone,
    field_value = field_value,
    field_cost = field_cost,
    weight = weight,
    protectedAreas = protectedAreas,
    algorithm =  algorithm,
    useAnnealing = useAnnealing,
    temperature_start = temperature_start,
    cooling_rate = cooling_rate,
    evalThreshold = evalThreshold)
  
  
  ## ---------------------
  ## Calculate n solutions
  ## ---------------------
  
  
  for(j in 1:solutions){
    start_solution_time <- Sys.time()
    # Get solution name
    solution <- paste0("rank", "_", field_value, "_", algorithm)
    # Iterate as many times as there are zones ie one zone dropped per iteration.
    zones <- unique(paste(df[[field_zone]]))
    #print(paste0("Total number of potential solutions is: ", factorial(zones)/2)) # BEWARE
    out <- data.frame(zones); colnames(out) <- field_zone # output dataframe
    drops <- c() # accumulate zone names that are dropped after each iteration
    sett[["zones"]] <- zones

    # Shiny progress bar
    withProgress(message = paste("Solution", j), min = 0, max = length(zones), value = 0, { 
  
    
    ## ----------------------------------
    ## For each solution, iterate i times
    ## ----------------------------------
    
    
    for(i in 1:3){ # debug
    #for(i in 1:length(zones)){
      # In shiny ap, report iterations as progress bar
      incProgress(amount = 1, detail = paste("Remaining zones:", length(zones) - i)) # Shiny
      remaining_zones <- setdiff(zones, drops)
      message(paste0("Remaining zones: ", length(remaining_zones)))
      if(length(remaining_zones) == 0){ #deprecated?
        message("Note: Full number of iterations not required to identify highest priority zone.")
        next()
      }
      # For each iteration, recalculate Di for all remaining zones.
      df2 <- df[df[[field_zone]] %in% c(remaining_zones, protectedAreas),] # complementary
      drop <- c()
      
      ## -----------------------------
      ## For each zone, calculate a Di
      ## -----------------------------
      
      for(zone in remaining_zones){
        delta_spp <- 0
        for(sp in features){
          # Qj
          total_sp <- sum(df2[,paste0(sp, "_", field_value)], na.rm = TRUE)
          # Qij
          zone_sp <- df2[df2[field_zone] == zone, paste0(sp, "_", field_value)]
          zone_sp <- ifelse(is.na(zone_sp), 0, zone_sp)
          # Define CAZ as Vj =  Max [Qij / Qj * Wj]
          if(algorithm == "CAZ"){
            delta_sp_w <- caz(total_sp = total_sp, zone_sp = zone_sp, weight = weight)
            delta_spp <- ifelse(delta_sp_w > delta_spp, delta_sp_w, delta_spp)
            # Define ABF as Vj =  Sum [(Qj - Qij) * Wj]
          }else if(algorithm == "ABF"){
            delta_sp_w <- abf(total_sp = total_sp, zone_sp = zone_sp, weight = weight)
            delta_spp <- delta_spp + delta_sp_w
          }
        }
        # Di = Vj * 1 / Ci
        if(missing(field_cost)){
          cost_zone <- 1
        }else{
          cost_zone <- df2[df2[[field_zone]] == zone, field_cost]
        }
        out[out[[field_zone]] == zone, paste0("Di_", i)] <- delta_spp / cost_zone
      }
      
      
      ## ---------------------------
      ## Drop zone with lowest value
      ## ---------------------------
      
      
      out[[field_zone]] <- as.character(out[[field_zone]])
      # Calculate absolute minimum.
      min <- min(out[paste0("Di_", i)], na.rm = TRUE)
      print(paste("absolute minimum:", min))
      # Calculate minimum based on simulated annealing.
      if(useAnnealing == TRUE){
        min2 <- min * (1 + cool_curve(temperature_start, cooling_rate, i))
        print(paste("annealed minimum:", min2))
        drop <- out[out[[paste0("Di_", i)]] <= min2, field_zone]
        drop <- drop[!is.na(drop)]
        print(drop)
      }else{
        drop <- out[out[[paste0("Di_", i)]] == min, field_zone]
      }
      # If multiple zones have the lowest Di, randomly select one zone to drop.
      if(length(drop) > 1){
        print("Note: Multible zones have the lowest Di; Randomly selecting one to remove.")
        drop <- drop[!is.na(drop)]
        drop <- sample(drop, 1)
      }
      # Order of zone removal is a zones priority rank (lower rank is better).
      rank <- (length(zones) + 1) - i
      print(paste("Zone rank:", rank))
      out[out[[field_zone]] %in% drop, solution] <- rank
      drops <- c(drops, drop)
      
      
      ## ---------------------------------------
      ## Save and evaluate results of a solution
      ## ---------------------------------------
      
      
      # Save detailed results of a solution.
      sol[[j]] <- out 
      
      # Evaluate a solution.
      eval[[j]] <- evaluate_prioritization(
        solution_name = solution,
        field_zone = field_zone,
        field_value = field_value,
        df_original = df,
        df_result = out[,c(field_zone, solution)],
        evalThreshold = evalThreshold)
      
      print(paste("Solution time:", Sys.time()-start_solution_time))
      
      }
    })
  }
  
  
  ## --------------------------------------------
  ## Save and return the results of all solutions
  ## --------------------------------------------
  
  
  # Create a summary data frame that just includes zones and
  # ranks from each solution.
  for(k in 1:length(sol)){
    s <- sol[[k]][,grep(pattern = paste0("^rank|", field_zone), x = names(sol[[k]]))]
    colnames(s) <- c(field_zone, paste0(solution, "_", k))
    if(k == 1){
      summ <- s
    }else{
      summ <- merge(summ, s, by = field_zone) 
    }
  }
  
  result[["summary"]] <- summ
  result[["solutions"]] <- sol
  result[["evals"]] <- eval
  result[["settings"]] <- sett
  print("return result")
  return(result)
  
} #function