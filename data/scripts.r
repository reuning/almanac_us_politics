library(tidyverse)

subgroup_analysis <- 
    function(data, variable = NULL, var_name = NULL,
            var_levels=NULL, levels=NULL,
            group_var=NULL, group_var_labels=NULL, 
            group_var_levels=NULL, 
            group_var_name=NULL, middle_value=NULL, 
            calc_mean=FALSE){

    if(is.null(group_var)){
        p_data <- data |> 
            rename(var=!!variable) |>
            group_by(VCF0004,var) |>  
            summarize(n=sum(VCF0009z)) |> 
            group_by(VCF0004) |>
            drop_na(var) |> 
            filter(var %in% levels) |> 
            mutate(total=sum(n), 
                Percentage=(n/total)*100, 
                var_name =factor(var, levels, 
                labels=var_levels)) |>
                rename("Year"="VCF0004") 
    } else {
        p_data <- data |> 
            rename(var=!!variable) |>
            group_by(VCF0004,var, !!sym(group_var)) |>  
            summarize(n=sum(VCF0009z)) |> 
            group_by(VCF0004, !!sym(group_var)) |>
            drop_na(var) |> 
            filter(var %in% levels) |> 
            rename(group=!!group_var) |> 
            mutate(total=sum(n), 
                !!group_var_name:=factor(group, levels=group_var_levels, 
                    labels=group_var_labels), 
                Percentage=(n/total)*100, 
                var_name =factor(var, levels, 
                labels=var_levels)) |>
                rename("Year"="VCF0004") 
    }

    if(is.null(middle_value)){
        p_data <- p_data |> rename(!!sym(var_name) := var_name)
    } else {
        p_data <- p_data |> mutate(!!sym(var_name) := forcats::fct_relevel(var_name, 
          "Don't Know", after=trunc(length(var_levels)/2))) |> 
          select(-var_name)

    }
    if(calc_mean==TRUE){
      if(is.null(group_var)){
       p_data <- p_data |> 
        group_by(Year) |> 
        summarize(Average= sum(n * as.numeric(!!sym(var_name)))/sum(n), 
                    total=sum(n), 
                    `Standard Deviation` = sqrt(sum( n * ((as.numeric(!!sym(var_name)) - Average)^2) / (sum(n)-1)) ), 
                    `Standard Error` = `Standard Deviation`/sqrt(sum(n)))
      } else {
        p_data <- p_data |> 
        group_by(Year, group, !!sym(group_var_name)) |> 
        summarize(Average= sum(n * as.numeric(!!sym(var_name)))/sum(n), 
                    total=sum(n), 
                    `Standard Deviation` = sqrt(sum( n * ((as.numeric(!!sym(var_name)) - Average)^2) / (sum(n)-1)) ), 
                    `Standard Error` = `Standard Deviation`/sqrt(sum(n)))
      }


    } else {
      p_data <- p_data |> mutate(
        `Standard Deviation` = sqrt((1-Percentage/100)*(Percentage/100))*100,
        `Standard Error` = `Standard Deviation`/sqrt(total)
      )
    }
    return(p_data)

}


facet_strip_bigger <- function(gp, box_expansion=35, spread=.5){

  # n_facets should be the number of facets x2
  n_facets <- length(gp[["x"]][["layout"]][["shapes"]])
  y_levels <- sort(c(gp[["x"]][["layout"]][["yaxis"]][["domain"]], 
                gp[["x"]][["layout"]][["yaxis2"]][["domain"]]))

  y_delta <- (y_levels[3] - y_levels[2])*spread
  y_levels[2] <- y_levels[2] - y_delta 
  y_levels[3] <- y_levels[3] + y_delta 

  gp[["x"]][["layout"]][["yaxis"]][["domain"]] <- y_levels[1:2]
  gp[["x"]][["layout"]][["yaxis"]][["domain"]] <- y_levels[3:4]

  for(ii in 1:n_facets){

    if(ii %% 2 == 0){
      gp[["x"]][["layout"]][["shapes"]][[ii]][["y1"]] <- y_levels[3] + box_expansion
      
    }
  }
  
  return(gp)
}