#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(tidyverse)
library(ggpubr)
library(plotly)
library(gt)
options(digits=3)

anes_df <- read_csv(here::here("data", "anes_timeseries_cdf_csv_20220916.csv"))

pal <- colorRampPalette(c("orangered3", "gray","darkblue"))
pal <- rev(c(pal(7)[1:3],"springgreen4", pal(7)[4:7]))

p <- anes_df |> 
    filter(VCF0803 %in% c(1:7,9)) |> 
    #filter(VCF0127b %in% c(1, 3) & VCF0803 > 0) |>
    group_by(VCF0004, VCF0803) |>  
    summarize(n=sum(VCF0009z)) |> 
    group_by(VCF0004) |>
    mutate(total=sum(n), 
        Percentage=n/total*100, 
        Party=factor(VCF0803, c(1:3,4,9,5:7), 
        labels=c("Ext Lib", "Liberal", "Slightly Lib", "Moderate", "Don't Know", "Slightly Con", "Conservative", "Ext Con"))) |> 
    rename("Year"="VCF0004") |>
    ggplot(aes(x=Year, y=Percentage, 
    fill=Party)) + 
    geom_col() + 
    geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
    geom_hline(yintercept=c(25, 75), linetype="dotted", color="grey70") + 
    scale_fill_manual("Party ID", values=pal) + 
    labs(y="Percentage", x="") + 
    theme_pubr() +
    theme(legend.position="bottom")
        
#
#
#
#
p
#
#
#
#
#
#
#
#

p <- ggplotly(p)  |> 
    layout(legend = list(
        orientation = "h"
        )
    ) 
p
#
#
#
#
#
#
#
#
#

levels <- 1:4
variable <- "VCF0110"
var_levels <- c("Less than High School", "High School Degree", 
        "Some College", "College Degree")
var_name <- "Education"
p <- list()

p_data <- anes_df |> 
    filter(VCF0803 %in% c(1:7,9)) |> 
    rename(var=!!variable) |>
    group_by(VCF0004, VCF0803,var) |>  
    summarize(n=sum(VCF0009z)) |> 
    group_by(VCF0004, var) |>
    mutate(total=sum(n), 
        Percentage=n/total*100, 
        Party=factor(VCF0803, c(1:3,4,9,5:7), 
        labels=c("Ext Lib", "Liberal", "Slightly Lib", "Moderate", "Don't Know", "Slightly Con", "Conservative", "Ext Con"))) |>
    rename("Year"="VCF0004")

for(ii in seq_along(levels)){
    p[[ii]] <- p_data |> 
        filter(total > 50) |>
        filter(var==ii) |>
        ggplot(aes(x=Year, y=Percentage, 
        fill=Party)) + 
        geom_col() + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_manual("Party ID", values=pal) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")

}

#
#
#
#
#
#
#
#
p[[1]]
#
#
#
#
#
#
#
#

ggplotly(p[[1]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
p[[2]]
#
#
#
#
#
#
#
#

ggplotly(p[[2]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
#
p[[3]]
#
#
#
#
#
#
#
#

ggplotly(p[[3]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
p[[4]]
#
#
#
#
#
#
#
#

ggplotly(p[[4]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#

p <- p_data |> filter(VCF0803==1) |> 
    group_by(Year) |> 
    mutate(Percentage= total/sum(total)*100, 
        !!var_name:=factor(var, labels=var_levels, levels=levels)) |> 
    ggplot(aes(x=Year, y=Percentage, 
        fill=!!sym(var_name))) + 
        geom_col(color="gray", linewidth=.5) + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_viridis_d(option="H", begin=.05, end=.95) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + 
        theme(legend.position="bottom")
#
#
#
#
#
p + guides(fill=guide_legend(nrow=2))
#
#
#
#
#
#
#
#

ggplotly(p) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#

data_out <- p_data |> ungroup() |>
    select(Year, var, Party, Percentage, total, n) |> 
    mutate(var=factor(var, levels, 
        labels=var_levels)) |> 
    rename("Group N"=total, "N"=n) |> 
    drop_na(var) |> 
    arrange(Year, var, Party) |> 
    rename(!!var_name:=var) 

data_out |> 
    gt() |> 
    fmt_integer(columns=c("N", "Group N")) |> 
    fmt_number(columns=Percentage) |>
    opt_interactive(use_filters=TRUE, use_compact_mode=TRUE)

write_csv(file=here::here("voter_trends", "data", paste0("ideo_",var_name, ".csv")), data_out)

#
#
#
#
#
#
#
#
#
#
#
#
#
#

levels <- 1:2
variable <- "VCF0104"
var_levels <- c("Male", "Female")
var_name <- "Gender"
p <- list()

p_data <- anes_df |> 
    filter(VCF0803 %in% c(1:7,9)) |> 
    rename(var=!!variable) |>
    group_by(VCF0004, VCF0803,var) |>  
    summarize(n=sum(VCF0009z)) |> 
    group_by(VCF0004, var) |>
    mutate(total=sum(n), 
        Percentage=n/total*100, 
        Party=factor(VCF0803, c(1:3,4,9,5:7), 
        labels=c("Ext Lib", "Liberal", "Slightly Lib", "Moderate", "Don't Know", "Slightly Con", "Conservative", "Ext Con"))) |>
    rename("Year"="VCF0004")

for(ii in seq_along(levels)){
    p[[ii]] <- p_data |> 
        filter(total > 50) |>
        filter(var==ii) |>
        ggplot(aes(x=Year, y=Percentage, 
        fill=Party)) + 
        geom_col() + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_manual("Party ID", values=pal) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")

}

#
#
#
#
#
#
#
p[[1]]
#
#
#
#
#
#
#
#

ggplotly(p[[1]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
p[[2]]
#
#
#
#
#
#
#
#

ggplotly(p[[2]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#

p <- p_data |> filter(VCF0803==1) |> 
    group_by(Year) |> 
    mutate(Percentage= total/sum(total)*100, 
        !!var_name:=factor(var, labels=var_levels, levels=levels)) |> 
    ggplot(aes(x=Year, y=Percentage, 
        fill=!!sym(var_name))) + 
        geom_col(color="gray", linewidth=.5) + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_viridis_d(option="H", begin=.05, end=.95) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")
#
#
#
#
#
p + guides(fill=guide_legend(nrow=2))
#
#
#
#
#
#
#
#

ggplotly(p) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#


data_out <- p_data |> ungroup() |>
    select(Year, var, Party, Percentage, total, n) |> 
    mutate(var=factor(var, levels, 
        labels=var_levels)) |> 
    rename("Group N"=total, "N"=n) |> 
    drop_na(var) |> 
    arrange(Year, var, Party) |> 
    rename(!!var_name:=var) 

data_out |> 
    gt() |> 
    fmt_integer(columns=c("N", "Group N")) |> 
    fmt_number(columns=Percentage) |>
    opt_interactive(use_filters=TRUE, use_compact_mode=TRUE)

write_csv(file=here::here("voter_trends", "data", paste0("ideo_",var_name, ".csv")), data_out)

#
#
#
#
#
#
#
#
#
#
#
#
#
#

levels <- 1:5
variable <- "VCF0114"
var_levels <- c("0-16 Percentile", "17-33 Percentile",
                "34-67 Percentile", "68-95 Percentile",
                "96-100 Percentile")
var_name <- "Income"
p <- list()

p_data <- anes_df |> 
    filter(VCF0803 %in% c(1:7,9)) |> 
    rename(var=!!variable) |>
    group_by(VCF0004, VCF0803,var) |>  
    summarize(n=sum(VCF0009z)) |> 
    group_by(VCF0004, var) |>
    mutate(total=sum(n), 
        Percentage=n/total*100, 
        Party=factor(VCF0803, c(1:3,4,9,5:7), 
        labels=c("Ext Lib", "Liberal", "Slightly Lib", "Moderate", "Don't Know", "Slightly Con", "Conservative", "Ext Con"))) |>
    rename("Year"="VCF0004")

for(ii in seq_along(levels)){
    p[[ii]] <- p_data |> 
        filter(total > 50) |>
        filter(var==ii) |>
        ggplot(aes(x=Year, y=Percentage, 
        fill=Party)) + 
        geom_col() + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_manual("Party ID", values=pal) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")

}

#
#
#
#
#
#
#
#
p[[1]]
#
#
#
#
#
#
#
#

ggplotly(p[[1]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
p[[2]]
#
#
#
#
#
#
#
#

ggplotly(p[[2]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
p[[3]]
#
#
#
#
#
#
#
#

ggplotly(p[[3]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
p[[4]]
#
#
#
#
#
#
#
#

ggplotly(p[[4]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
#
p[[5]]
#
#
#
#
#
#
#
#

ggplotly(p[[5]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#

p <- p_data |> filter(VCF0803==1) |> 
    group_by(Year) |> 
    mutate(Percentage= total/sum(total)*100, 
        !!var_name:=factor(var, labels=var_levels, levels=levels)) |> 
    ggplot(aes(x=Year, y=Percentage, 
        fill=!!sym(var_name))) + 
        geom_col(color="gray", linewidth=.5) + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_viridis_d(option="H", begin=.05, end=.95) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")
#
#
#
#
#
p + guides(fill=guide_legend(nrow=2))
#
#
#
#
#
#
#
#

ggplotly(p) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
#


data_out <- p_data |> ungroup() |>
    select(Year, var, Party, Percentage, total, n) |> 
    mutate(var=factor(var, levels, 
        labels=var_levels)) |> 
    rename("Group N"=total, "N"=n) |> 
    drop_na(var) |> 
    arrange(Year, var, Party) |> 
    rename(!!var_name:=var) 

data_out |> 
    gt() |> 
    fmt_integer(columns=c("N", "Group N")) |> 
    fmt_number(columns=Percentage) |>
    opt_interactive(use_filters=TRUE, use_compact_mode=TRUE)

write_csv(file=here::here("voter_trends", "data", paste0("ideo_",var_name, ".csv")), data_out)

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

levels <- 1:4
variable <- "VCF0105b"
var_levels <- c("White, non-Hispanic", "Black, non-Hispanic", 
        "Hispanic", "Other, non-Hispanic")
var_name <- "Race"
p <- list()

p_data <- anes_df |> 
    filter(VCF0803 %in% c(1:7,9)) |> 
    rename(var=!!variable) |>
    group_by(VCF0004, VCF0803,var) |>  
    summarize(n=sum(VCF0009z)) |> 
    group_by(VCF0004, var) |>
    mutate(total=sum(n), 
        Percentage=n/total*100, 
        Party=factor(VCF0803, c(1:3,4,9,5:7), 
        labels=c("Ext Lib", "Liberal", "Slightly Lib", "Moderate", "Don't Know", "Slightly Con", "Conservative", "Ext Con"))) |>
    rename("Year"="VCF0004")

for(ii in seq_along(levels)){
    p[[ii]] <- p_data |> 
        filter(total > 50) |> 
        filter(var==ii) |>
        ggplot(aes(x=Year, y=Percentage, 
        fill=Party)) + 
        geom_col() + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_manual("Party ID", values=pal) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")

}

#
#
#
#
#
#
#
#
p[[1]]
#
#
#
#
#
#
#
#

ggplotly(p[[1]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
#
p[[2]]
#
#
#
#
#
#
#
#

ggplotly(p[[2]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
p[[3]]
#
#
#
#
#
#
#
#

ggplotly(p[[3]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
#
p[[4]]
#
#
#
#
#
#
#
#

ggplotly(p[[4]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#

p <- p_data |> filter(VCF0803==1) |> 
    group_by(Year) |> 
    mutate(Percentage= total/sum(total)*100, 
        !!var_name:=factor(var, labels=var_levels, levels=levels)) |> 
    ggplot(aes(x=Year, y=Percentage, 
        fill=!!sym(var_name))) + 
        geom_col(color="gray", linewidth=.5) + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_viridis_d(option="H", begin=.05, end=.95) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")
#
#
#
#
#
p + guides(fill=guide_legend(nrow=2))
#
#
#
#
#
#
#
#

ggplotly(p) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
#


data_out <- p_data |> ungroup() |>
    select(Year, var, Party, Percentage, total, n) |> 
    mutate(var=factor(var, levels, 
        labels=var_levels)) |> 
    rename("Group N"=total, "N"=n) |> 
    drop_na(var) |> 
    arrange(Year, var, Party) |> 
    rename(!!var_name:=var) 

data_out |> 
    gt() |> 
    fmt_integer(columns=c("N", "Group N")) |> 
    fmt_number(columns=Percentage) |>
    opt_interactive(use_filters=TRUE, use_compact_mode=TRUE)

write_csv(file=here::here("voter_trends", "data", paste0("ideo_",var_name, ".csv")), data_out)

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#


levels <- 1:2
variable <- "VCF0148a"
var_levels <- c("Working Class", "Middle Class")
var_name <- "Class"
p <- list()

p_data <- anes_df |> 
    mutate(VCF0148a = 
        case_match(VCF0148a, 
            1:3 ~ 1, 
            4:6 ~ 2, 
            .default=NA)) |> 
    filter(VCF0803 %in% c(1:7,9)) |> 
    rename(var=!!variable) |>
    group_by(VCF0004, VCF0803,var) |>  
    summarize(n=sum(VCF0009z)) |> 
    group_by(VCF0004, var) |>
    mutate(total=sum(n), 
        Percentage=n/total*100, 
        Party=factor(VCF0803, c(1:3,4,9,5:7), 
        labels=c("Ext Lib", "Liberal", "Slightly Lib", "Moderate", "Don't Know", "Slightly Con", "Conservative", "Ext Con"))) |>
    rename("Year"="VCF0004")

for(ii in seq_along(levels)){
    p[[ii]] <- p_data |> 
        filter(total > 50) |>
        filter(var==ii) |>
        ggplot(aes(x=Year, y=Percentage, 
        fill=Party)) + 
        geom_col() + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_manual("Party ID", values=pal) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")

}

#
#
#
#
#
#
#
p[[1]]
#
#
#
#
#
#
#
#

ggplotly(p[[1]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
p[[2]]
#
#
#
#
#
#
#
#

ggplotly(p[[2]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#

p <- p_data |> filter(VCF0803==1) |> 
    group_by(Year) |> 
    mutate(Percentage= total/sum(total)*100, 
        !!var_name:=factor(var, labels=var_levels, levels=levels)) |> 
    ggplot(aes(x=Year, y=Percentage, 
        fill=!!sym(var_name))) + 
        geom_col(color="gray", linewidth=.5) + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_viridis_d(option="H", begin=.05, end=.95) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")
#
#
#
#
#
p + guides(fill=guide_legend(nrow=2))
#
#
#
#
#
#
#
#

ggplotly(p) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
#


data_out <- p_data |> ungroup() |>
    select(Year, var, Party, Percentage, total, n) |> 
    mutate(var=factor(var, levels, 
        labels=var_levels)) |> 
    rename("Group N"=total, "N"=n) |> 
    drop_na(var) |> 
    arrange(Year, var, Party) |> 
    rename(!!var_name:=var) 

data_out |> 
    gt() |> 
    fmt_integer(columns=c("N", "Group N")) |> 
    fmt_number(columns=Percentage) |>
    opt_interactive(use_filters=TRUE, use_compact_mode=TRUE)

write_csv(file=here::here("voter_trends", "data", paste0("ideo_",var_name, ".csv")), data_out)

#
#
#
#
#
#
#
#
#
#
#
#
#
#


levels <- 1:2
variable <- "VCF0127"
var_levels <- c("Union Household", "Non-Union")
var_name <- "Class"
p <- list()

p_data <- anes_df |> 
    filter(VCF0803 %in% c(1:7,9)) |> 
    rename(var=!!variable) |>
    group_by(VCF0004, VCF0803,var) |>  
    summarize(n=sum(VCF0009z)) |> 
    group_by(VCF0004, var) |>
    mutate(total=sum(n), 
        Percentage=n/total*100, 
        Party=factor(VCF0803, c(1:3,4,9,5:7), 
        labels=c("Ext Lib", "Liberal", "Slightly Lib", "Moderate", "Don't Know", "Slightly Con", "Conservative", "Ext Con"))) |>
    rename("Year"="VCF0004")

for(ii in seq_along(levels)){
    p[[ii]] <- p_data |> 
        filter(total > 50) |>
        filter(var==ii) |>
        ggplot(aes(x=Year, y=Percentage, 
        fill=Party)) + 
        geom_col() + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_manual("Party ID", values=pal) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")

}

#
#
#
#
#
#
#
#
p[[1]]
#
#
#
#
#
#
#
#

ggplotly(p[[1]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
p[[2]]
#
#
#
#
#
#
#
#

ggplotly(p[[2]]) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
#

p <- p_data |> filter(VCF0803==1) |> 
    group_by(Year) |> 
    mutate(Percentage= total/sum(total)*100, 
        !!var_name:=factor(var, labels=var_levels, levels=levels)) |> 
    ggplot(aes(x=Year, y=Percentage, 
        fill=!!sym(var_name))) + 
        geom_col(color="gray", linewidth=.5) + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_viridis_d(option="H", begin=.05, end=.95) + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() + theme(legend.position="bottom")
#
#
#
#
#
p + guides(fill=guide_legend(nrow=2))
#
#
#
#
#
#
#
#

ggplotly(p) |> 
    layout(legend = list(
        orientation = "h"
        )
    )

#
#
#
#
#
#
#
#
#


data_out <- p_data |> ungroup() |>
    select(Year, var, Party, Percentage, total, n) |> 
    mutate(var=factor(var, levels, 
        labels=var_levels)) |> 
    rename("Group N"=total, "N"=n) |> 
    drop_na(var) |> 
    arrange(Year, var, Party) |> 
    rename(!!var_name:=var) 

data_out |> 
    gt() |> 
    fmt_integer(columns=c("N", "Group N")) |> 
    fmt_number(columns=Percentage) |>
    opt_interactive(use_filters=TRUE, use_compact_mode=TRUE)

write_csv(file=here::here("voter_trends", "data", paste0("ideo_",var_name, ".csv")), data_out)

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
