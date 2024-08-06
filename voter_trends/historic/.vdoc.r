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

source(here::here("data", "scripts.r"))



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
#


levels <- c(1:7, 9)
variable <- "VCF0811"
var_levels <- c("1 - Solve poverty", "2", "3", "4", "5", "6", 
                "7 - Use all force", "Don't Know")
var_name <- "Urban Unrest"



all_df <- subgroup_analysis(data=anes_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, calc_mean=TRUE)

all_df <- all_df |> 
        mutate(`95% Lower`=Average - 1.96 * `Standard Error`, 
                 `95% Upper`=Average + 1.96 * `Standard Error`)
plot_data <- all_df |> 
            filter(total > 50) 

p <- ggplot(plot_data, 
        aes(x=Year, y=Average, 
            ymin=`95% Lower`,
            ymax=`95% Upper`)) + 
        geom_linerange(position=position_dodge(width=1),
            color=alpha("gray", 0.5)) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        scale_color_viridis_d(option="G", begin=0.1, end=0.9) + 
        labs(y="Average", x="") + 
        theme_pubr() + 
        theme(axis.text.x = element_text(angle=90, vjust=.5), legend.position="bottom")
        
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
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    mutate("Sub Group"="All") |> 
    rename("Total"=total) 


#
#
#
#
#
#
#




all_df <- subgroup_analysis(data=anes_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels)
    
p <- ggplot(all_df, aes(x=Year, y=Percentage, 
        fill=!!sym(var_name))) + 
        geom_col(color="black") + 
        geom_hline(yintercept=50, linetype="dashed", color="grey70") + 
        geom_hline(yintercept=c(25,75), linetype="dotted",  color="grey70") + 
        scale_fill_viridis_d(option="H") + 
        # scale_y_continuous(labels=scales::label_percent()) +
        labs(y="Percentage", x="") + 
        theme_pubr() 
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
group_var_name <- "Education"
group_var <- "VCF0110"
group_var_levels <- 1:4
group_var_labels <- c("Less than High School", "High School Degree", 
        "Some College", "College Degree")
sub_df <- subgroup_analysis(data=anes_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, group_var=group_var, 
                group_var_name=group_var_name,
                group_var_levels=group_var_levels, 
                group_var_labels=group_var_labels, calc_mean=TRUE)

sub_df <- sub_df |> 
        mutate(`95% Lower`=Average - 1.96 * `Standard Error`, 
                 `95% Upper`=Average + 1.96 * `Standard Error`)
plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50) 

p <- ggplot(plot_data, 
        aes(x=Year, y=Average, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
            color=!!sym(group_var_name))) + 
        geom_linerange(position=position_dodge(width=1),
            color=alpha("gray", 0.5),
            aes(group=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        scale_color_viridis_d(option="G", begin=0.1, end=0.9) + 
        labs(y="Average", x="") + 
        theme_pubr() + theme(axis.text.x = element_text(angle=90, vjust=.5),legend.position="bottom")
        
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
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-group) |> 
    rename("Total"=total, 
            "Sub Group"=!!sym(group_var_name)) 

#
#
#
#
#
#
#



group_var_name <- "Gender"
group_var="VCF0104"
group_var_levels=1:2
group_var_labels=c("Male", "Female")

sub_df <- subgroup_analysis(data=anes_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, group_var=group_var, 
                group_var_name=group_var_name,
                group_var_levels=group_var_levels, 
                group_var_labels=group_var_labels, calc_mean=TRUE)

sub_df <- sub_df |> 
        mutate(`95% Lower`=Average - 1.96 * `Standard Error`, 
                 `95% Upper`=Average + 1.96 * `Standard Error`)
plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50) 

p <- ggplot(plot_data, 
        aes(x=Year, y=Average, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
            color=!!sym(group_var_name))) + 
        geom_linerange(position=position_dodge(width=1),
            color=alpha("gray", 0.5),
            aes(group=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        scale_color_viridis_d(option="G", begin=0.1, end=0.9) + 
        labs(y="Average", x="") + 
        theme_pubr() + theme(axis.text.x = element_text(angle=90, vjust=.5),legend.position="bottom")
        
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
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-group) |> 
    rename("Total"=total, 
            "Sub Group"=!!sym(group_var_name)) |> 
    rbind(all_df)

```
#
#
#
#
#
#
group_var_name <- "Income"
group_var <- "VCF0114"
group_var_levels <- 1:5
group_var_labels <- c("0-16 Percentile", "17-33 Percentile",
                "34-67 Percentile", "68-95 Percentile",
                "96-100 Percentile")
sub_df <- subgroup_analysis(data=anes_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, group_var=group_var, 
                group_var_name=group_var_name,
                group_var_levels=group_var_levels, 
                group_var_labels=group_var_labels, calc_mean=TRUE)

sub_df <- sub_df |> 
        mutate(`95% Lower`=Average - 1.96 * `Standard Error`, 
                 `95% Upper`=Average + 1.96 * `Standard Error`)
plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50) 

p <- ggplot(plot_data, 
        aes(x=Year, y=Average, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
            color=!!sym(group_var_name))) + 
        geom_linerange(position=position_dodge(width=1),
            color=alpha("gray", 0.5),
            aes(group=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        scale_color_viridis_d(option="G", begin=0.1, end=0.9) + 
        labs(y="Average", x="") + 
        theme_pubr() + theme(axis.text.x = element_text(angle=90, vjust=.5), legend.position="bottom")
        
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
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-group) |> 
    rename("Total"=total, 
            "Sub Group"=!!sym(group_var_name)) |> 
    rbind(all_df)

#
#
#
#
#
#
#
#
group_var_name <- "Partisanship"
group_var <- "VCF0303"
group_var_levels <- 1:3
group_var_labels <- c("Democrats", "Independents", "Republicans")

tmp_df <- anes_df |> 
    mutate(VCF0148a = 
        case_match(VCF0148a, 
            1:3 ~ 1, 
            4:6 ~ 2, 
            .default=NA))
            
sub_df <- subgroup_analysis(data=tmp_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, group_var=group_var, 
                group_var_name=group_var_name,
                group_var_levels=group_var_levels, 
                group_var_labels=group_var_labels, calc_mean=TRUE)

sub_df <- sub_df |> 
        mutate(`95% Lower`=Average - 1.96 * `Standard Error`, 
                 `95% Upper`=Average + 1.96 * `Standard Error`)
plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50) 

p <- ggplot(plot_data, 
        aes(x=Year, y=Average, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
            color=!!sym(group_var_name))) + 
        geom_linerange(position=position_dodge(width=1),
            color=alpha("gray", 0.5),
            aes(group=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        scale_color_viridis_d(option="G", begin=0.1, end=0.9) + 
        labs(y="Average", x="") + 
        theme_pubr() + theme(axis.text.x = element_text(angle=90, vjust=.5),legend.position="bottom")
        
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
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-group) |> 
    rename("Total"=total, 
            "Sub Group"=!!sym(group_var_name)) |> 
    rbind(all_df)
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
group_var_name <- "Race"
group_var <- "VCF0105b"
group_var_levels <- 1:4
group_var_labels <- c("White, non-Hispanic", "Black, non-Hispanic", 
        "Hispanic", "Other, non-Hispanic")
sub_df <- subgroup_analysis(data=anes_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, group_var=group_var, 
                group_var_name=group_var_name,
                group_var_levels=group_var_levels, 
                group_var_labels=group_var_labels, calc_mean=TRUE)

sub_df <- sub_df |> 
        mutate(`95% Lower`=Average - 1.96 * `Standard Error`, 
                 `95% Upper`=Average + 1.96 * `Standard Error`)
plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50) 

p <- ggplot(plot_data, 
        aes(x=Year, y=Average, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
            color=!!sym(group_var_name))) + 
        geom_linerange(position=position_dodge(width=1),
            color=alpha("gray", 0.5),
            aes(group=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        scale_color_viridis_d(option="G", begin=0.1, end=0.9) + 
        labs(y="Average", x="") + 
        theme_pubr() + theme(axis.text.x = element_text(angle=90, vjust=.5), legend.position="bottom")
        
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
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-group) |> 
    rename("Total"=total, 
            "Sub Group"=!!sym(group_var_name)) |> 
    rbind(all_df)

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
group_var_name <- "Class"
group_var <- "VCF0148a"
group_var_levels <- 1:2
group_var_labels <- c("Working Class", "Middle Class")

tmp_df <- anes_df |> 
    mutate(VCF0148a = 
        case_match(VCF0148a, 
            1:3 ~ 1, 
            4:6 ~ 2, 
            .default=NA))

sub_df <- subgroup_analysis(data=tmp_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, group_var=group_var, 
                group_var_name=group_var_name,
                group_var_levels=group_var_levels, 
                group_var_labels=group_var_labels, calc_mean=TRUE)

sub_df <- sub_df |> 
        mutate(`95% Lower`=Average - 1.96 * `Standard Error`, 
                 `95% Upper`=Average + 1.96 * `Standard Error`)
plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50) 

p <- ggplot(plot_data, 
        aes(x=Year, y=Average, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
            color=!!sym(group_var_name))) + 
        geom_linerange(position=position_dodge(width=1),
            color=alpha("gray", 0.5),
            aes(group=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        scale_color_viridis_d(option="G", begin=0.1, end=0.9) + 
        labs(y="Average", x="") + 
        theme_pubr() + theme(axis.text.x = element_text(angle=90, vjust=.5),legend.position="bottom")
        
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
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-group) |> 
    rename("Total"=total, 
            "Sub Group"=!!sym(group_var_name)) |> 
    rbind(all_df)

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
group_var_name <- "Union"
group_var <- "VCF0127"
group_var_levels <- 1:2
group_var_labels <-c("Union Household", "Non-Union")
sub_df <- subgroup_analysis(data=anes_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, group_var=group_var, 
                group_var_name=group_var_name,
                group_var_levels=group_var_levels, 
                group_var_labels=group_var_labels, calc_mean=TRUE)

sub_df <- sub_df |> 
        mutate(`95% Lower`=Average - 1.96 * `Standard Error`, 
                 `95% Upper`=Average + 1.96 * `Standard Error`)
plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50) 

p <- ggplot(plot_data, 
        aes(x=Year, y=Average, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
            color=!!sym(group_var_name))) + 
        geom_linerange(position=position_dodge(width=1),
            color=alpha("gray", 0.5),
            aes(group=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        scale_color_viridis_d(option="G", begin=0.1, end=0.9) + 
        labs(y="Average", x="") + 
        theme_pubr() + theme(axis.text.x = element_text(angle=90, vjust=.5), legend.position="bottom")
        
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
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-group) |> 
    rename("Total"=total, 
            "Sub Group"=!!sym(group_var_name)) |> 
    rbind(all_df)

#
#
#
#
#
#
#
#

write_csv(file=here::here("voter_trends", "data", paste0("opinion_",var_name, ".csv")), all_df)

#
#
#
#
#
#
#
#
#
