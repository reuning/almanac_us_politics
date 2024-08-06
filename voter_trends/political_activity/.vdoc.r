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


levels <- c(1:2)
variable <- "VCF0718"
var_levels <- c("No", "Yes")
var_name <- "Attend Rallies"



all_df <- subgroup_analysis(data=anes_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, middle_value="Don't Know")
    
all_df <- all_df |> 
        mutate(`95% Lower`=Percentage - 1.96 * `Standard Error`, 
                 `95% Upper`=Percentage + 1.96 * `Standard Error`) |> 
        filter(var==2)

p <- ggplot(all_df, aes(x=Year, y=Percentage, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
        color=!!sym(var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        geom_linerange(position=position_dodge(width=1), 
            alpha=.25,
            aes(group=!!sym(var_name)))  +
        scale_color_viridis_d(option="H") + 
        # scale_y_continuous(labels=scales::label_percent()) +
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

all_df <- all_df |> select(-var) |> 
    rename("Group N"=total, "N"=n ) |> 
    mutate(Group="All") |> 
    ungroup()

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
                group_var_labels=group_var_labels)

plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50)

plot_data <- plot_data |> 
        mutate(`95% Lower`=Percentage - 1.96 * `Standard Error`, 
                 `95% Upper`=Percentage + 1.96 * `Standard Error`) |> 
        filter(var==2)

p <- ggplot(plot_data, aes(x=Year, y=Percentage, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
        color=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        geom_linerange(position=position_dodge(width=1), 
            alpha=.25,
            aes(group=!!sym(group_var_name)))  +
        scale_color_viridis_d(option="H") + 
        facet_wrap(vars(!!sym(var_name)), labeller=label_wrap_gen()) +
        labs(y="Percentage", x="") + 
        theme_pubr() 

p <- ggplotly(p)  |> 
    layout(legend = list(
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-var, -group) |> 
    rename("Group N"=total, "N"=n, 
            "Group"=!!group_var_name) |> 
    rbind(all_df)
#
#
#
#
#
#



group_var_name <- "Gender"
sub_df <- subgroup_analysis(data=anes_df, 
                variable=variable,
                var_name=var_name,
                var_levels=var_levels, 
                levels=levels, group_var="VCF0104", 
                group_var_name=group_var_name,
                group_var_levels=1:2, group_var_labels=c("Male", "Female"))

plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50)

plot_data <- plot_data |> 
        mutate(`95% Lower`=Percentage - 1.96 * `Standard Error`, 
                 `95% Upper`=Percentage + 1.96 * `Standard Error`) |> 
        filter(var==2)

p <- ggplot(plot_data, aes(x=Year, y=Percentage, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
        color=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        geom_linerange(position=position_dodge(width=1), 
            alpha=.25,
            aes(group=!!sym(group_var_name)))  +
        scale_color_viridis_d(option="H") + 
        facet_wrap(vars(!!sym(var_name)), labeller=label_wrap_gen()) +
        labs(y="Percentage", x="") + 
        theme_pubr() 

p <- ggplotly(p)  |> 
    layout(legend = list(
        orientation = "h",
        y=-0.2
        )
    )
p    
all_df <- plot_data |> ungroup() |> 
    select(-var, -group) |> 
    rename("Group N"=total, "N"=n, 
            "Group"=!!group_var_name) |> 
    rbind(all_df)
#
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
                group_var_labels=group_var_labels)

plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50)


plot_data <- plot_data |> 
        mutate(`95% Lower`=Percentage - 1.96 * `Standard Error`, 
                 `95% Upper`=Percentage + 1.96 * `Standard Error`) |> 
        filter(var==2)

p <- ggplot(plot_data, aes(x=Year, y=Percentage, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
        color=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        geom_linerange(position=position_dodge(width=1), 
            alpha=.25,
            aes(group=!!sym(group_var_name)))  +
        scale_color_viridis_d(option="H") + 
        facet_wrap(vars(!!sym(var_name)), labeller=label_wrap_gen()) +
        labs(y="Percentage", x="") + 
        theme_pubr() 

p <- ggplotly(p)  |> 
    layout(legend = list(
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-var, -group) |> 
    rename("Group N"=total, "N"=n, 
            "Group"=!!group_var_name) |> 
    rbind(all_df)
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
                group_var_labels=group_var_labels)

plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50)


plot_data <- plot_data |> 
        mutate(`95% Lower`=Percentage - 1.96 * `Standard Error`, 
                 `95% Upper`=Percentage + 1.96 * `Standard Error`) |> 
        filter(var==2)

p <- ggplot(plot_data, aes(x=Year, y=Percentage, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
        color=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        geom_linerange(position=position_dodge(width=1), 
            alpha=.25,
            aes(group=!!sym(group_var_name)))  +
        scale_color_viridis_d(option="H") + 
        facet_wrap(vars(!!sym(var_name)), labeller=label_wrap_gen()) +
        labs(y="Percentage", x="") + 
        theme_pubr() 

p <- ggplotly(p)  |> 
    layout(legend = list(
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-var, -group) |> 
    rename("Group N"=total, "N"=n, 
            "Group"=!!group_var_name) |> 
    rbind(all_df)
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
                group_var_labels=group_var_labels)

plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50)


plot_data <- plot_data |> 
        mutate(`95% Lower`=Percentage - 1.96 * `Standard Error`, 
                 `95% Upper`=Percentage + 1.96 * `Standard Error`) |> 
        filter(var==2)

p <- ggplot(plot_data, aes(x=Year, y=Percentage, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
        color=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        geom_linerange(position=position_dodge(width=1), 
            alpha=.25,
            aes(group=!!sym(group_var_name)))  +
        scale_color_viridis_d(option="H") + 
        facet_wrap(vars(!!sym(var_name)), labeller=label_wrap_gen()) +
        labs(y="Percentage", x="") + 
        theme_pubr() 

p <- ggplotly(p)  |> 
    layout(legend = list(
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-var, -group) |> 
    rename("Group N"=total, "N"=n, 
            "Group"=!!group_var_name) |> 
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
                group_var_labels=group_var_labels)

plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50)


plot_data <- plot_data |> 
        mutate(`95% Lower`=Percentage - 1.96 * `Standard Error`, 
                 `95% Upper`=Percentage + 1.96 * `Standard Error`) |> 
        filter(var==2)

p <- ggplot(plot_data, aes(x=Year, y=Percentage, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
        color=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        geom_linerange(position=position_dodge(width=1), 
            alpha=.25,
            aes(group=!!sym(group_var_name)))  +
        scale_color_viridis_d(option="H") + 
        facet_wrap(vars(!!sym(var_name)), labeller=label_wrap_gen()) +
        labs(y="Percentage", x="") + 
        theme_pubr() 

p <- ggplotly(p)  |> 
    layout(legend = list(
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-var, -group) |> 
    rename("Group N"=total, "N"=n, 
            "Group"=!!group_var_name) |> 
    rbind(all_df)
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
                group_var_labels=group_var_labels)

plot_data <- sub_df |> drop_na(!!sym(group_var_name)) |> 
            filter(total > 50)


plot_data <- plot_data |> 
        mutate(`95% Lower`=Percentage - 1.96 * `Standard Error`, 
                 `95% Upper`=Percentage + 1.96 * `Standard Error`) |> 
        filter(var==2)

p <- ggplot(plot_data, aes(x=Year, y=Percentage, 
            ymin=`95% Lower`,
            ymax=`95% Upper`,
        color=!!sym(group_var_name))) + 
        geom_point(position=position_dodge(width=1)) + 
        geom_line(position=position_dodge(width=1)) +
        geom_linerange(position=position_dodge(width=1), 
            alpha=.25,
            aes(group=!!sym(group_var_name)))  +
        scale_color_viridis_d(option="H") + 
        facet_wrap(vars(!!sym(var_name)), labeller=label_wrap_gen()) +
        labs(y="Percentage", x="") + 
        theme_pubr() 
        
p <- ggplotly(p)  |> 
    layout(legend = list(
        orientation = "h",
        y=-0.2
        )
    ) 
p

all_df <- plot_data |> ungroup() |> 
    select(-var, -group) |> 
    rename("Group N"=total, "N"=n, 
            "Group"=!!group_var_name) |> 
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
