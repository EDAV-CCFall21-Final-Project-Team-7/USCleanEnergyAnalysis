library(tidyverse)
library(data.table)
library(d3r)
library(parcoords)
library(GGally)
library(janitor)
library(egg)
library(ggpubr)

#Finding Missing Patterns Function
find_missing_patterns <- function(df, aggregation = 'count'){
  
  #find missing patterns
  missing_patterns <- data.frame(is.na(df)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  #create g1
  missing_patterns_sums <- df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>% 
    pivot_longer(cols = names(df), names_to='name',values_to='value') %>%
    arrange(desc(value))
  
  if (aggregation=='percent'){
    total_sum<-length(row.names(df))
    missing_patterns_sums$value<- missing_patterns_sums$value*100/total_sum
    print(total_sum)
    y_label = 'percent rows \n missing'
  }
  else{
    y_label = 'num rows \n missing'
  }
  column_reorder <- missing_patterns_sums$name
  
  
  g1 <- ggplot(missing_patterns_sums, aes(x=fct_reorder(name, -value), y=value))+
    geom_bar(stat='identity', fill='#9eb7ee') +
    scale_y_continuous(limits=c(0, max(missing_patterns_sums$value)), expand=c(0,0)) +
    xlab('') +
    ylab(y_label) +
    ggtitle('Missing value patterns') +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5), 
          panel.background = element_rect(fill = "white"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color='darkgray', size=.1)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(aspect.ratio = .25)
  
  #Create g3
  missing_patterns_count <- missing_patterns %>% mutate_all((as.integer)) %>% adorn_totals("col")
  missing_patterns_count$col <- as.factor(ifelse(missing_patterns_count$Total - missing_patterns_count$count == 0 , 1, 0.5))
  missing_patterns_count$id <- row.names(missing_patterns_count)
  
  if(aggregation=='percent'){
    missing_patterns_count$count<-missing_patterns_count$count*100/sum(missing_patterns_count$count)
    x_label = 'percent rows'
  }
  else{
    x_label = 'row count'
  }
  pattern_id <- missing_patterns_count  %>% filter(col == 1) %>% select(id) %>% as.character()
  missing_patterns_count$id <- factor(missing_patterns_count$id)
  missing_patterns_count$id <- fct_relevel(missing_patterns_count$id, paste(sort(as.integer(levels(missing_patterns_count$id)), decreasing = TRUE)))
  g3 <- missing_patterns_count %>%
    #rowid_to_column(var="pattern") %>%
    #mutate(pattern = factor(pattern)) %>%
    ggplot(aes(y=fct_reorder(id, -desc(count)), x=count)) + 
    geom_bar(stat='identity', fill='#9eb7ee',aes(alpha = factor(missing_patterns_count$col))) +
    scale_x_continuous(limits=c(0, max(missing_patterns_count$count)), expand=c(0,0)) + #, breaks = seq(0,max(missing_patterns$count), 5) if want to have the y axis break on 5s
    ylab('') +
    xlab(y_label) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5), 
          panel.background = element_rect(fill = "white"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color='darkgray', size=.1),) +
    scale_alpha_manual(values=c("0.5"=0.5,"1"=1),guide="none") +
    theme(aspect.ratio = 4)
  
  #create g2
  main_plot_df <- missing_patterns %>%
    select(-c(count)) %>%
    as_tibble() %>%
    rowid_to_column(var="Y") %>%
    gather(key="X", value="Z", -1) 
  
  main_plot_df$X <- as.factor(main_plot_df$X)
  main_plot_df$Y <- as.factor(main_plot_df$Y)
  main_plot_df$X <- fct_relevel(main_plot_df$X, column_reorder)
  main_plot_df$Y <- fct_relevel(main_plot_df$Y, rev(levels(main_plot_df$Y)))
  
  
  main_plot_df$Z <- as.factor(ifelse(main_plot_df$Y == pattern_id, .5, main_plot_df$Z))
  
  
  g2 <- ggplot(main_plot_df, aes(X, Y, fill= Z)) + 
    geom_tile(color='white') +
    theme(legend.position="none") +
    scale_fill_manual(values=c('#cacaca', 'darkgray', '#b2a0e1')) +
    annotate("text",x=length(unique(main_plot_df$X))/2,y=length(unique(main_plot_df$Y))-as.integer(pattern_id)+1,label='complete cases') + 
    #xlab('Variable') +
    ylab('Missing Pattern') +
    theme_classic() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1)) +
    coord_equal() + theme(aspect.ratio = length(row.names(missing_patterns))/(length(names(missing_patterns))-1))
  
  #Arrange graphs
  blank <- ggplot() + 
    geom_point(aes(1,1), colour="white") +
    theme(                              
      plot.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  
  ggarrange(g1,blank,g2,g3,ncol=2,nrow=2,widths = c(16,4),heights=c(1,4))
  #return(missing_patterns_count)
}



#Get Wb data & process it for analysis

us_wb_df_r <- as.data.frame(read_csv('data/raw/clean_worldbank.csv'))

us_wb_df <- as.data.frame(read_csv('data/raw/clean_worldbank.csv'))
row.names(us_wb_df)<-us_wb_df[,"Series"]
us_wb_df <-us_wb_df[,2:length(names(us_wb_df))]

#Apply short names
rownames(us_wb_df) <- c('Pop T', 'Pop G', 'Area', 'Pov Rt', 'GNI T','GNI PC', 'GNI PPP', 'GNI PPP Pc', 'Inc Shr', 'Life Exp', 'Fertility Rt', 'Ado Ft', 'Contr Pr', 'Birth Rt', 'Mort Rt', 'Under Wt', 'Imm Mea', 'Prim Comp', 'Sec Sch', 'Prim Sch', 'HIV', 'Forest A', 'Water P', 'Energy Use', 'C02 Em', 'Elec Pwr', 'GDP T', 'GDP %', 'Infl %', 'Agri %', 'Industry %', 'Exports %', 'Imports %', 'Gross %', 'Revenue, %', 'Start up', 'Mkt Cap %', 'Military %', 'Mobile Sub', 'HT Exp %', 'Merchandise %', 'Net Bart', 'Ext Debt', 'Debt Sv %', 'Migration', 'Remittance', 'FDI', 'ODA')



#matrix for missing data analysis
my_us_wb_m <- t(us_wb_df)
my_us_wb_df_p <- as.data.frame(t(us_wb_df))

#Missing value by column
colSums(is.na(my_us_wb_m)) %>%
  sort(decreasing = TRUE)

missing_column_values <- as.matrix(colSums(is.na(my_us_wb_m)) %>%
           sort(decreasing = TRUE))
#Missing value by row
rowSums(is.na(my_us_wb_m)) %>%
  sort(decreasing = TRUE)

missing_row_values <- as.matrix(rowSums(is.na(my_us_wb_m)) %>%
                                  sort(decreasing = TRUE))

library(mi)
x <- mi::missing_data.frame(my_us_wb_m)
image(x)

find_missing_patterns(us_wb_df_r, 'count')
find_missing_patterns(my_us_wb_df_p, 'count')

#  Parallel coordinate plot 

my_us_wb_df_p <- my_us_wb_df_p %>% relocate('GDP %', .before = 'GDP T')
my_us_wb_df_p <- my_us_wb_df_p %>% relocate('GNI PC', .before = 'GDP %')

library(tidyverse)
library (dplyr)
library(GGally)

ggparcoord(my_us_wb_df_p, columns = 23:27, scale = "globalminmax") + 
  geom_vline(xintercept = 1:8, color = "lightblue") +
  ggtitle("US Macro Energy Consumption, CO2 Emission, & Economic Indicators ") + 
  xlab("") + 
  ylab("") +
  theme(plot.title = element_text(hjust=.5))

ggparcoord(my_us_wb_df_p, columns = 23:27,  alphaLines = 0.3, scale = "uniminmax") + 
  geom_vline(xintercept = 1:8, color = "lightblue") +
  ggtitle("U.S. Macro Energy Consumption, \n CO2 Emission, & Economic Indicators ") + 
  xlab("") + 
  ylab("Standardize Scale") +
  theme_classic(16) +
  theme(plot.title = element_text(hjust=.5))

#library(GGally)
#library(parcoords)
#parcoords(my_us_wb_df_p[,23:27], reorderable = T, queue = T, brushMode = '1D-axes', withD3 = TRUE)


ggparcoord(my_us_wb_df_p, columns = 23:27, scale = "uniminmax", alphaLines = 0.3, splineFactor = 10) + 
  geom_vline(xintercept = 1:8, color = "lightblue") +
  ggtitle("US Macro Energy Consumption, CO2 Emission, & Economic Indicators ") + 
  xlab("") + 
  ylab("") +
  theme(plot.title = element_text(hjust=.5))

# Annual CO2 emission trend plot

ggplot(my_us_wb_df_p, aes( my_us_wb_df_p$`Energy Use`, my_us_wb_df_p$`C02 Em`)) +
  geom_point() +
  ylab("CO2 emissions (metric tons per capita)") +
  xlab("Energy use per capita") +
  ggtitle("U.S. CO2 emission went up with more per capita energy consumption 1960 - 2020") +
  geom_smooth( method = 'lm', se = FALSE) +
  stat_poly_eq(formula = y~x) +
  theme_classic(16) +
  theme(plot.title = element_text(hjust=.5))


my_us_wb_df_p_yr <- my_us_wb_df_p
my_us_wb_df_p_yr <-tibble::rownames_to_column(my_us_wb_df_p_yr, "Year")
my_us_wb_df_p_yr$Year <- as.numeric(my_us_wb_df_p_yr$Year)
my_us_wb_df_p_yr_2000 <-my_us_wb_df_p_yr %>% filter(Year > 1999)

ggplot(my_us_wb_df_p_yr_2000, aes( my_us_wb_df_p_yr_2000$Year, my_us_wb_df_p_yr_2000$`C02 Em`)) +
  geom_point() +
  ylab("CO2 emissions (metric tons per capita)") +
  xlab("Year") +
  ggtitle("U.S. reversed the trend of per capaita CO2 emission 2020 - 2020") +
  stat_smooth(colour = "green") +
  theme_classic(16) +
  theme(plot.title = element_text(hjust=.5))





