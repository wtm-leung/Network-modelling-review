###############################################################################
# Title: NetModRev data analysis
# Author: William Leung 
# Date: 26/02/2023
# Publication: Simulating contact networks for livestock disease epidemiology: a systematic review; 
# Full author list: William T. M. Leung, James W. Rudge, and Guillaume Fournié
# Journal: JRSI
###############################################################################


# load packages----------------------------------------------------------------
library(sf)
library(tigris)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library("viridis")
library("gridExtra")


# Define common plotting options:----------------------------------------------

    # font size
    font_size <-
      list(theme(title=element_text(size=12, hjust=0.5),
            axis.title=element_text(size=12),
            axis.text = element_text(size=10),
            legend.text = element_text(size=12),
            strip.text.y = element_text(size = 12)) )
    
    # Custom colours for model groups 
    custom_col <- 
    list( scale_fill_manual(values = c("Mathematical" = "#313695",
                                        "ABM" = "#74ADD1",
                                        "Radiation model" = "#E0F3F8",
                                        "Gravity model" = "#FEE090",
                                        "(T)ERGM" = "#F46D43",
                                        "Statistical other" = "#A50026",
                                        "Random forests" = "#000000"))  )  
    
    

# Load data: models and papers-------------------------------------------------
d <- read.csv("./data/models.csv")
p <- read.csv("./data/papers_years.csv")



# Organise data ---------------------------------------------------------------

# MODELS
  # convert string to vector/list - these are vars which have multiple entries split by commas
  d$application <- strsplit(d$application, ",")
  d$livestock <- strsplit(d$livestock, ",")
  d$country <- strsplit(d$country, ",")
  d$data <- strsplit(d$data, ",")
  
  # remove lists where not needed
  d$country <- as.character(d$country)
  
  # Group TERGMs and ERGMs for this analysis
  d$framework[d$framework=="TERGM"] <- "ERGM"
  
  # Group mathematical models together for this analysis
  d$framework[d$framework %in% c("Generalised_random_graph","Other_mathematical","Scale-free","Spatial","Watts-Strogatz_")]  <- "Mathematical"
  table(d$framework)

  
# PAPERS
  # Group TERGMs and ERGMs for this analysis
  p$model[p$model=="TERGM"] <- "ERGM"


###############################################################################
# PLOTS------------------------
###############################################################################


# 1. Map of countries models have been developed for -----------------------------  

library(rnaturalearth)

  # Load a Simple Features object of the World
  w <- ne_countries(scale = 'small',                # low 'res' 
                       returnclass = "sf",          # format of the data
                       type = "map_units")          # get individual countries rather than soverign states
                    
                    
  # remove Antarctica
  row.to.del <- which(w$sovereignt == "Antarctica")
  w <- w[-row.to.del, ]
  

  
# Check the country names in our data == the names in the sf data
  
        # subset the countries in our d data that are NOT in the sf data
        unique(unlist(d$country))[!(unique(unlist(d$country)) %in% w$sovereignt)]
  
        # rename those that are missing for consistency with map
        d$country[d$country=="USA"] <- "United States of America"
        d$country[d$country=="Scotland"] <- "United Kingdom"        # This map does not separate UK 
        d$country[d$country=="Britain"] <- "United Kingdom"        # This map does not separate UK 
        d$country[d$country=="Tanzania"] <- "United Republic of Tanzania" 
    
        # check it worked
        unique(unlist(d$country))[!(unique(unlist(d$country)) %in% w$sovereignt)]
        
        
# summarise number of models for each country 
sett <- d %>% group_by(country) %>% summarise( count = n() )
  
# join the data to map data
w_lab <- left_join(w, sett, by=c("sovereignt"="country"))

# change NAs to zeros
w_lab$count[is.na(w_lab$count)] <- 0


# Transform from numeric to categorical for clearer plotting
w_lab$Models <- as.character(w_lab$count)
w_lab$Models <- ordered(w_lab$Models, levels = c("0","1","2","3","11"))


# It was neccesarry to use sovereignt to highlight the whole of the UK. 
# Now remove subunits that should not be highlighted i.e. they do not have models developed for them
#w_lab %>% filter(Models>0) %>% select(sovereignt, subunit, Models) %>% View()
w_lab$Models[w_lab$subunit %in% c("French Southern and Antarctic Lands",
                                  "Falkland Islands",
                                  "French Guiana",
                                  "New Caledonia",
                                  "Puerto Rico")] <- 0


# Plot
plot.map  <-
ggplot(w_lab, aes(fill = Models)) + 
geom_sf() + 
  scale_color_viridis(discrete = TRUE, direction = -1) +
theme_classic() +
  theme_void()


  
# 2. Livestock types studied -----------------------------------------------------  

d$livestock2   <- ifelse(d$livestock %in% c("Pigs","Poultry","Cattle","Sheep_goats"), 
                              d$livestock,
                              "Multiple")        

table(unlist(d$livestock2 ))


plot.livestock <-        
d %>% group_by(livestock2) %>% tally()  %>%     
mutate(Livestock_focus = factor(livestock2, levels=c("Pigs", "Cattle", "Poultry", "Sheep_goats", "Multiple"),
                                labels=c("Pigs", "Cattle", "Poultry", "Sheep/ \ngoats", "Multiple"))) %>%

          
ggplot(aes(x = Livestock_focus, y = n)) +    # reorder orders the bars by highest Count first (-Count)
geom_bar(stat = "identity", fill="#606060") +
theme_classic() +
theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
  axis.title.x=element_blank()) +
  ylab("Models") +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) 





# 3. Model group/type by year------------------------------------------------------------

# Tabulate number of models of each type year
model.groups <- 
d %>% 
group_by(year, framework) %>% 
summarise(count = n())  


# Order and label the levels
model.groups$framework <- ordered(model.groups$framework,
                                   levels = c("Mathematical","ABM","Radiation_model","Gravity_model","ERGM", "Statistical_other","Random_Forest"),
                                   labels = c("Mathematical","ABM","Radiation model","Gravity model","(T)ERGM", "Statistical other","Random forests"))


# plot
plot.year <- 
ggplot(data = model.groups, aes(x = year, y = count, fill = framework)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_continuous(breaks = seq(2008, 2022, by = 2)) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 2)) +
  labs(x = NULL, y = "Models") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        #legend.position = "right",
        legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_blank()) +
        custom_col


    # Extract only the legend of this plot - for neater plotting later
    plot.year.legend <- 
    ggplot(data = model.groups, aes(x = year, y = count, fill = framework)) +
      geom_bar(position = "stack", stat = "identity") +
      scale_x_continuous(breaks = seq(2008, 2022, by = 2)) +
      scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 2)) +
      labs(x = NULL, y = "Models") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.x = element_blank()) +
            custom_col

      legend <- cowplot::get_legend(plot.year.legend)



# 4. Model applications. Multiple allowed---------------------------------------

# Arrange data: we need a table of unlisted application, and then the corresponding model type for each

# make the dataframe you need #1
df1 <- data.frame(application=(unlist(d$application)))

# how many elements are in each list?
temp_lengths <- lengths(d$application)   # use this to repeat the model type x times

# repeat elements x times and add this to the df
df1$framework <- unlist(mapply(rep, d$framework, temp_lengths))


# Plot
data_plot.applications <-
df1 %>%
arrange(application) %>%
  group_by(framework, application) %>% summarise( count = n() )    %>%
   filter(application!="Presents_model")             # remove one study that just uses modeled network to demonstrate the utility of the algorithm


# Order and label the levels
data_plot.applications$framework <- ordered(data_plot.applications$framework,
                                           levels = c("Mathematical","ABM","Radiation_model","Gravity_model","ERGM", "Statistical_other","Random_Forest"),
                                           labels = c("Mathematical","ABM","Radiation model","Gravity model","(T)ERGM", "Statistical other","Random forests"))

# Order and label the levels
 data_plot.applications$application <- ordered(data_plot.applications$application,
                                           levels = c("Behavioural_response",
                                                      "SA_surveillance",
                                                      "SA_altering_network",
                                                      "SA_disease_control",
                                                      "Structure_transmission",
                                                      "Network_generating_processes",
                                                      "Limited_data"
                                                      ),
                                           labels = c("Behavioural response",
                                                      "SA: Surveillance",
                                                      "SA: Altering network",
                                                      "SA: Disease control",
                                                      "Structure & transmission",
                                                      "Network generating processes",
                                                      "Limited data"))

plot.applications <-
   ggplot(data_plot.applications, aes(fill = framework, y = count, x = application)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  theme(
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "right") +
  ylab("Count of models") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  font_size +
  custom_col




# 5. Data needs_multiple allowed------------------------------------------------

# Arrange data: we need a table of unlisted application, and then the corresponding model type for each

# make the dataframe you need #1
df2 <- data.frame(data=(unlist(d$data)))

# how many elements are in each list?
temp_lengths2 <- lengths(d$data)   # use this to repeat the model type x times

# repeat elements x times and add this to the df
df2$framework <- unlist(mapply(rep, d$framework, temp_lengths2))

# Arrange data
plot.data_data <-
df2 %>%
arrange(data) %>%
  group_by(framework, data) %>% summarise( count = n() )   


# Order and label the levels
plot.data_data$framework <- ordered(plot.data_data$framework,
                                           levels = c("Mathematical","ABM","Radiation_model","Gravity_model","ERGM", "Statistical_other","Random_Forest"),
                                           labels = c("Mathematical","ABM","Radiation model","Gravity model","(T)ERGM", "Statistical other","Random forests"))

# Order and label the levels
 plot.data_data$data <- ordered(plot.data_data$data,
    levels=c( "Network_survey", "LITS", "Movement_permits","Census", "Emergent", "Expert_opinion"),
    labels=c( "Network survey", "LITS", "Movement permits","Census", "Emergent", "Expert opinion")
    )
     


plot.data <-
ggplot(plot.data_data, aes(fill = framework, y = count, x = data)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  ylab("Count of models") +
  theme(legend.title = element_blank()) +
  font_size +
  custom_col





###############################################################################
# PAPERS analysis----------------------
###############################################################################

# Make TERGMs and ERGMs the same for this analysis
p$model[p$model=="TERGM"] <- "ERGM"


# 6. Papers by year-------------------------------------------------------------
plot.papers.year <- 
ggplot(p, aes(x = year)) +    # reorder orders the bars by highest Count first (-Count)
geom_bar(fill="#606060") +
theme_classic() +
theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.title.x=element_blank()) +
    ylab("Publications") +
    scale_y_continuous(limits = c(0,8), breaks = seq(0, 8, by = 2)) +
    scale_x_continuous(breaks = seq(2008, 2022, by = 2))







###############################################################################
# Arrange plots -------------------
###############################################################################

library(gridExtra)
library(grid)


# Figure 2----------------------------------------------------------------------

# Create a PDF file
pdf("./figures/fig2.pdf", width = 6.4, height = 6.4)    # inches

# Create the plot
fig <- grid.arrange(plot.map,
                    arrangeGrob(plot.papers.year, plot.year, plot.livestock, ncol = 3),   # Second row with 2 plots in 2 different columns
                    legend,
                    nrow = 3,
                    heights = c(1, 1, 0.25))

# Add labels
grid.text("A", x = 0.02, y = 0.9, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("B", x = 0.02, y = 0.525, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("C", x = 0.35, y = 0.525, gp = gpar(fontsize = 12, fontface = "bold"))
grid.text("D", x = 0.7, y = 0.525, gp = gpar(fontsize = 12, fontface = "bold"))

# Print the plot to the PDF file
print(fig)

# Close the PDF file and save it
dev.off()



# Figure 3----------------------------------------------------------------------
fig3 <-
ggpubr::ggarrange(plot.applications,plot.data,ncol=2, nrow=1, common.legend = T, legend="right",
                      labels = c('A', 'B'), 
                      widths = c(1,0.5) ) 
 

ggsave(fig3, filename = "./figures/fig3.pdf", 
       width = 22, height = 9, units="cm") 

