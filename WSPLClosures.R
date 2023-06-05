# LIS 572 Final Project 
# Maria Arteaga Cuevas

#load/install libraries
library(tidyr) 
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(data.table) 
library(wesanderson)
library(maps)


# Q1
# What are the most common reasons for WSPL closures reported in 2021?

# Load libraries df
wpl_df <- read.csv("/Users/mariaarteaga/Desktop/2021branchlibraries_V2.csv", stringsAsFactors = FALSE)

# Select relevant columns relating to library name, type, and service (dplyr)
wpl_selected <- wpl_df %>% 
  select(Branch_Library,City,County,Outlet_Type,Covid19:Other_Closure)

# Remove Outlet_Type that are not Branch and Central Libraries (dplyr)
wpl_filtered<- filter(wpl_selected, Outlet_Type == "Branch" | Outlet_Type == "Central Library")

num_values <- wpl_filtered%>%
  mutate(Covid19 = ifelse(Covid19 == "No",0,1)) %>% 
  mutate(Natural_Disaster = ifelse(Natural_Disaster == "No",0,1)) %>% 
  mutate(Construction = ifelse(Construction == "No",0,1)) %>% 
  mutate(Relocation = ifelse(Relocation == "No",0,1)) %>% 
  mutate(Budget = ifelse(Budget == "No",0,1)) %>% 
  mutate(Other_Closure = ifelse(Other_Closure == "Yes",1,0))

# Select county and closure columns
closures_only <- num_values %>% 
  select(County,Covid19,Natural_Disaster,Construction,Relocation,Budget,Other_Closure)

# Make NA values 0 using tidyr mutate and replace_na
closures_only <- mutate_all(closures_only, ~replace_na(.,0)) 

# Find sum of all columns (looking at the whole state not just counties )
closure_sums <- closures_only %>% 
  select(-c('County'))

# Compute column sums
totals <- closure_sums %>%                        
  replace(is.na(.), 0) %>%
  summarise_all(sum)

# flip data
#transpose data frame (RUN AT THE SAME TIME)
transposed_df <- transpose(totals)
#redefine row and column names
rownames(transposed_df) <- colnames(totals)
colnames(transposed_df) <- rownames(totals)
# Duplicate example data # Apply setDT function
transposed_df <- setDT(transposed_df, keep.rownames = TRUE)[]       

# Rename columns 
closure_totals <- transposed_df %>% 
  rename("Closure_Reason" = "rn",
         "Total" = "1")

# Remove "_" from Closure_Reason
closure_totals$Closure_Reason<-gsub("_"," ",as.character(closure_totals$Closure_Reason))

# Q1 PLOT 
custom_color1 <- wes_palette("Royal1", 6, type = "continuous")

q1_plot <- ggplot(data = closure_totals)+
  geom_col(mapping = aes(x = reorder(Closure_Reason, Total), y = Total), fill = custom_color1)+
  labs(title = "Reasons for WSPL closures in 2021", x = "Closure Reason", y = "# of closures",)+
  labs(fill = "Closure Reason")+
  coord_flip()

q1_plotly <- ggplotly(q1_plot, tooltip = c("Total"))



#Q2
# What WSPL counties have reported library closures due to natural disasters or weather events? 

# Use wpl_filtered from Q1 select only counties and nd column
q2_nd_df <- wpl_filtered %>% 
  select(County, Natural_Disaster)

# Convert to lowercase
q2_nd_df$County <- tolower(q2_nd_df$County)

# Convert natural disaster yes values to 1 and no to 0 
q2_num_values <- q2_nd_df%>%
  mutate(Natural_Disaster = ifelse(Natural_Disaster == "No",0,1)) 

# Find sum of closures by county
q2_totals <- q2_num_values %>%
  group_by(County) %>% 
  summarize(sum(Natural_Disaster))

# Rename columns
q2_totals <- q2_totals %>% 
  rename("Total" = "sum(Natural_Disaster)")

# WA county data 
state_map <- map_data("county","washington")

# Rename subregion to County
state_map <- state_map %>% 
  rename("County" = "subregion")

# Merge the data sets together
nd_map<- merge(q2_totals, state_map, by = "County")

# Sort  df
nd_map <- arrange(nd_map, group, order)


# Q2 PLOT Choropleth map 
q2_plot <- ggplot(nd_map, aes(x = long, y = lat, group = group, fill = Total)) +
  geom_polygon(color = "black") + #lines
  scale_fill_continuous(low = "white", high = "black") +
  labs(title = 'WSPL Closures\nReason: Natural Disaster 2021', fill = '# Closed') +
  coord_map("polyconic")

q2_plotly <- ggplotly(q2_plot, tooltip = "Total")

# Q2 PLOT Bar graph 
plot2_df <- q2_totals %>% 
  filter(Total >= 1)

q2_plot2 <- ggplot(data = plot2_df)+
  geom_col(mapping = aes(x = reorder(County, Total), y = Total), fill = "grey")+
  labs(title = "WSPL Closures\nReason: Natural Disaster 2021", x = "County", y = "# Closed",) +
  labs(fill = "County") +
  coord_flip()

q2_plotly2 <- ggplotly(q2_plot2, tooltip = "Total")



#Q3
# What are the most common reasons for WSPL closures reported in 2019?

# Load libraries df
q3_wpl_df <- read.csv("/Users/mariaarteaga/Desktop/2019stats_V2.csv", stringsAsFactors = FALSE)

# Rename columns that will be used
q3_wpl_df <- q3_wpl_df %>% 
  rename("Branch_Library" = "Branch.Name",
         "Outlet_Type" = "Outlet.Type.Code",
         "Natural_Disaster" = "Weather..Wildfire..other.natural.phenomena",
         "Construction" = "Construction..New.or.Remodel.",
         "Relocation" = "Relocation",
         "Budget" = "Budget",
         "Other_Closure" = "Other")

# Select relevant columns relating to library name, type, and service (dplyr)
q3_wpl_selected <- q3_wpl_df %>% 
  select(Branch_Library,City,County,Outlet_Type,Natural_Disaster:Other_Closure)

# Remove Outlet_Type that are not Branch and Central Libraries (dplyr)
q3_wpl_filtered<- filter(q3_wpl_selected, Outlet_Type == "Branch" | Outlet_Type == "Central Library")

# Convert yes values to 1 and no to 0 
q3_num_values <- q3_wpl_filtered%>%
  mutate(Natural_Disaster = ifelse(Natural_Disaster == "No",0,1)) %>% 
  mutate(Construction = ifelse(Construction == "No",0,1)) %>% 
  mutate(Relocation = ifelse(Relocation == "No",0,1)) %>% 
  mutate(Budget = ifelse(Budget == "No",0,1)) %>% 
  mutate(Other_Closure = ifelse(Other_Closure == "Yes",1,0))

# Select only closure columns
q3_closures_only <- q3_num_values %>% 
  select(County,Natural_Disaster,Construction,Relocation,Budget,Other_Closure)

# Find sum of all columns (looking at the whole state not just counties )
q3_closure_sums <- q3_closures_only %>% 
  select(-c('County'))

# Compute column sums
q3_totals <- q3_closure_sums %>%                        
  summarise_all(sum)

# flip data 
# Transpose data frame (RUN AT THE SAME TIME)
q3_transposed_df <- transpose(q3_totals)
# Redefine row and column names
rownames(q3_transposed_df) <- colnames(q3_totals)
colnames(q3_transposed_df) <- rownames(q3_totals)
# Duplicate example data # Apply setDT function
q3_transposed_df <- setDT(q3_transposed_df, keep.rownames = TRUE)[]  

# Rename columns 
q3_closure_totals <- q3_transposed_df %>% 
  rename("Closure_Reason" = "rn",
         "Total" = "1")
# Remove _
q3_closure_totals$Closure_Reason<-gsub("_"," ",as.character(q3_closure_totals$Closure_Reason))


# Q3 PLOT 
custom_color <- wes_palette("Royal1", 5, type = "continuous")

q3_plot <- ggplot(data = q3_closure_totals)+
  geom_col(mapping = aes(x = reorder(Closure_Reason, Total), y = Total), fill = custom_color)+
  labs(title = "Reasons for WSPL closures in 2019", x = "Closure Reason", y = "# of closures",) +
  labs(fill = "Closure Reason") +
  coord_flip()

q3_plotly <- ggplotly(q3_plot, tooltip = c("Total"))



#Q4
#What are the most common reasons for WSPL closures reported in 2019?

# Load libraries df
q4_wpl_df <- read.csv("/Users/mariaarteaga/Desktop/2020branchlibraries_V2.csv", stringsAsFactors = FALSE)

# Select relevant columns relating to library name, type, and service (dplyr)
q4_wpl_selected <- q4_wpl_df %>% 
  select(Branch_Library,City,County,Outlet_Type,Covid19:Other_Closure)
# Remove Outlet_Type that are not Branch and Central Libraries (dplyr)
q4_wpl_filtered<- filter(q4_wpl_selected, Outlet_Type == "Branch" | Outlet_Type == "Central Library")

q4_num_values <- q4_wpl_filtered%>%
  mutate(Covid19 = ifelse(Covid19 == "No",0,1)) %>% 
  mutate(Natural_Disaster = ifelse(Natural_Disaster == "No",0,1)) %>% 
  mutate(Construction = ifelse(Construction == "No",0,1)) %>% 
  mutate(Relocation = ifelse(Relocation == "No",0,1)) %>% 
  mutate(Budget = ifelse(Budget == "No",0,1)) %>% 
  mutate(Other_Closure = ifelse(Other_Closure == "Yes",1,0))

# Select county and closure columns
q4_closures_only <- q4_num_values %>% 
  select(County,Covid19,Natural_Disaster,Construction,Relocation,Budget,Other_Closure)

# Make NA values 0 using tidyr mutate and replace_na
q4_closures_only <- mutate_all(q4_closures_only, ~replace_na(.,0)) 

# Find sum of all columns (looking at the whole state not just counties )
q4_closure_sums <- q4_closures_only %>% 
  select(-c('County'))
# Compute column sums
q4_totals <- q4_closure_sums %>%                        
  replace(is.na(.), 0) %>%
  summarise_all(sum)

# flip data
#transpose data frame (RUN AT THE SAME TIME)
q4_transposed_df <- transpose(q4_totals)
#redefine row and column names
rownames(q4_transposed_df) <- colnames(q4_totals)
colnames(q4_transposed_df) <- rownames(q4_totals)
# Duplicate example data # Apply setDT function
q4_transposed_df <- setDT(q4_transposed_df, keep.rownames = TRUE)[]   

# Rename columns 
q4_closure_totals <- q4_transposed_df %>% 
  rename("Closure_Reason" = "rn",
         "Total" = "1")

# Remove "_" from Closure_Reason
q4_closure_totals$Closure_Reason<-gsub("_"," ",as.character(q4_closure_totals$Closure_Reason))

# Q4 PLOT 
custom_color1 <- wes_palette("Royal1", 6, type = "continuous")

q4_plot <- ggplot(data = q4_closure_totals)+
  geom_col(mapping = aes(x = reorder(Closure_Reason, Total), y = Total), fill = custom_color1)+
  labs(title = "Reasons for WSPL closures in 2020", x = "Closure Reason", y = "# of closures",)+
  labs(fill = "Closure Reason")+
  coord_flip()

q4_plotly <- ggplotly(q4_plot, tooltip = c("Total"))