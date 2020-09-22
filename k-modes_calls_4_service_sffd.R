# K-modes for the clustering of
# San Francisco's Fire Department's calls for service
# by
# Aguilar, Sarahí: 0189970@up.edu.mx
# Escobar, Diana:  0231930@up.edu.mx
# Lagunes, Pablo:  0234684@up.edu.mx

rm(list = ls(all = TRUE))

library(data.table)
dt <- fread('./Fire_Department_Calls_for_Service.csv')

##### Preprocessing #####

# Replacing spaces in column names
new_names <- gsub(' ', '_', names(dt))
setnames(dt, 
         old = names(dt),
         new = new_names)

# Homologate City values 
dt[, City := ifelse(City %in% c('TI', 'TREASURE ISLAND', 'Treasure Isla'), 'Treasure Island', City)]
dt[, City := ifelse(City %in% c('SFO', 'SF', 'SAN FRANCISCO'), 'San Francisco', City)]
dt[, City := ifelse(City %in% c('HP', 'HUNTERS POINT'), 'Hunters Point', City)]
dt[, City := ifelse(City %in% c('PR', 'PRESIDIO'), 'Presidio', City)]
dt[, City := ifelse(City %in% c('FM', 'FORT MASON'), 'Fort Mason', City)]
dt[, City := ifelse(City %in% c('YB', 'YERBA BUENA', 'YERBA BUENA IS'), 'Yerba Buena', City)]
dt[, City := ifelse(City %in% c('DC', 'DALY CITY'), 'Daly City', City)]
dt[, City := ifelse(City == 'BN', 'Brisbane', City)]
dt[, City := ifelse(City == 'OAK', 'Oak', City)]

# Selecting columns of interest
selected_cols <- c('Unit_ID', 
                   'ALS_Unit',
                   'Call_Type', 
                   'Call_Final_Disposition', 
                   'Number_of_Alarms',
                   'Unit_sequence_in_call_dispatch',
                   'Supervisor_District',
                   'City',
                   'Received_DtTm',
                   'Response_DtTm',
                   'Final_Priority')
dt <- dt[, selected_cols, with = FALSE]

# Removing rows with blank or NA values
dt[dt == ''] <- NA
dt <- dt[complete.cases(dt), ]

# Calculate y variable: Received_to_Response
library(lubridate)

dt$Received_DtTm <- parse_date_time(dt$Received_DtTm, '%m/%d/%Y %H:%M:%S %p')
dt$Response_DtTm <- parse_date_time(dt$Response_DtTm, '%m/%d/%Y %H:%M:%S %p')
dt$Received_to_Response <- as.numeric(dt$Response_DtTm - dt$Received_DtTm)

# Parsing columns to factor
factor_cols <- c('Unit_ID', 
                 'ALS_Unit',
                 'Call_Type',        
                 'Call_Final_Disposition', 
                 'Number_of_Alarms',
                 'Unit_sequence_in_call_dispatch',
                 'Supervisor_District',
                 'City',
                 'Final_Priority')
dt[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

##### K-modes #####

library(klaR)
# set.seed(01112019)
cluster_cols <- c('Unit_ID', 
                 'Call_Type', 
                 'Call_Final_Disposition',
                 'Number_of_Alarms',
                 'Unit_sequence_in_call_dispatch')
set.seed(42)
dt_clusters <- kmodes(dt[, cluster_cols, with = FALSE], 4, iter.max = 10)

# Exporting clustered data
dt$cluster <- as.factor(dt_clusters$cluster)
write.csv(dt, 'clustered_data.csv')

##### Exploring results #####

table(dt_clusters$cluster)/nrow(dt)*100

dt[cluster == 1, .N/nrow(dt[cluster == 1,]), by = Call_Type][order(V1)]

dt[, .(priority = 2:3, percentage = tabulate(Final_Priority)/.N), by = cluster]
