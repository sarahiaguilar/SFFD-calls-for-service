# Online linear regression for the prediction of response time 
# in San Francisco's Fire Department's calls for service
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

# Selecting columns of interest
selected_cols <- c('Call_Type', 
                   'ALS_Unit',
                   'Call_Final_Disposition', 
                   'Supervisor_District',
                   'Final_Priority',
                   'Received_DtTm',
                   'Response_DtTm')
dt <- dt[, selected_cols, with = FALSE]

# Removing rows with blank or NA values
dt[dt == ''] <- NA
dt <- dt[complete.cases(dt), ]

# Calculate y variable: Received_to_Response
library(lubridate)

dt$Received_DtTm <- parse_date_time(dt$Received_DtTm, '%m/%d/%Y %H:%M:%S %p')
dt$Response_DtTm <- parse_date_time(dt$Response_DtTm, '%m/%d/%Y %H:%M:%S %p')
dt$Received_to_Response <- as.numeric(dt$Response_DtTm - dt$Received_DtTm)

# Remove rows with mistaken and outlying Received_to_Response values
dt <- dt[Received_to_Response >= 0]
quantile(dt$Received_to_Response, 0.95)
dt <- dt[Received_to_Response <= as.numeric(quantile(dt$Received_to_Response, 0.95))] # 469

# One-hot encoding
cols_to_dummy <- c('Call_Type',
                   'ALS_Unit',
                   'Call_Final_Disposition',
                   'Supervisor_District',
                   'Final_Priority')
dt <- dt[, c(cols_to_dummy, 'Received_to_Response'), with = FALSE]
dt <- fastDummies::dummy_cols(dt, 
                              select_columns = cols_to_dummy,
                              remove_first_dummy = TRUE)

new_names <- gsub(' ', '_', names(dt))
setnames(dt,
         old = names(dt),
         new = new_names)

##### Data exploration #####

# Exploring explicative variables
dt[, .N/nrow(dt), by = ALS_Unit][order(V1)]
dt[, mean(Received_to_Response), by = ALS_Unit]

dt[, .N/nrow(dt), by = Final_Priority][order(V1)]
dt[, mean(Received_to_Response), by = Final_Priority]

dt[, .N/nrow(dt), by = Call_Type][order(V1)]
dt[, mean(Received_to_Response), by = Call_Type]

dt[, .N/nrow(dt), by = Call_Final_Disposition][order(V1)]
dt[, mean(Received_to_Response), by = Call_Final_Disposition]

dt[, .N/nrow(dt), by = Supervisor_District][order(V1)]
dt[, mean(Received_to_Response), by = Supervisor_District]

# Exploring Received_to_Response
mean(dt$Received_to_Response)
max(dt$Received_to_Response)

ggplot(dt, aes(x = Received_to_Response)) + 
  geom_histogram(color = '#199988', fill = 'white') + 
  xlab('Response time (s)') + 
  ylab('f') +
  theme_light()

# Correlations
categorical_correlations <- function(variables, df){
  sapply(variables, function(y) sapply(variables, function(x) vcd::assocstats(table(df[, x], df[, y]))$cramer))
}
correlations <- categorical_correlations(cols_to_dummy, as.data.frame(dt))
write.csv(correlations, 'correlations_final.csv')

##### Online linear regression model ######

dt <- dt[, -c(cols_to_dummy), with = FALSE]

dat <- copy(dt)
dat <- cbind(x0 = 1, dat)

n <- nrow(dat)

y <- as.matrix(dat[, 'Received_to_Response'])
X <- as.matrix(dat[, -c('Received_to_Response')])

epoch <- 100
alpha <- 0.001

beta <- matrix(rep(0, dim(X)[2]), nrow = dim(X)[2])
costo <- rep(0, epoch)

for(i in seq(1, epoch, by = 1)){
  for(j in seq(1, n, by = 1)){
    y_hat <- X[j, ] %*% beta
    error <- y_hat - y[j]
    beta <- beta - alpha * X[j, ] %*% error
  }
  costo[i] <- 1/(2*n) * sum(error^2)
  print(i)
}

y_pred <- X %*% beta
dat$pred <- y_pred

# Exporting results
write.csv(dat, 'dat_final.csv')
write.csv(costo, 'costo_final.csv')
write.csv(beta, 'beta_final.csv')
write.csv(names(dat), 'names_final.csv')

##### Results exploration ######

# Exploring predicted Received_to_Response
mean(dat$pred)
max(dat$pred)

ggplot(dat, aes(x = pred)) + 
  geom_histogram(color = '#199988', fill = 'white') + 
  xlab('Response time (s)') + 
  ylab('f') +
  theme_light()

# Plot actual and predicted Received_to_Response values
ggplot(dat[1:2000, ], aes(pred, Received_to_Response)) + 
  geom_point(colour = '#199988', size = 1) +
  xlab('Predicted y') + 
  ylab('y') +
  theme_light()

# Exploring cost through 100 epochs
costo_dt <- data.table(1:100, costo)
ggplot(costo_dt, aes(V1, costo)) + 
  geom_line(alpha = 0.5) +
  geom_point(color = '#199988', size = 3) +
  xlab('Epoch') + 
  ylab('Costo') +
  theme_light()
