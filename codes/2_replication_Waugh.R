# ------------------------------------------------------------------------------
# Replication - Waugh
# ------------------------------------------------------------------------------
# Javier Tasso
# December 2021 
# Upenn 

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Clean and set working directory 
rm(list = ls())
inputs_dir <- "C:/Users/Javier/Dropbox/Aplicaciones/Overleaf/Econ714-Replication/inputs"
outputs_dir <- "C:/Users/Javier/Dropbox/Aplicaciones/Overleaf/Econ714-Replication"

# Load packages 
library("readxl", quietly = TRUE)
library("xtable", quietly = TRUE)
library("lmtest", quietly = TRUE)
library("sandwich", quietly = TRUE)
library("stargazer", quietly = TRUE)

# Trade shares in 1996 
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Here I want to construct table 1
setwd(inputs_dir)
trade_shares_1996 <- read_excel("itid_data.xlsx",sheet = "Trade Share 1996") 
colnames(trade_shares_1996) <- c("Country", "USA","ARG","AUS","AUT","BEL","BEN","BGD","BOL","BRA","CAF","CAN","CHE","CHL","CHN","CMR","COL","CRI","DNK","DOM","ECU","EGY","ESP","ETH","FIN","FRA","GBR","GHA","GRC","GTM","HND","IND","IRL","IRN","ISR","ITA","JAM","JOR","JPN","KEN","KOR","LKA","MEX","MLI","MOZ","MUS","MWI","MYS","NER","NIC","NLD","NOR","NPL","NZL","PAK","PAN","PER","PHL","PNG","PRT","PRY","RWA","SEN","SLE","SLV","SWE","SYR","TGO","THA","TUN","TUR","UGA","URY","VEN","ZAF","ZAR","ZMB","ZWE")
trade_shares_1996$Country <- c("USA","ARG","AUS","AUT","BEL","BEN","BGD","BOL","BRA","CAF","CAN","CHE","CHL","CHN","CMR","COL","CRI","DNK","DOM","ECU","EGY","ESP","ETH","FIN","FRA","GBR","GHA","GRC","GTM","HND","IND","IRL","IRN","ISR","ITA","JAM","JOR","JPN","KEN","KOR","LKA","MEX","MLI","MOZ","MUS","MWI","MYS","NER","NIC","NLD","NOR","NPL","NZL","PAK","PAN","PER","PHL","PNG","PRT","PRY","RWA","SEN","SLE","SLV","SWE","SYR","TGO","THA","TUN","TUR","UGA","URY","VEN","ZAF","ZAR","ZMB","ZWE")
trade_shares_1996 <- as.matrix(trade_shares_1996)
rownames(trade_shares_1996) <- trade_shares_1996[,1]
trade_shares_1996 <- trade_shares_1996[, -1]
trade_shares_1996 <- as.data.frame(trade_shares_1996)
table_1 <- as.matrix(trade_shares_1996[rownames(trade_shares_1996) == "USA" | rownames(trade_shares_1996) == "CAN" | rownames(trade_shares_1996) == "JPN" | rownames(trade_shares_1996) == "MEX" | rownames(trade_shares_1996) == "CHN" | rownames(trade_shares_1996) == "SEN" | rownames(trade_shares_1996) == "MWI" | rownames(trade_shares_1996) == "ZAR" ,colnames(trade_shares_1996) == "USA" | colnames(trade_shares_1996) == "CAN" | colnames(trade_shares_1996) == "JPN" | colnames(trade_shares_1996) == "MEX" | colnames(trade_shares_1996) == "CHN" | colnames(trade_shares_1996) == "SEN" | colnames(trade_shares_1996) == "MWI" | colnames(trade_shares_1996) == "ZAR"])
table_1_bis <- cbind(as.numeric(table_1[,1]), as.numeric(table_1[,2]), as.numeric(table_1[,3]), as.numeric(table_1[,4]), as.numeric(table_1[,5]), as.numeric(table_1[,6]), as.numeric(table_1[,7]), as.numeric(table_1[,8])) * 100
rownames(table_1_bis) <- rownames(table_1)
colnames(table_1_bis) <- colnames(table_1)
table_1 <- table_1_bis
remove(table_1_bis)
setwd(outputs_dir)
print(xtable(table_1, type = "latex", digits = 2, latex.environments = "center", caption = "1996 Trade Shares for Selected Countries", label = "table_1_waugh", align = c("c","c", "c", "c", "c","c", "c", "c", "c")), table.placement='!htbp' , file = "table_1_waugh.tex")

# Clean stuff
remove(table_1)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Correlation between price of tradables and GDP 
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

setwd(inputs_dir)
price_and_income <- read_excel("itid_data.xlsx",sheet = "Price and Income") 
colnames(price_and_income) <- c("Country", "Price85", "Price96", "GDPpw85", "Kpw85", "Labor85", "GDPpw96", "Kpw96", "Labor96")
price_and_income$Country <- c("USA","ARG","AUS","AUT","BEL","BEN","BGD","BOL","BRA","CAF","CAN","CHE","CHL","CHN","CMR","COL","CRI","DNK","DOM","ECU","EGY","ESP","ETH","FIN","FRA","GBR","GHA","GRC","GTM","HND","IND","IRL","IRN","ISR","ITA","JAM","JOR","JPN","KEN","KOR","LKA","MEX","MLI","MOZ","MUS","MWI","MYS","NER","NIC","NLD","NOR","NPL","NZL","PAK","PAN","PER","PHL","PNG","PRT","PRY","RWA","SEN","SLE","SLV","SWE","SYR","TGO","THA","TUN","TUR","UGA","URY","VEN","ZAF","ZAR","ZMB","ZWE")

# Do a linear regression to get the line
price_and_income$Price96_rel_USA <- price_and_income$Price96 / price_and_income$Price96[which(price_and_income$Country == "USA")]
price_and_income$GDPpw96_rel_USA <- price_and_income$GDPpw96 / price_and_income$GDPpw96[which(price_and_income$Country == "USA")]

l_fit <- lm(Price96_rel_USA ~ GDPpw96_rel_USA, data = price_and_income)


# Raw Plot 
setwd(outputs_dir)
pdf("raw_scatterplot_figure_1.pdf", width = 7, height = 5)
plot(price_and_income$GDPpw96_rel_USA, price_and_income$Price96_rel_USA, pch = 19, xlab = "GDP per worker Relative to USA", ylab = "Price of Tradables Relative to USA")
text(price_and_income$GDPpw96_rel_USA, price_and_income$Price96_rel_USA, labels = price_and_income$Country, cex = .5, pos = 4)
lines(price_and_income$GDPpw96_rel_USA, l_fit$fitted.values, type = "l", col = "blue", lw = 2)
dev.off()

# In log2 scale 
price_and_income$Price96_rel_USA_log2 <- log2(price_and_income$Price96_rel_USA)
price_and_income$GDPpw96_rel_USA_log2 <- log2(price_and_income$GDPpw96_rel_USA)
l_fit_2 <- lm(Price96_rel_USA_log2 ~ GDPpw96_rel_USA_log2, data = price_and_income)

# Plot 
setwd(outputs_dir)
pdf("scatterplot_figure_1.pdf", width = 7, height = 5)
plot(price_and_income$GDPpw96_rel_USA_log2, price_and_income$Price96_rel_USA_log2, pch = 19, xlab = "GDP per worker Relative to USA", ylab = "Price of Tradables Relative to USA", xaxt = "n", yaxt = "n")
axis(1, at = c(-6, -5, -4, -3, -2, -1, 0), labels = c(2^(-6), 2^(-5), 2^(-4), 2^(-3),2^(-2), 2^(-1),2^0))
axis(2, at = c(-2, -1.5, -1, -0.5, 0, 0.5, 1), labels = c(2^(-2),2^(-1.5),2^(-1), 2^(-0.5),2^(0),2^(0.5),2^(1)))
text(price_and_income$GDPpw96_rel_USA_log2, price_and_income$Price96_rel_USA_log2, labels = price_and_income$Country, cex = .5, pos = 4)
lines(price_and_income$GDPpw96_rel_USA_log2, l_fit_2$fitted.values, type = "l", col = "blue", lw = 2)
dev.off()

# Clean stuff
remove(l_fit, l_fit_2)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Work with price data to get measure of tau 
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

setwd(inputs_dir)
disag_price_data <- read_excel("1985_disag_prices.xlsx")
disag_price_data2 <- disag_price_data[, -5]
disag_price_data2 <- disag_price_data2[, -c(which(disag_price_data2[2,] == NaN))]
disag_price_data2[4,1] <- "Country"
disag_price_data2[4,2] <- "In_my_data"
disag_price_data2[4,3] <- "In_my_data_and_OECD"
disag_price_data2[4,4] <- "Only_in_my_data"
disag_price_data2 <- disag_price_data2[-c(1,2,3),]
variable_names <- disag_price_data2[1,]
colnames(disag_price_data2) <- variable_names
disag_price_data2 <- disag_price_data2[-c(1),]
disag_price_data2 <- disag_price_data2[, -81]
disag_price_data <- disag_price_data2 
remove(disag_price_data2, variable_names)

# Try to generate ln(tau)
n_countries <- length(disag_price_data$Country)
importer <- numeric(n_countries * n_countries)
exporter <- numeric(n_countries * n_countries)
tau_hat_log <- numeric(n_countries * n_countries)

# Exporter is jj, importer is ii
for (ii in 1:n_countries) {

  for (jj in 1:n_countries) {
    
    if (jj != ii) {
    
      prices_importer_log <- log(as.numeric(disag_price_data[ii,-c(1,2,3,4)]))
      prices_exporter_log <- log(as.numeric(disag_price_data[jj,-c(1,2,3,4)]))
      log_diff_prices <- prices_importer_log - prices_exporter_log
      first_max <- max(na.omit(log_diff_prices))
      second_max <- max(na.omit(log_diff_prices[-which(log_diff_prices == first_max)]))
      tau_hat_log[(ii-1) * 64 + jj] <- second_max 
      
    }
      
    else {
      
      tau_hat_log[(ii-1) * 64 + jj] <- NA    
    
    }
    
    importer[(ii-1) * 64 + jj] <- disag_price_data$Country[ii]
    exporter[(ii-1) * 64 + jj] <- disag_price_data$Country[jj]
    
  }
  
}

# Clean 
remove(ii, jj, second_max, prices_exporter_log, prices_importer_log, first_max, log_diff_prices, n_countries)

# Generate dataset 
dataset_with_tau <- as.data.frame(cbind(importer, exporter, tau_hat_log))

# Clean 
remove(importer, exporter, tau_hat_log)

# Generate an id for the observation importer-exporter
dataset_with_tau$id <- paste(dataset_with_tau$importer, dataset_with_tau$exporter)

# Now I want to reshape the trade_shares_1996
n_countries <- length(trade_shares_1996$USA)
importer <- numeric(n_countries * n_countries)
exporter <- numeric(n_countries * n_countries)
trade_share <- numeric(n_countries * n_countries)
trade_share_normalized <- numeric(n_countries * n_countries)
trade_shares_1996_matrix <- as.matrix(trade_shares_1996)

# Importer is cc (for column) and exporter is rr (for row)

for (cc in 1:n_countries) {
  
  for (rr in 1:n_countries) {
    
    importer[(cc-1) * n_countries + rr] <- colnames(trade_shares_1996)[cc]
    exporter[(cc-1) * n_countries + rr] <- rownames(trade_shares_1996)[rr]
    trade_share[(cc-1) * n_countries + rr] <- as.numeric(trade_shares_1996_matrix[rr,cc])
    trade_share_normalized[(cc-1) * n_countries + rr] <- as.numeric(trade_shares_1996_matrix[rr,cc]) / as.numeric(trade_shares_1996_matrix[cc,cc])
    
  }
  
}

# Remove things
remove(cc, rr, n_countries, trade_shares_1996_matrix)

# Create a temporal dataset to merge 
trade_share_normalized_log <- log(trade_share_normalized)
dataset_temporal_to_merge <- as.data.frame(cbind(importer, exporter, trade_share, trade_share_normalized, trade_share_normalized_log))

# Remove things
remove(importer, exporter, trade_share, trade_share_normalized) 

# Some things as numeric 
dataset_temporal_to_merge$trade_share <- as.numeric(as.character(dataset_temporal_to_merge$trade_share))
dataset_temporal_to_merge$trade_share_normalized <- as.numeric(as.character(dataset_temporal_to_merge$trade_share_normalized))
dataset_temporal_to_merge$trade_share_normalized_log <- as.numeric(as.character(dataset_temporal_to_merge$trade_share_normalized_log))

# Generate id 
dataset_temporal_to_merge$id <- paste(dataset_temporal_to_merge$importer, dataset_temporal_to_merge$exporter)

# Merge the two datasets 
combined_dataset <- merge(dataset_with_tau, dataset_temporal_to_merge, by = c("id", "id"))

# Now we can clean the combined dataset
remove(dataset_temporal_to_merge)
combined_dataset <- subset(combined_dataset, select = -c(importer.y, exporter.y))
combined_dataset_old_including_zeros_and_ones <- combined_dataset
combined_dataset <- combined_dataset[combined_dataset$trade_share_normalized > 0 & combined_dataset$trade_share_normalized < 1, ]
combined_dataset$tau_hat_log <- as.numeric(as.character(combined_dataset$tau_hat_log))

# Plot the relationship between normalized trade shares and tau
l_fit <- lm(trade_share_normalized_log ~ tau_hat_log, data = combined_dataset)
setwd(outputs_dir)
pdf("trade_shares_and_tau.pdf", width = 7, height = 5)
plot(combined_dataset$tau_hat_log, combined_dataset$trade_share_normalized_log, pch = 1, xlab = "Tau (in logs)", ylab = "Normalized Trade Shares (in logs)")
lines(combined_dataset$tau_hat_log, l_fit$fitted.values, type = "l", col = "blue", lw = 2)
dev.off()

# Clean 
remove(l_fit)


# Regression with no constat, first estimate of theta 
linear_model_for_theta <- lm(trade_share_normalized_log ~ tau_hat_log - 1, data = combined_dataset)
cov = vcovHC(linear_model_for_theta, type = "HC")
robust.se = sqrt(diag(cov))
stargazer(linear_model_for_theta, se=list(robust.se), align=TRUE, title="First Estimate of Theta", dep.var.labels = "Trade Share (in logs)", covariate.labels = c("Tau (in logs)"),omit.stat=c("f", "ser", "ll"), out = "linear_model_for_theta.tex")
remove(cov, robust.se, trade_share_normalized_log)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Add distance and border, then save the dataset
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Start with distance
setwd(inputs_dir)
distance_data <- read_excel("itid_data.xlsx",sheet = "Distance") 
colnames(distance_data) <- c("Country", "USA","ARG","AUS","AUT","BEL","BEN","BGD","BOL","BRA","CAF","CAN","CHE","CHL","CHN","CMR","COL","CRI","DNK","DOM","ECU","EGY","ESP","ETH","FIN","FRA","GBR","GHA","GRC","GTM","HND","IND","IRL","IRN","ISR","ITA","JAM","JOR","JPN","KEN","KOR","LKA","MEX","MLI","MOZ","MUS","MWI","MYS","NER","NIC","NLD","NOR","NPL","NZL","PAK","PAN","PER","PHL","PNG","PRT","PRY","RWA","SEN","SLE","SLV","SWE","SYR","TGO","THA","TUN","TUR","UGA","URY","VEN","ZAF","ZAR","ZMB","ZWE")
distance_data$Country <- c("USA","ARG","AUS","AUT","BEL","BEN","BGD","BOL","BRA","CAF","CAN","CHE","CHL","CHN","CMR","COL","CRI","DNK","DOM","ECU","EGY","ESP","ETH","FIN","FRA","GBR","GHA","GRC","GTM","HND","IND","IRL","IRN","ISR","ITA","JAM","JOR","JPN","KEN","KOR","LKA","MEX","MLI","MOZ","MUS","MWI","MYS","NER","NIC","NLD","NOR","NPL","NZL","PAK","PAN","PER","PHL","PNG","PRT","PRY","RWA","SEN","SLE","SLV","SWE","SYR","TGO","THA","TUN","TUR","UGA","URY","VEN","ZAF","ZAR","ZMB","ZWE")
n_countries <- length(distance_data$Country)
importer <- numeric(n_countries * n_countries)
exporter <- numeric(n_countries * n_countries)
distance <- numeric(n_countries * n_countries)
distance_data_matrix <- as.matrix(distance_data)

for (cc in 1:n_countries) {
  
  for (rr in 1:n_countries) {
    
    importer[(cc-1) * n_countries + rr] <- colnames(distance_data)[cc]
    exporter[(cc-1) * n_countries + rr] <- distance_data$Country[rr]
    distance[(cc-1) * n_countries + rr] <- distance_data_matrix[rr,cc]
    
  }
  
}

temporal_to_merge <- as.data.frame(cbind(importer, exporter, distance))
temporal_to_merge$distance <- as.numeric(as.character(temporal_to_merge$distance))
temporal_to_merge <- temporal_to_merge[temporal_to_merge$importer != "Country", ]
temporal_to_merge$id <- paste(temporal_to_merge$importer, temporal_to_merge$exporter)
combined_dataset <- merge(combined_dataset, temporal_to_merge, "id", "id")
remove(temporal_to_merge, distance_data, distance_data_matrix)

# Next, shared border 
border_data <- read_excel("itid_data.xlsx",sheet = "Border") 
colnames(border_data) <- c("Country", "USA","ARG","AUS","AUT","BEL","BEN","BGD","BOL","BRA","CAF","CAN","CHE","CHL","CHN","CMR","COL","CRI","DNK","DOM","ECU","EGY","ESP","ETH","FIN","FRA","GBR","GHA","GRC","GTM","HND","IND","IRL","IRN","ISR","ITA","JAM","JOR","JPN","KEN","KOR","LKA","MEX","MLI","MOZ","MUS","MWI","MYS","NER","NIC","NLD","NOR","NPL","NZL","PAK","PAN","PER","PHL","PNG","PRT","PRY","RWA","SEN","SLE","SLV","SWE","SYR","TGO","THA","TUN","TUR","UGA","URY","VEN","ZAF","ZAR","ZMB","ZWE")
border_data$Country <- c("USA","ARG","AUS","AUT","BEL","BEN","BGD","BOL","BRA","CAF","CAN","CHE","CHL","CHN","CMR","COL","CRI","DNK","DOM","ECU","EGY","ESP","ETH","FIN","FRA","GBR","GHA","GRC","GTM","HND","IND","IRL","IRN","ISR","ITA","JAM","JOR","JPN","KEN","KOR","LKA","MEX","MLI","MOZ","MUS","MWI","MYS","NER","NIC","NLD","NOR","NPL","NZL","PAK","PAN","PER","PHL","PNG","PRT","PRY","RWA","SEN","SLE","SLV","SWE","SYR","TGO","THA","TUN","TUR","UGA","URY","VEN","ZAF","ZAR","ZMB","ZWE")
n_countries <- length(border_data$Country)
importer <- numeric(n_countries * n_countries)
exporter <- numeric(n_countries * n_countries)
border <- numeric(n_countries * n_countries)
border_data_matrix <- as.matrix(border_data)

for (cc in 1:n_countries) {
  
  for (rr in 1:n_countries) {
    
    importer[(cc-1) * n_countries + rr] <- colnames(border_data)[cc]
    exporter[(cc-1) * n_countries + rr] <- border_data$Country[rr]
    border[(cc-1) * n_countries + rr] <- border_data_matrix[rr,cc]
      
  }
  
}

remove(border_data_matrix, cc, n_countries, rr)
temporal_to_merge <- as.data.frame(cbind(importer, exporter, border))
temporal_to_merge <- temporal_to_merge[temporal_to_merge$importer != "Country",]
temporal_to_merge$id <- paste(temporal_to_merge$importer, temporal_to_merge$exporter)
temporal_to_merge <- subset(temporal_to_merge, select = -c(importer, exporter)) 
combined_dataset <- merge(combined_dataset, temporal_to_merge, "id", "id")
combined_dataset <- subset(combined_dataset, select = -c(importer.x, exporter.x))

# Generate a dummy for OECD or not 
combined_dataset$importer_is_OECD = 0
combined_dataset$importer_is_OECD[combined_dataset$importer == "AUS" | combined_dataset$importer == "AUT" | combined_dataset$importer == "BEL" | combined_dataset$importer == "CAN" | combined_dataset$importer == "DNK" | combined_dataset$importer == "ESP" | combined_dataset$importer == "FIN" | combined_dataset$importer == "FRA" | combined_dataset$importer == "GBR" | combined_dataset$importer == "GRC" | combined_dataset$importer == "IRL" | combined_dataset$importer == "IRN" | combined_dataset$importer == "ITA" | combined_dataset$importer == "JPN" |                                     combined_dataset$importer == "KOR" | combined_dataset$importer == "NOR" | combined_dataset$importer == "NZL" | combined_dataset$importer == "SWE" | combined_dataset$importer == "TUR" | combined_dataset$importer == "USA" 
] = 1
combined_dataset$exporter_is_OECD = 0
combined_dataset$exporter_is_OECD[combined_dataset$exporter == "AUS" | combined_dataset$exporter == "AUT" | combined_dataset$exporter == "BEL" | combined_dataset$exporter == "CAN" | combined_dataset$exporter == "DNK" | combined_dataset$exporter == "ESP" | combined_dataset$exporter == "FIN" | combined_dataset$exporter == "FRA" | combined_dataset$exporter == "GBR" | combined_dataset$exporter == "GRC" | combined_dataset$exporter == "IRL" | combined_dataset$exporter == "IRN" | combined_dataset$exporter == "ITA" | combined_dataset$exporter == "JPN" |                                     combined_dataset$exporter == "KOR" | combined_dataset$exporter == "NOR" | combined_dataset$exporter == "NZL" | combined_dataset$exporter == "SWE" | combined_dataset$exporter == "TUR" | combined_dataset$exporter == "USA" 
] = 1


# Export dataset 
  # I will export it in the input folder 
write.csv(combined_dataset,"dataset_in_panel_form.csv", row.names = FALSE)


# Remove things
remove(border, distance, exporter, importer, border_data, combined_dataset_old_including_zeros_and_ones, dataset_with_tau, disag_price_data, linear_model_for_theta, price_and_income, temporal_to_merge, trade_shares_1996, combined_dataset, inputs_dir, outputs_dir)


