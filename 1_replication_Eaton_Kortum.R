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

# Load libraries
#
#

# Load gravity data and country names
setwd(inputs_dir)
gravity_data <- read.table("gravity_data.txt")
country_names <- read.table("country_names.txt")
gravity_data$importer <- gravity_data$V1
gravity_data$exporter <- gravity_data$V2
gravity_data$trade_data <- gravity_data$V3
gravity_data$distance <- gravity_data$V4
gravity_data$border <- gravity_data$V5
gravity_data <- subset(gravity_data, select = c(importer, exporter, trade_data, distance, border))
gravity_data$trade_data_log_10 <- log10(gravity_data$trade_data)
gravity_data$distance_log_10 <- log10(gravity_data$distance)
country_names$name <- country_names$V1
country_names$name_short <- country_names$V2
country_names$number <- country_names$V3
country_names <- subset(country_names, select = c(name, name_short, number))

# Identify the countries used in Eaton Kortum 
  # Denmark is missing
  # New Zealand is missing
  # Netherlands is missing
country_names$used_in_EK <- 0 
country_names$used_in_EK[country_names$name_short == "AUS" | country_names$name_short == "AUT" | country_names$name_short == "BEL" | country_names$name_short == "CAN" | country_names$name_short == "FIN" | country_names$name_short == "FRA" | country_names$name_short == "DEU" | country_names$name_short == "GRC" | country_names$name_short == "ITA" | country_names$name_short == "JPN" | country_names$name_short == "PRT" | country_names$name_short == "ESP" | country_names$name_short == "SWE" | country_names$name_short == "CHE" | country_names$name_short == "GBR" | country_names$name_short == "USA"] <- 1
gravity_data$importer_used_in_EK <- 0 
gravity_data$exporter_used_in_EK <- 0 

for (ii in country_names$number[country_names$used_in_EK==1]) {
  
  gravity_data$importer_used_in_EK[gravity_data$importer == ii] <- 1 
  gravity_data$exporter_used_in_EK[gravity_data$exporter == ii] <- 1 
    
}

gravity_data$used_in_EK <- gravity_data$importer_used_in_EK + gravity_data$exporter_used_in_EK
gravity_data$used_in_EK[gravity_data$used_in_EK < 2] <- 0
gravity_data$used_in_EK[gravity_data$used_in_EK == 2] <- 1
remove(ii)

# Fitted values
l_fit <- lm(trade_data_log_10 ~ distance_log_10, data = subset(gravity_data, gravity_data$trade_data < 1 & gravity_data$trade_data > 0 & gravity_data$used_in_EK == 1))


# Figure 1
setwd(outputs_dir)
pdf("distance_and_normalized_import_share_EK_fig_1.pdf", width = 7, height = 5)
plot(gravity_data$distance_log_10[gravity_data$trade_data < 1 & gravity_data$trade_data > 0 & gravity_data$used_in_EK == 1], gravity_data$trade_data_log[gravity_data$trade_data < 1 & gravity_data$trade_data > 0 & gravity_data$used_in_EK == 1], pch = 19, xlab = "Distance (in miles)", ylab = "Normalized Import Share", xaxt = "n", yaxt = "n", xlim = c(2,4.25), ylim = c(-4,0))
axis(1, at = c(2,3,4), labels = c(10^2, 10^3, 10^4))
axis(2, at = c(-4,-3,-2,-1,0), labels = c(10^(-4),10^(-3),10^(-2),10^(-1),10^(0)))
lines(gravity_data$distance_log_10[gravity_data$trade_data < 1 & gravity_data$trade_data > 0 & gravity_data$used_in_EK == 1], l_fit$fitted.values, type = "l", col = "blue", lw = 2)
dev.off()

# Clean things 
remove(l_fit)

# Regression on distance
gravity_data$trade_data_log <- log(gravity_data$trade_data) 
gravity_data$distance_log <- log(gravity_data$distance)
model_1 <- lm(trade_data_log ~ distance_log, data = gravity_data[which(gravity_data$trade_data > 0 & gravity_data$trade_data < 1 & gravity_data$used_in_EK== 1), ])
remove(model_1)

# I wasn't able to open price data so I won't be able to compute the theta estimate that Eaton Kortum present
# Moving on, I try to replicate Waugh (2010) International Trade and Income Differences




