# ------------------------------------------------------------------------------
# Replication - Diagnosis
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
library("stringr", quietly = TRUE)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd(inputs_dir)
complete_dataset <- read.csv("dataset_in_panel_form.csv")

# Generate distance dummies
complete_dataset$dist_1 = 0
complete_dataset$dist_1[complete_dataset$distance<375] = 1
complete_dataset$dist_2 = 0 
complete_dataset$dist_2[complete_dataset$distance>=375 & complete_dataset$distance <750] = 1
complete_dataset$dist_3 = 0 
complete_dataset$dist_3[complete_dataset$distance>=750 & complete_dataset$distance <1500] = 1
complete_dataset$dist_4 = 0 
complete_dataset$dist_4[complete_dataset$distance>=1500 & complete_dataset$distance <3000] = 1
complete_dataset$dist_5 = 0 
complete_dataset$dist_5[complete_dataset$distance>=3000 & complete_dataset$distance <6000] = 1
complete_dataset$dist_6 = 0 
complete_dataset$dist_6[complete_dataset$distance>=6000] = 1

# Possible Problem 1 - I think that tau hat doesn't behave well when correlating with distance
complete_dataset$distance_logs <- log(complete_dataset$distance)
l_fit <- lm(tau_hat_log ~ distance_logs, data = complete_dataset)

setwd(outputs_dir)
pdf("distance_and_tau_hat.pdf", width = 7, height = 5)
plot(complete_dataset$distance_logs, complete_dataset$tau_hat_log, pch = 1, xlab = "Distance (in logs)", ylab = "Tau (in logs)")
lines(complete_dataset$distance_logs, l_fit$fitted.values, type = "l", col = "blue", lw = 2)
dev.off()

# Possible Problem 2 - Maybe it does not correlate well when using trade shares 
  # Here I want to replicate figure 2 of eaton kortum 
  # I think I get something that makes sense
l_fit <- lm(trade_share_normalized_log ~ tau_hat_log, data = subset(complete_dataset, complete_dataset$exporter_is_OECD == 1 & complete_dataset$importer_is_OECD ==1))
setwd(outputs_dir)
pdf("tau_and_norm_trade_share_OECD.pdf", width = 7, height = 5)
plot(complete_dataset$tau_hat_log[complete_dataset$exporter_is_OECD == 1 & complete_dataset$importer_is_OECD ==1], complete_dataset$trade_share_normalized_log[complete_dataset$exporter_is_OECD == 1 & complete_dataset$importer_is_OECD ==1], pch = 1, xlab = "Tau (in logs)", ylab = "Normalized Trade Share (in logs)")
lines(complete_dataset$tau_hat_log[complete_dataset$exporter_is_OECD == 1 & complete_dataset$importer_is_OECD ==1], l_fit$fitted.values, type = "l", col = "blue", lw = 2)
dev.off()

# Possible Problem 3 - Sample is not the same
  # What if I focus only on OECD countries 
# Linear model without exporter or imported fixed effects 
m1_no_fe <- lm(trade_share_normalized_log ~  dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border - 1, data = subset(complete_dataset, complete_dataset$importer_is_OECD == 1 & complete_dataset$exporter_is_OECD == 1))

# Linear model with exporter fixed effects 
m2_exporter_fe <- lm(trade_share_normalized_log ~  dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border + factor(exporter) - 1, data = subset(complete_dataset, complete_dataset$importer_is_OECD == 1 & complete_dataset$exporter_is_OECD == 1))

# Linear model with importer fixed effects 
m3_exporter_fe <- lm(trade_share_normalized_log ~  dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border + factor(importer) - 1, data = subset(complete_dataset, complete_dataset$importer_is_OECD == 1 & complete_dataset$exporter_is_OECD == 1))

# Linear model with both importer and expoter FE
m4_both_fe <- lm(trade_share_normalized_log ~  dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border + factor(exporter) + factor(importer) - 1, data = subset(complete_dataset, complete_dataset$importer_is_OECD == 1 & complete_dataset$exporter_is_OECD == 1))

# Store results 
setwd(outputs_dir)
stargazer(m1_no_fe, m2_exporter_fe, m3_exporter_fe, m4_both_fe, align=TRUE, title="Geographic Barriers",keep = c("dist_1", "dist_2", "dist_3", "dist_4", "dist_5", "dist_6", "border") , dep.var.labels = "Trade Share (in logs)", label = "models_geo_OECD", column.labels = c("No FE", "FE exporter", "FE importer", "FE both"), covariate.labels = c("Dist 1", "Dist 2", "Dist 3", "Dist 4", "Dist 5", "Dist 6", "Shared Border"), omit.stat=c("f", "ser", "ll"), out = "models_geo_OECD.tex")

