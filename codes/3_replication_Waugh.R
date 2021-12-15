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

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Different linear models 
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# Here dep. var is trade share -------------------------------------------------

# Linear model for theta
m1_for_theta <- lm(trade_share_normalized_log ~ tau_hat_log - 1, data = complete_dataset)

# Linear model without exporter or imported fixed effects 
m2_no_fe <- lm(trade_share_normalized_log ~ tau_hat_log + dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border - 1, data = complete_dataset)

# Linear model with exporter fixed effects 
m3_exporter_fe <- lm(trade_share_normalized_log ~ tau_hat_log + dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border + factor(exporter) - 1, data = complete_dataset)

# Linear model with importer fixed effects 
m4_importer_fe <- lm(trade_share_normalized_log ~ tau_hat_log + dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border + factor(importer) - 1, data = complete_dataset)

# Linear model with both importer and expoter FE
m5_both_fe <- lm(trade_share_normalized_log ~ tau_hat_log + dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border + factor(importer) + factor(exporter) - 1, data = complete_dataset)

# Table with results 
setwd(outputs_dir)
stargazer(m2_no_fe, m3_exporter_fe, m4_importer_fe, m5_both_fe, align=TRUE, title="Estimates of Theta - Different Models",keep = c("tau_hat_log", "dist_1", "dist_2", "dist_3", "dist_4", "dist_5", "dist_6", "border") , dep.var.labels = "Trade Share (in logs)", label = "models_theta", column.labels = c("No FE", "FE exporter", "FE importer", "FE both"), covariate.labels = c("Tau (in logs)", "Dist 1", "Dist 2", "Dist 3", "Dist 4", "Dist 5", "Dist 6", "Shared Border"), omit.stat=c("f", "ser", "ll"), out = "models_theta.tex")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Now I want to generate plots that correlate the fixed effects (the intercepts) and normalized GDP per worker
setwd(inputs_dir)
price_and_income <- read_excel("itid_data.xlsx",sheet = "Price and Income") 
colnames(price_and_income) <- c("Country", "Price85", "Price96", "GDPpw85", "Kpw85", "Labor85", "GDPpw96", "Kpw96", "Labor96")
price_and_income$Country <- c("USA","ARG","AUS","AUT","BEL","BEN","BGD","BOL","BRA","CAF","CAN","CHE","CHL","CHN","CMR","COL","CRI","DNK","DOM","ECU","EGY","ESP","ETH","FIN","FRA","GBR","GHA","GRC","GTM","HND","IND","IRL","IRN","ISR","ITA","JAM","JOR","JPN","KEN","KOR","LKA","MEX","MLI","MOZ","MUS","MWI","MYS","NER","NIC","NLD","NOR","NPL","NZL","PAK","PAN","PER","PHL","PNG","PRT","PRY","RWA","SEN","SLE","SLV","SWE","SYR","TGO","THA","TUN","TUR","UGA","URY","VEN","ZAF","ZAR","ZMB","ZWE")
price_and_income$GDPpw96_rel_USA <- price_and_income$GDPpw96 / price_and_income$GDPpw96[which(price_and_income$Country == "USA")]

# Extract the exporter fixed effects from m3_exporter_fe 
exporter_fe <- -m3_exporter_fe$coefficients[-c(1,2,3,4,5,6,7,8)]
names_exporter_fe <- names(exporter_fe)
numbe_exporter_fe <- unname(exporter_fe)
names_exporter_fe <- str_sub(names_exporter_fe, -3, -1)
temporal_to_merge <- as.data.frame(cbind(names_exporter_fe, numbe_exporter_fe))
exporter_fe_and_gdp <- merge(price_and_income, temporal_to_merge, "names_exporter_fe", "Country")
remove(temporal_to_merge)
exporter_fe_and_gdp$GDPpw96_rel_USA_log2 <- log2(exporter_fe_and_gdp$GDPpw96_rel_USA)

# Now plot the relationship and see what's going on 
exporter_fe_and_gdp$numbe_exporter_fe <- as.numeric(as.character(exporter_fe_and_gdp$numbe_exporter_fe))
l_fit <- lm(numbe_exporter_fe ~ GDPpw96_rel_USA_log2, data = exporter_fe_and_gdp)

# Save plot
setwd(outputs_dir)
pdf("exporter_FE_and_GDP.pdf", width = 7, height = 5)
plot(exporter_fe_and_gdp$GDPpw96_rel_USA_log2, exporter_fe_and_gdp$numbe_exporter_fe, pch = 19, xlab = "GDP per worker Relative to USA", ylab = "Exporter FE", xaxt = "n")
axis(1, at = c(-5, -4, -3, -2, -1, 0), labels = c(2^(-5), 2^(-4), 2^(-3),2^(-2), 2^(-1),2^0))
text(exporter_fe_and_gdp$GDPpw96_rel_USA_log2, exporter_fe_and_gdp$numbe_exporter_fe, labels = exporter_fe_and_gdp$Country, cex = .5, pos = 4)
lines(exporter_fe_and_gdp$GDPpw96_rel_USA_log2[-which(is.na(exporter_fe_and_gdp$numbe_exporter_fe))], l_fit$fitted.values, type = "l", col = "blue", lw = 2)
dev.off()

# Clean
remove(l_fit)

# Now repeat for importer FE 
# Extract the importer fixed effects from m4 
importer_fe <- m4_importer_fe$coefficients[-c(1,2,3,4,5,6,7,8)]
names_importer_fe <- names(importer_fe)
numbe_importer_fe <- unname(importer_fe)
names_importer_fe <- str_sub(names_importer_fe, -3, -1)
temporal_to_merge <- as.data.frame(cbind(names_importer_fe, numbe_importer_fe))
importer_fe_and_gdp <- merge(price_and_income, temporal_to_merge, "names_importer_fe", "Country")
remove(temporal_to_merge)
importer_fe_and_gdp$GDPpw96_rel_USA_log2 <- log2(importer_fe_and_gdp$GDPpw96_rel_USA)

# Now plot the relationship and see what's going on 
importer_fe_and_gdp$numbe_importer_fe <- as.numeric(as.character(importer_fe_and_gdp$numbe_importer_fe))
l_fit <- lm(numbe_importer_fe ~ GDPpw96_rel_USA_log2, data = importer_fe_and_gdp)

# Save plot
setwd(outputs_dir)
pdf("importer_FE_and_GDP.pdf", width = 7, height = 5)
plot(importer_fe_and_gdp$GDPpw96_rel_USA_log2, importer_fe_and_gdp$numbe_importer_fe, pch = 19, xlab = "GDP per worker Relative to USA", ylab = "Importer FE", xaxt = "n")
axis(1, at = c(-5, -4, -3, -2, -1, 0), labels = c(2^(-5), 2^(-4), 2^(-3),2^(-2), 2^(-1),2^0))
text(importer_fe_and_gdp$GDPpw96_rel_USA_log2, importer_fe_and_gdp$numbe_importer_fe, labels = importer_fe_and_gdp$Country, cex = .5, pos = 4)
lines(importer_fe_and_gdp$GDPpw96_rel_USA_log2[-which(is.na(importer_fe_and_gdp$numbe_importer_fe))], l_fit$fitted.values, type = "l", col = "blue", lw = 2)
dev.off()

# Clean
remove(l_fit, m1_for_theta, m2_no_fe, m3_exporter_fe, m4_importer_fe, m5_both_fe)


# Here I want to estimate four models that are similar to what Waugh has in table 2
# Linear model 

# Linear model without exporter or imported fixed effects 
m1_no_fe <- lm(trade_share_normalized_log ~  dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border - 1, data = complete_dataset)

# Linear model with exporter fixed effects 
m2_exporter_fe <- lm(trade_share_normalized_log ~  dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border + factor(exporter) - 1, data = complete_dataset)

# Linear model with importer fixed effects 
m3_exporter_fe <- lm(trade_share_normalized_log ~  dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border + factor(importer) - 1, data = complete_dataset)

# Linear model with both importer and expoter FE
m4_both_fe <- lm(trade_share_normalized_log ~  dist_1 + dist_2 + dist_3 + dist_4 + dist_5 + dist_6 + border + factor(exporter) + factor(importer) - 1, data = complete_dataset)

# Table with results 
setwd(outputs_dir)
stargazer(m1_no_fe, m2_exporter_fe, m3_exporter_fe, m4_both_fe, align=TRUE, title="Geographic Barriers",keep = c("dist_1", "dist_2", "dist_3", "dist_4", "dist_5", "dist_6", "border") , dep.var.labels = "Trade Share (in logs)", label = "models_geo", column.labels = c("No FE", "FE exporter", "FE importer", "FE both"), covariate.labels = c("Dist 1", "Dist 2", "Dist 3", "Dist 4", "Dist 5", "Dist 6", "Shared Border"), omit.stat=c("f", "ser", "ll"), out = "models_geo.tex")













# There is a problem with the measure tau_hat_log 
plot(log(complete_dataset$distance), complete_dataset$tau_hat_log)
plot(log(complete_dataset$distance[complete_dataset$importer_is_OECD == 1 & complete_dataset$exporter_is_OECD == 1]), complete_dataset$tau_hat_log[complete_dataset$importer_is_OECD == 1 & complete_dataset$exporter_is_OECD == 1])
plot(log(complete_dataset$distance[complete_dataset$importer_is_OECD == 0 & complete_dataset$exporter_is_OECD == 0]), complete_dataset$tau_hat_log[complete_dataset$importer_is_OECD == 0 & complete_dataset$exporter_is_OECD == 0])





