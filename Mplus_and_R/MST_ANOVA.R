# Set working directory
setwd("N:/durable/Students/Corona research project/Rådata og metadata")
# Read in MST dataset
mst <- data.table::fread("MST.dat", header = FALSE, na.strings = "-999")
# Retain used variables only
mst <- mst[, c(1, 3:5, 9:11, 15:19, 25, 34, 40, 49, 55, 61, 67, 73:74)]
# Assign names to columns
names(mst) <- c(
    "agecoh", "mid", "north", "age", "immi1", "immi2", "female",
    "tamr", "prior", "insti", "foster", "psyc",
    "bhv0", "rsk0", "bhv1", "rsk1",
    "bhv6", "bhv12", "bhv18", "chgbhv", "chgrsk"
)

#########################
# Study imperfect cases #
#########################

# Retain only complete cases
mst_complete <- mst[complete.cases(mst)]
# How many cases contain missing data?
(n_missing <- nrow(mst) - nrow(mst_complete))
# Percentage of missing cases relative to total
(percent_missing <- n_missing / nrow(mst) * 100)

#################################################
# ANOVA for group differences at admission (T0) #
#################################################

# Turn binary variables into factors
mst$agecoh <- as.factor(mst$agecoh)
# Create a placeholder array
temp <- data.frame(matrix(NA, nrow = 13, ncol = 4))
# Assign column names
names(temp) <- c("df_1", "df_2", "F-stat", "p-value")
# Assign row names
row.names(temp) <- c(
    "bhv0", "rsk0",
    "mid", "north",
    "age", "female", "immi1", "immi2",
    "tamr", "prior", "insti", "foster", "psyc"
)

# Outcome variables
temp[1, 1] <- unlist(summary(aov(bhv0 ~ agecoh, data = mst)))[1]
temp[1, 2] <- unlist(summary(aov(bhv0 ~ agecoh, data = mst)))[2]
temp[1, 3] <- unlist(summary(aov(bhv0 ~ agecoh, data = mst)))[7]
temp[1, 4] <- unlist(summary(aov(bhv0 ~ agecoh, data = mst)))[9]

temp[2, 1] <- unlist(summary(aov(rsk0 ~ agecoh, data = mst)))[1]
temp[2, 2] <- unlist(summary(aov(rsk0 ~ agecoh, data = mst)))[2]
temp[2, 3] <- unlist(summary(aov(rsk0 ~ agecoh, data = mst)))[7]
temp[2, 4] <- unlist(summary(aov(rsk0 ~ agecoh, data = mst)))[9]

# Geographics
temp[3, 1] <- unlist(summary(aov(mid ~ agecoh, data = mst)))[1]
temp[3, 2] <- unlist(summary(aov(mid ~ agecoh, data = mst)))[2]
temp[3, 3] <- unlist(summary(aov(mid ~ agecoh, data = mst)))[7]
temp[3, 4] <- unlist(summary(aov(mid ~ agecoh, data = mst)))[9]

temp[4, 1] <- unlist(summary(aov(north ~ agecoh, data = mst)))[1]
temp[4, 2] <- unlist(summary(aov(north ~ agecoh, data = mst)))[2]
temp[4, 3] <- unlist(summary(aov(north ~ agecoh, data = mst)))[7]
temp[4, 4] <- unlist(summary(aov(north ~ agecoh, data = mst)))[9]

# Demographics
temp[5, 1] <- unlist(summary(aov(age ~ agecoh, data = mst)))[1]
temp[5, 2] <- unlist(summary(aov(age ~ agecoh, data = mst)))[2]
temp[5, 3] <- unlist(summary(aov(age ~ agecoh, data = mst)))[7]
temp[5, 4] <- unlist(summary(aov(age ~ agecoh, data = mst)))[9]

temp[6, 1] <- unlist(summary(aov(female ~ agecoh, data = mst)))[1]
temp[6, 2] <- unlist(summary(aov(female ~ agecoh, data = mst)))[2]
temp[6, 3] <- unlist(summary(aov(female ~ agecoh, data = mst)))[7]
temp[6, 4] <- unlist(summary(aov(female ~ agecoh, data = mst)))[9]

temp[7, 1] <- unlist(summary(aov(immi1 ~ agecoh, data = mst)))[1]
temp[7, 2] <- unlist(summary(aov(immi1 ~ agecoh, data = mst)))[2]
temp[7, 3] <- unlist(summary(aov(immi1 ~ agecoh, data = mst)))[7]
temp[7, 4] <- unlist(summary(aov(immi1 ~ agecoh, data = mst)))[9]

temp[8, 1] <- unlist(summary(aov(immi2 ~ agecoh, data = mst)))[1]
temp[8, 2] <- unlist(summary(aov(immi2 ~ agecoh, data = mst)))[2]
temp[8, 3] <- unlist(summary(aov(immi2 ~ agecoh, data = mst)))[7]
temp[8, 4] <- unlist(summary(aov(immi2 ~ agecoh, data = mst)))[9]

# Treatment variables
temp[9, 1] <- unlist(summary(aov(tamr ~ agecoh, data = mst)))[1]
temp[9, 2] <- unlist(summary(aov(tamr ~ agecoh, data = mst)))[2]
temp[9, 3] <- unlist(summary(aov(tamr ~ agecoh, data = mst)))[7]
temp[9, 4] <- unlist(summary(aov(tamr ~ agecoh, data = mst)))[9]

temp[10, 1] <- unlist(summary(aov(prior ~ agecoh, data = mst)))[1]
temp[10, 2] <- unlist(summary(aov(prior ~ agecoh, data = mst)))[2]
temp[10, 3] <- unlist(summary(aov(prior ~ agecoh, data = mst)))[7]
temp[10, 4] <- unlist(summary(aov(prior ~ agecoh, data = mst)))[9]

temp[11, 1] <- unlist(summary(aov(insti ~ agecoh, data = mst)))[1]
temp[11, 2] <- unlist(summary(aov(insti ~ agecoh, data = mst)))[2]
temp[11, 3] <- unlist(summary(aov(insti ~ agecoh, data = mst)))[7]
temp[11, 4] <- unlist(summary(aov(insti ~ agecoh, data = mst)))[9]

temp[12, 1] <- unlist(summary(aov(foster ~ agecoh, data = mst)))[1]
temp[12, 2] <- unlist(summary(aov(foster ~ agecoh, data = mst)))[2]
temp[12, 3] <- unlist(summary(aov(foster ~ agecoh, data = mst)))[7]
temp[12, 4] <- unlist(summary(aov(foster ~ agecoh, data = mst)))[9]

temp[13, 1] <- unlist(summary(aov(psyc ~ agecoh, data = mst)))[1]
temp[13, 2] <- unlist(summary(aov(psyc ~ agecoh, data = mst)))[2]
temp[13, 3] <- unlist(summary(aov(psyc ~ agecoh, data = mst)))[7]
temp[13, 4] <- unlist(summary(aov(psyc ~ agecoh, data = mst)))[9]

# Round F-statistics and p-values
temp[, 3] <- round(temp[, 3], 2)
temp[, 4] <- round(temp[, 4], 3)
# Display summary table
temp

###############################
# Plot effectiveness measures #
###############################

# Frequency tables for behavioural changes
table(unlist(mst$chgbhv)) # Full sample
table(unlist(mst$chgbhv[which(mst$agecoh == 1)])) # Before
table(unlist(mst$chgbhv[which(mst$agecoh == 2)])) # During
table(unlist(mst$chgbhv[which(mst$agecoh == 3)])) # After

# Frequency tables for risk reduction
table(unlist(mst$chgrsk)) # Full sample
table(unlist(mst$chgrsk[which(mst$agecoh == 1)])) # Before
table(unlist(mst$chgrsk[which(mst$agecoh == 2)])) # During
table(unlist(mst$chgrsk[which(mst$agecoh == 3)])) # After

# Activate package for mimicking distribution by moments
library(PearsonDS)

pdf("MST_effectiveness.pdf", paper = "a4")
# Reset canvas to 3-row by 2-column. Plot top-down, then left-right
par(mfcol = c(3, 2))
# Ret common scales
sum_common_x <- c(-3, 5)
sum_common_break <- 8
x_seq <- seq(min(sum_common_x), max(sum_common_x), 0.1)

hist(mst$chgbhv[which(mst$agecoh == 1)],
    xlim = sum_common_x,
    breaks = sum_common_break,
    freq = FALSE,
    xlab = "BEFORE", main = "MST Behavioral Improvement"
)
# Red zero line
abline(v = 0, col = "red")
# Blue distribution curve
# Superimpose on histogram
par(new = TRUE)
# Extract "before"
before <- mst$chgbhv[which(mst$agecoh == 1)]
# Discard NAs in "before"
before <- before[!is.na(before)]
plot(
    # Series of x
    x_seq,
    # Series of y
    dpearson(
        x_seq,
        moments = empMoments(before)
    ),
    xlab = "", ylab = "", axes = FALSE, # Turn off labels and axes
    type = "l", col = "blue" # Line style and colour
)

hist(mst$chgbhv[which(mst$agecoh == 2)],
    xlim = sum_common_x,
    breaks = sum_common_break, freq = FALSE,
    xlab = "DURING", main = ""
)
abline(v = 0, col = "red")
par(new = TRUE)
during <- mst$chgbhv[which(mst$agecoh == 2)]
during <- during[!is.na(during)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(during)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)

hist(mst$chgbhv[which(mst$agecoh == 3)],
    xlim = sum_common_x,
    breaks = sum_common_break, freq = FALSE,
    xlab = "AFTER", main = ""
)
abline(v = 0, col = "red")
par(new = TRUE)
after <- mst$chgbhv[which(mst$agecoh == 3)]
after <- after[!is.na(after)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(after)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)

yls_common_x <- c(-10, 30)
yls_common_break <- 20
x_seq <- seq(min(yls_common_x), max(yls_common_x), 0.1)

hist(mst$chgrsk[which(mst$agecoh == 1)],
    xlim = yls_common_x,
    breaks = yls_common_break, freq = FALSE,
    xlab = "BEFORE", main = "MST Risk Reduction"
)
abline(v = 0, col = "red")
par(new = TRUE)
before <- mst$chgrsk[which(mst$agecoh == 1)]
before <- before[!is.na(before)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(before)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)
hist(mst$chgrsk[which(mst$agecoh == 2)],
    xlim = yls_common_x,
    breaks = yls_common_break, freq = FALSE,
    xlab = "DURING", main = ""
)
abline(v = 0, col = "red")
par(new = TRUE)
during <- mst$chgrsk[which(mst$agecoh == 2)]
during <- during[!is.na(during)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(during)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)
hist(mst$chgrsk[which(mst$agecoh == 3)],
    xlim = yls_common_x,
    breaks = yls_common_break, freq = FALSE,
    xlab = "AFTER", main = ""
)
abline(v = 0, col = "red")
par(new = TRUE)
after <- mst$chgrsk[which(mst$agecoh == 3)]
after <- after[!is.na(after)]
plot(
    x_seq,
    dpearson(
        x_seq,
        moments = empMoments(after)
    ),
    xlab = "", ylab = "",
    type = "l", col = "blue", axes = FALSE
)
# Restore canvas
par(mfcol = c(1, 1))
dev.off()
