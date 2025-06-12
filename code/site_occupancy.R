#################################
### Nest occupancy estimation ###
#################################


library(dplyr)

sites <- read.csv("data/site_detailsII.csv")

rownames(sites) <- sites$site

sites$extra <- sites$dateofextraboxes

sites$extra <- c(rep(2017,3), NA, 2017, NA, 2017, 2017, NA, rep(2017, 5), NA, rep(2017, 6), NA, NA, 2017, 2017, NA, 2017, 2017, NA, 2017, 2017, NA, rep(2017, 8), NA, 2017, NA, NA)

sites["BAD", c("X2015", "X2016")] <- 1
sites["MCH", "X2015"] <- 1
sites["PTH", "X2016"] <- 1
sites["BIR", "X2015"] <- 1
sites["DUN", "X2016"] <- 1
sites["DLW", "X2020"] <- 1
sites["CRU", "X2020"] <- 1
sites[25-44, "X2020"] <- 1
sites[38-44, "X2021"] <- 1
sites["TAI", "X2015"] <- 1

sites$X2022 <- 1
sites$X2023 <- 1
sites$X2024 <- 1

sites <- sites %>%  relocate(X2022:X2024, .before = extras) 

av_occupancy <- data.frame(site = sites$site, occupancy = rep(NA, nrow(sites)))


# EDI

EDI_nb <- data.frame(year = c(2014:2021), site = rep("EDI", 8), all_nb = seq(1,8,1), occ_nb = rep(NA, 8))

for (i in 1:nrow(EDI_nb)) {
  if (is.na(sites["EDI", "extra"])) {
    EDI_nb[i,"all_nb"] <- sites["EDI","Current.Boxes"]
  } else if (EDI_nb[i, "year"] >= sites["EDI", "extra"]) {
    EDI_nb[i,"all_nb"] <- sites["EDI","Current.Boxes"]
  } else if (EDI_nb[i, "year"] < sites["EDI", "extra"]) {
    EDI_nb[i,"all_nb"] <- sites["EDI","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["EDI",i])) {
    EDI_nb[row, "all_nb"] <- NA
  }
}

blutiEDI <- blutidf[which(blutidf$site == "EDI"),] %>% count(year)

for (i in 1:nrow(EDI_nb)) {
  if (EDI_nb[i,"year"] %in% blutiEDI$year) {
    EDI_nb[i,"occ_nb"] <- blutiEDI[which(blutiEDI$year == EDI_nb[i,"year"]),"n"]
  } else {
    EDI_nb[i,"occ_nb"] <- 0
  }
}

EDI_nb$occupancy <- EDI_nb$occ_nb/EDI_nb$all_nb

av_occupancy[which(av_occupancy$site == "EDI"),"occupancy"] <- mean(na.omit(EDI_nb$occupancy))

# RSY site

RSY_nb <- data.frame(year = c(2014:2024), site = rep("RSY", 11), all_nb = seq(1,11,1), occ_nb = rep(0, 11))

for (i in 1:nrow(RSY_nb)) {
  if (RSY_nb[i, "year"] < sites["RSY", "extra"]) {
    RSY_nb[i,"all_nb"] <- sites["RSY","Original.Boxes"]
  } else if (RSY_nb[i, "year"] >= sites["RSY", "extra"]) {
    RSY_nb[i,"all_nb"] <- sites["RSY","Current.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["RSY",i])) {
    RSY_nb[row, "all_nb"] <- NA
  }
}

blutiRSY <- blutidf[which(blutidf$site == "RSY"),] %>% count(year)

for (i in 1:nrow(RSY_nb)) {
  if (RSY_nb[i,"year"] %in% blutiRSY$year) {
    RSY_nb[i,"occ_nb"] <- blutiRSY[which(blutiRSY$year == RSY_nb[i,"year"]),"n"]
  } else {
    RSY_nb[i,"occ_nb"] <- 0
  }
}

RSY_nb$occupancy <- RSY_nb$occ_nb/RSY_nb$all_nb 

av_occupancy[which(av_occupancy$site == "RSY"),"occupancy"] <- mean(na.omit(RSY_nb$occupancy))


# FOF site

FOF_nb <- data.frame(year = c(2014:2024), site = rep("FOF", 11), all_nb = seq(1,11,1), occ_nb = rep(0, 11))

for (i in 1:nrow(FOF_nb)) {
  if (FOF_nb[i, "year"] < sites["FOF", "extra"]) {
    FOF_nb[i,"all_nb"] <- sites["FOF","Original.Boxes"]
  } else if (FOF_nb[i, "year"] >= sites["FOF", "extra"]) {
    FOF_nb[i,"all_nb"] <- sites["FOF","Current.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["FOF",i])) {
    FOF_nb[row, "all_nb"] <- NA
  }
}

blutiFOF <- blutidf[which(blutidf$site == "FOF"),] %>% count(year)

for (i in 1:nrow(FOF_nb)) {
  if (FOF_nb[i,"year"] %in% blutiFOF$year) {
    FOF_nb[i,"occ_nb"] <- blutiFOF[which(blutiFOF$year == FOF_nb[i,"year"]),"n"]
  } else {
    FOF_nb[i,"occ_nb"] <- 0
  }
}

FOF_nb$occupancy <- FOF_nb$occ_nb/FOF_nb$all_nb 

av_occupancy[which(av_occupancy$site == "FOF"),"occupancy"] <- mean(na.omit(FOF_nb$occupancy))

# BAD site - occupancy estimation might be wrong as there are breeding recordings even though, according to sites.csv, nestboxes were missing

BAD_nb <- data.frame(year = c(2014:2024), site = rep("BAD", 11), all_nb = seq(1,11,1), occ_nb = rep(0, 11))

for (i in 1:nrow(BAD_nb)) {
  if (is.na(sites["BAD", "extra"])) {
    BAD_nb[i,"all_nb"] <- sites["BAD","Current.Boxes"]
  } else if (BAD_nb[i, "year"] >= sites["BAD", "extra"]) {
    BAD_nb[i,"all_nb"] <- sites["BAD","Current.Boxes"]
  } else if (BAD_nb[i, "year"] < sites["BAD", "extra"]) {
    BAD_nb[i,"all_nb"] <- sites["BAD","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["BAD",i])) {
    BAD_nb[row, "all_nb"] <- NA
  }
}

blutiBAD <- blutidf[which(blutidf$site == "BAD"),] %>% count(year)

for (i in 1:nrow(BAD_nb)) {
  if (BAD_nb[i,"year"] %in% blutiBAD$year) {
    BAD_nb[i,"occ_nb"] <- blutiBAD[which(blutiBAD$year == BAD_nb[i,"year"]),"n"]
  } else {
    BAD_nb[i,"occ_nb"] <- 0
  }
}

BAD_nb$occupancy <- BAD_nb$occ_nb/BAD_nb$all_nb 

av_occupancy[which(av_occupancy$site == "BAD"),"occupancy"] <- mean(na.omit(BAD_nb$occupancy))

# LVN site

LVN_nb <- data.frame(year = c(2014:2024), site = rep("LVN", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(LVN_nb)) {
  if (is.na(sites["LVN", "extra"])) {
    LVN_nb[i,"all_nb"] <- sites["LVN","Current.Boxes"]
  } else if (LVN_nb[i, "year"] >= sites["LVN", "extra"]) {
    LVN_nb[i,"all_nb"] <- sites["LVN","Current.Boxes"]
  } else if (LVN_nb[i, "year"] < sites["LVN", "extra"]) {
    LVN_nb[i,"all_nb"] <- sites["LVN","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["LVN",i])) {
    LVN_nb[row, "all_nb"] <- NA
  }
}

blutiLVN <- blutidf[which(blutidf$site == "LVN"),] %>% count(year)

for (i in 1:nrow(LVN_nb)) {
  if (LVN_nb[i,"year"] %in% blutiLVN$year) {
    LVN_nb[i,"occ_nb"] <- blutiLVN[which(blutiLVN$year == LVN_nb[i,"year"]),"n"]
  } else {
    LVN_nb[i,"occ_nb"] <- 0
  }
}

LVN_nb$occupancy <- LVN_nb$occ_nb/LVN_nb$all_nb

av_occupancy[which(av_occupancy$site == "LVN"),"occupancy"] <- mean(na.omit(LVN_nb$occupancy))

# DOW site

DOW_nb <- data.frame(year = c(2014:2024), site = rep("DOW", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(DOW_nb)) {
  if (is.na(sites["DOW", "extra"])) {
    DOW_nb[i,"all_nb"] <- sites["DOW","Current.Boxes"]
  } else if (DOW_nb[i, "year"] >= sites["DOW", "extra"]) {
    DOW_nb[i,"all_nb"] <- sites["DOW","Current.Boxes"]
  } else if (DOW_nb[i, "year"] < sites["DOW", "extra"]) {
    DOW_nb[i,"all_nb"] <- sites["DOW","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["DOW",i])) {
    DOW_nb[row, "all_nb"] <- NA
  }
}

blutiDOW <- blutidf[which(blutidf$site == "DOW"),] %>% count(year)

for (i in 1:nrow(DOW_nb)) {
  if (DOW_nb[i,"year"] %in% blutiDOW$year) {
    DOW_nb[i,"occ_nb"] <- blutiDOW[which(blutiDOW$year == DOW_nb[i,"year"]),"n"]
  } else {
    DOW_nb[i,"occ_nb"] <- 0
  }
}

DOW_nb$occupancy <- DOW_nb$occ_nb/DOW_nb$all_nb

av_occupancy[which(av_occupancy$site == "DOW"),"occupancy"] <- mean(na.omit(DOW_nb$occupancy))

# GLF site

GLF_nb <- data.frame(year = c(2014:2024), site = rep("GLF", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(GLF_nb)) {
  if (is.na(sites["GLF", "extra"])) {
    GLF_nb[i,"all_nb"] <- sites["GLF","Current.Boxes"]
  } else if (GLF_nb[i, "year"] >= sites["GLF", "extra"]) {
    GLF_nb[i,"all_nb"] <- sites["GLF","Current.Boxes"]
  } else if (GLF_nb[i, "year"] < sites["GLF", "extra"]) {
    GLF_nb[i,"all_nb"] <- sites["GLF","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["GLF",i])) {
    GLF_nb[row, "all_nb"] <- NA
  }
}

blutiGLF <- blutidf[which(blutidf$site == "GLF"),] %>% count(year)

for (i in 1:nrow(GLF_nb)) {
  if (GLF_nb[i,"year"] %in% blutiGLF$year) {
    GLF_nb[i,"occ_nb"] <- blutiGLF[which(blutiGLF$year == GLF_nb[i,"year"]),"n"]
  } else {
    GLF_nb[i,"occ_nb"] <- 0
  }
}

GLF_nb$occupancy <- GLF_nb$occ_nb/GLF_nb$all_nb

av_occupancy[which(av_occupancy$site == "GLF"),"occupancy"] <- mean(na.omit(GLF_nb$occupancy))

# SER site

SER_nb <- data.frame(year = c(2014:2024), site = rep("SER", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(SER_nb)) {
  if (is.na(sites["SER", "extra"])) {
    SER_nb[i,"all_nb"] <- sites["SER","Current.Boxes"]
  } else if (SER_nb[i, "year"] >= sites["SER", "extra"]) {
    SER_nb[i,"all_nb"] <- sites["SER","Current.Boxes"]
  } else if (SER_nb[i, "year"] < sites["SER", "extra"]) {
    SER_nb[i,"all_nb"] <- sites["SER","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["SER",i])) {
    SER_nb[row, "all_nb"] <- NA
  }
}

blutiSER <- blutidf[which(blutidf$site == "SER"),] %>% count(year)

for (i in 1:nrow(SER_nb)) {
  if (SER_nb[i,"year"] %in% blutiSER$year) {
    SER_nb[i,"occ_nb"] <- blutiSER[which(blutiSER$year == SER_nb[i,"year"]),"n"]
  } else {
    SER_nb[i,"occ_nb"] <- 0
  }
}

SER_nb$occupancy <- SER_nb$occ_nb/SER_nb$all_nb

av_occupancy[which(av_occupancy$site == "SER"),"occupancy"] <- mean(na.omit(SER_nb$occupancy))

# MCH site - MIGHT BE A WRONG ESTIMATION; SAME REASON AS FOR BAD

MCH_nb <- data.frame(year = c(2014:2024), site = rep("MCH", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(MCH_nb)) {
  if (is.na(sites["MCH", "extra"])) {
    MCH_nb[i,"all_nb"] <- sites["MCH","Current.Boxes"]
  } else if (MCH_nb[i, "year"] >= sites["MCH", "extra"]) {
    MCH_nb[i,"all_nb"] <- sites["MCH","Current.Boxes"]
  } else if (MCH_nb[i, "year"] < sites["MCH", "extra"]) {
    MCH_nb[i,"all_nb"] <- sites["MCH","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["MCH",i])) {
    MCH_nb[row, "all_nb"] <- NA
  }
}

blutiMCH <- blutidf[which(blutidf$site == "MCH"),] %>% count(year)

for (i in 1:nrow(MCH_nb)) {
  if (MCH_nb[i,"year"] %in% blutiMCH$year) {
    MCH_nb[i,"occ_nb"] <- blutiMCH[which(blutiMCH$year == MCH_nb[i,"year"]),"n"]
  } else {
    MCH_nb[i,"occ_nb"] <- 0
  }
}

MCH_nb$occupancy <- MCH_nb$occ_nb/MCH_nb$all_nb

av_occupancy[which(av_occupancy$site == "MCH"),"occupancy"] <- mean(na.omit(MCH_nb$occupancy))

# PTH site

PTH_nb <- data.frame(year = c(2014:2024), site = rep("PTH", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(PTH_nb)) {
  if (is.na(sites["PTH", "extra"])) {
    PTH_nb[i,"all_nb"] <- sites["PTH","Current.Boxes"]
  } else if (PTH_nb[i, "year"] >= sites["PTH", "extra"]) {
    PTH_nb[i,"all_nb"] <- sites["PTH","Current.Boxes"]
  } else if (PTH_nb[i, "year"] < sites["PTH", "extra"]) {
    PTH_nb[i,"all_nb"] <- sites["PTH","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["PTH",i])) {
    PTH_nb[row, "all_nb"] <- NA
  }
}

blutiPTH <- blutidf[which(blutidf$site == "PTH"),] %>% count(year)

for (i in 1:nrow(PTH_nb)) {
  if (PTH_nb[i,"year"] %in% blutiPTH$year) {
    PTH_nb[i,"occ_nb"] <- blutiPTH[which(blutiPTH$year == PTH_nb[i,"year"]),"n"]
  } else {
    PTH_nb[i,"occ_nb"] <- 0
  }
}

PTH_nb$occupancy <- PTH_nb$occ_nb/PTH_nb$all_nb

av_occupancy[which(av_occupancy$site == "PTH"),"occupancy"] <- mean(na.omit(PTH_nb$occupancy))

# STY site

STY_nb <- data.frame(year = c(2014:2024), site = rep("STY", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(STY_nb)) {
  if (is.na(sites["STY", "extra"])) {
    STY_nb[i,"all_nb"] <- sites["STY","Current.Boxes"]
  } else if (STY_nb[i, "year"] >= sites["STY", "extra"]) {
    STY_nb[i,"all_nb"] <- sites["STY","Current.Boxes"]
  } else if (STY_nb[i, "year"] < sites["STY", "extra"]) {
    STY_nb[i,"all_nb"] <- sites["STY","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["STY",i])) {
    STY_nb[row, "all_nb"] <- NA
  }
}

blutiSTY <- blutidf[which(blutidf$site == "STY"),] %>% count(year)

for (i in 1:nrow(STY_nb)) {
  if (STY_nb[i,"year"] %in% blutiSTY$year) {
    STY_nb[i,"occ_nb"] <- blutiSTY[which(blutiSTY$year == STY_nb[i,"year"]),"n"]
  } else {
    STY_nb[i,"occ_nb"] <- 0
  }
}

STY_nb$occupancy <- STY_nb$occ_nb/STY_nb$all_nb

av_occupancy[which(av_occupancy$site == "STY"),"occupancy"] <- mean(na.omit(STY_nb$occupancy))

# BIR site

BIR_nb <- data.frame(year = c(2014:2024), site = rep("BIR", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(BIR_nb)) {
  if (is.na(sites["BIR", "extra"])) {
    BIR_nb[i,"all_nb"] <- sites["BIR","Current.Boxes"]
  } else if (BIR_nb[i, "year"] >= sites["BIR", "extra"]) {
    BIR_nb[i,"all_nb"] <- sites["BIR","Current.Boxes"]
  } else if (BIR_nb[i, "year"] < sites["BIR", "extra"]) {
    BIR_nb[i,"all_nb"] <- sites["BIR","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["BIR",i])) {
    BIR_nb[row, "all_nb"] <- NA
  }
}

blutiBIR <- blutidf[which(blutidf$site == "BIR"),] %>% count(year)

for (i in 1:nrow(BIR_nb)) {
  if (BIR_nb[i,"year"] %in% blutiBIR$year) {
    BIR_nb[i,"occ_nb"] <- blutiBIR[which(blutiBIR$year == BIR_nb[i,"year"]),"n"]
  } else {
    BIR_nb[i,"occ_nb"] <- 0
  }
}

BIR_nb$occupancy <- BIR_nb$occ_nb/BIR_nb$all_nb

av_occupancy[which(av_occupancy$site == "BIR"),"occupancy"] <- mean(na.omit(BIR_nb$occupancy))

# DUN site

DUN_nb <- data.frame(year = c(2014:2024), site = rep("DUN", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(DUN_nb)) {
  if (is.na(sites["DUN", "extra"])) {
    DUN_nb[i,"all_nb"] <- sites["DUN","Current.Boxes"]
  } else if (DUN_nb[i, "year"] >= sites["DUN", "extra"]) {
    DUN_nb[i,"all_nb"] <- sites["DUN","Current.Boxes"]
  } else if (DUN_nb[i, "year"] < sites["DUN", "extra"]) {
    DUN_nb[i,"all_nb"] <- sites["DUN","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["DUN",i])) {
    DUN_nb[row, "all_nb"] <- NA
  }
}

blutiDUN <- blutidf[which(blutidf$site == "DUN"),] %>% count(year)

for (i in 1:nrow(DUN_nb)) {
  if (DUN_nb[i,"year"] %in% blutiDUN$year) {
    DUN_nb[i,"occ_nb"] <- blutiDUN[which(blutiDUN$year == DUN_nb[i,"year"]),"n"]
  } else {
    DUN_nb[i,"occ_nb"] <- 0
  }
}

DUN_nb$occupancy <- DUN_nb$occ_nb/DUN_nb$all_nb

av_occupancy[which(av_occupancy$site == "DUN"),"occupancy"] <- mean(na.omit(DUN_nb$occupancy))

# BLG site

BLG_nb <- data.frame(year = c(2014:2024), site = rep("BLG", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(BLG_nb)) {
  if (is.na(sites["BLG", "extra"])) {
    BLG_nb[i,"all_nb"] <- sites["BLG","Current.Boxes"]
  } else if (BLG_nb[i, "year"] >= sites["BLG", "extra"]) {
    BLG_nb[i,"all_nb"] <- sites["BLG","Current.Boxes"]
  } else if (BLG_nb[i, "year"] < sites["BLG", "extra"]) {
    BLG_nb[i,"all_nb"] <- sites["BLG","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["BLG",i])) {
    BLG_nb[row, "all_nb"] <- NA
  }
}

blutiBLG <- blutidf[which(blutidf$site == "BLG"),] %>% count(year)

for (i in 1:nrow(BLG_nb)) {
  if (BLG_nb[i,"year"] %in% blutiBLG$year) {
    BLG_nb[i,"occ_nb"] <- blutiBLG[which(blutiBLG$year == BLG_nb[i,"year"]),"n"]
  } else {
    BLG_nb[i,"occ_nb"] <- 0
  }
}

BLG_nb$occupancy <- BLG_nb$occ_nb/BLG_nb$all_nb

av_occupancy[which(av_occupancy$site == "BLG"),"occupancy"] <- mean(na.omit(BLG_nb$occupancy))

# PIT site

PIT_nb <- data.frame(year = c(2014:2024), site = rep("PIT", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(PIT_nb)) {
  if (is.na(sites["PIT", "extra"])) {
    PIT_nb[i,"all_nb"] <- sites["PIT","Current.Boxes"]
  } else if (PIT_nb[i, "year"] >= sites["PIT", "extra"]) {
    PIT_nb[i,"all_nb"] <- sites["PIT","Current.Boxes"]
  } else if (PIT_nb[i, "year"] < sites["PIT", "extra"]) {
    PIT_nb[i,"all_nb"] <- sites["PIT","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["PIT",i])) {
    PIT_nb[row, "all_nb"] <- NA
  }
}

blutiPIT <- blutidf[which(blutidf$site == "PIT"),] %>% count(year)

for (i in 1:nrow(PIT_nb)) {
  if (PIT_nb[i,"year"] %in% blutiPIT$year) {
    PIT_nb[i,"occ_nb"] <- blutiPIT[which(blutiPIT$year == PIT_nb[i,"year"]),"n"]
  } else {
    PIT_nb[i,"occ_nb"] <- 0
  }
}

PIT_nb$occupancy <- PIT_nb$occ_nb/PIT_nb$all_nb

av_occupancy[which(av_occupancy$site == "PIT"),"occupancy"] <- mean(na.omit(PIT_nb$occupancy))

# KCK site

KCK_nb <- data.frame(year = c(2014:2024), site = rep("KCK", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(KCK_nb)) {
  if (is.na(sites["KCK", "extra"])) {
    KCK_nb[i,"all_nb"] <- sites["KCK","Current.Boxes"]
  } else if (KCK_nb[i, "year"] >= sites["KCK", "extra"]) {
    KCK_nb[i,"all_nb"] <- sites["KCK","Current.Boxes"]
  } else if (KCK_nb[i, "year"] < sites["KCK", "extra"]) {
    KCK_nb[i,"all_nb"] <- sites["KCK","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["KCK",i])) {
    KCK_nb[row, "all_nb"] <- NA
  }
}

blutiKCK <- blutidf[which(blutidf$site == "KCK"),] %>% count(year)

for (i in 1:nrow(KCK_nb)) {
  if (KCK_nb[i,"year"] %in% blutiKCK$year) {
    KCK_nb[i,"occ_nb"] <- blutiKCK[which(blutiKCK$year == KCK_nb[i,"year"]),"n"]
  } else {
    KCK_nb[i,"occ_nb"] <- 0
  }
}

KCK_nb$occupancy <- KCK_nb$occ_nb/KCK_nb$all_nb

av_occupancy[which(av_occupancy$site == "KCK"),"occupancy"] <- mean(na.omit(KCK_nb$occupancy))

# KCZ site

KCZ_nb <- data.frame(year = c(2014:2024), site = rep("KCZ", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(KCZ_nb)) {
  if (is.na(sites["KCZ", "extra"])) {
    KCZ_nb[i,"all_nb"] <- sites["KCZ","Current.Boxes"]
  } else if (KCZ_nb[i, "year"] >= sites["KCZ", "extra"]) {
    KCZ_nb[i,"all_nb"] <- sites["KCZ","Current.Boxes"]
  } else if (KCZ_nb[i, "year"] < sites["KCZ", "extra"]) {
    KCZ_nb[i,"all_nb"] <- sites["KCZ","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["KCZ",i])) {
    KCZ_nb[row, "all_nb"] <- NA
  }
}

blutiKCZ <- blutidf[which(blutidf$site == "KCZ"),] %>% count(year)

for (i in 1:nrow(KCZ_nb)) {
  if (KCZ_nb[i,"year"] %in% blutiKCZ$year) {
    KCZ_nb[i,"occ_nb"] <- blutiKCZ[which(blutiKCZ$year == KCZ_nb[i,"year"]),"n"]
  } else {
    KCZ_nb[i,"occ_nb"] <- 0
  }
}

KCZ_nb$occupancy <- KCZ_nb$occ_nb/KCZ_nb$all_nb

av_occupancy[which(av_occupancy$site == "KCZ"),"occupancy"] <- mean(na.omit(KCZ_nb$occupancy))

# BLA site

BLA_nb <- data.frame(year = c(2014:2024), site = rep("BLA", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(BLA_nb)) {
  if (is.na(sites["BLA", "extra"])) {
    BLA_nb[i,"all_nb"] <- sites["BLA","Current.Boxes"]
  } else if (BLA_nb[i, "year"] >= sites["BLA", "extra"]) {
    BLA_nb[i,"all_nb"] <- sites["BLA","Current.Boxes"]
  } else if (BLA_nb[i, "year"] < sites["BLA", "extra"]) {
    BLA_nb[i,"all_nb"] <- sites["BLA","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["BLA",i])) {
    BLA_nb[row, "all_nb"] <- NA
  }
}

blutiBLA <- blutidf[which(blutidf$site == "BLA"),] %>% count(year)

for (i in 1:nrow(BLA_nb)) {
  if (BLA_nb[i,"year"] %in% blutiBLA$year) {
    BLA_nb[i,"occ_nb"] <- blutiBLA[which(blutiBLA$year == BLA_nb[i,"year"]),"n"]
  } else {
    BLA_nb[i,"occ_nb"] <- 0
  }
}

BLA_nb$occupancy <- BLA_nb$occ_nb/BLA_nb$all_nb

av_occupancy[which(av_occupancy$site == "BLA"),"occupancy"] <- mean(na.omit(BLA_nb$occupancy))

# CAL site

CAL_nb <- data.frame(year = c(2014:2024), site = rep("CAL", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(CAL_nb)) {
  if (is.na(sites["CAL", "extra"])) {
    CAL_nb[i,"all_nb"] <- sites["CAL","Current.Boxes"]
  } else if (CAL_nb[i, "year"] >= sites["CAL", "extra"]) {
    CAL_nb[i,"all_nb"] <- sites["CAL","Current.Boxes"]
  } else if (CAL_nb[i, "year"] < sites["CAL", "extra"]) {
    CAL_nb[i,"all_nb"] <- sites["CAL","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["CAL",i])) {
    CAL_nb[row, "all_nb"] <- NA
  }
}

blutiCAL <- blutidf[which(blutidf$site == "CAL"),] %>% count(year)

for (i in 1:nrow(CAL_nb)) {
  if (CAL_nb[i,"year"] %in% blutiCAL$year) {
    CAL_nb[i,"occ_nb"] <- blutiCAL[which(blutiCAL$year == CAL_nb[i,"year"]),"n"]
  } else {
    CAL_nb[i,"occ_nb"] <- 0
  }
}

CAL_nb$occupancy <- CAL_nb$occ_nb/CAL_nb$all_nb

av_occupancy[which(av_occupancy$site == "CAL"),"occupancy"] <- mean(na.omit(CAL_nb$occupancy))


# DNM site

DNM_nb <- data.frame(year = c(2014:2024), site = rep("DNM", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(DNM_nb)) {
  if (is.na(sites["DNM", "extra"])) {
    DNM_nb[i,"all_nb"] <- sites["DNM","Current.Boxes"]
  } else if (DNM_nb[i, "year"] >= sites["DNM", "extra"]) {
    DNM_nb[i,"all_nb"] <- sites["DNM","Current.Boxes"]
  } else if (DNM_nb[i, "year"] < sites["DNM", "extra"]) {
    DNM_nb[i,"all_nb"] <- sites["DNM","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["DNM",i])) {
    DNM_nb[row, "all_nb"] <- NA
  }
}

blutiDNM <- blutidf[which(blutidf$site == "DNM"),] %>% count(year)

for (i in 1:nrow(DNM_nb)) {
  if (DNM_nb[i,"year"] %in% blutiDNM$year) {
    DNM_nb[i,"occ_nb"] <- blutiDNM[which(blutiDNM$year == DNM_nb[i,"year"]),"n"]
  } else {
    DNM_nb[i,"occ_nb"] <- 0
  }
}

DNM_nb$occupancy <- DNM_nb$occ_nb/DNM_nb$all_nb

av_occupancy[which(av_occupancy$site == "DNM"),"occupancy"] <- mean(na.omit(DNM_nb$occupancy))

# DNC site

DNC_nb <- data.frame(year = c(2014:2024), site = rep("DNC", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(DNC_nb)) {
  if (is.na(sites["DNC", "extra"])) {
    DNC_nb[i,"all_nb"] <- sites["DNC","Current.Boxes"]
  } else if (DNC_nb[i, "year"] >= sites["DNC", "extra"]) {
    DNC_nb[i,"all_nb"] <- sites["DNC","Current.Boxes"]
  } else if (DNC_nb[i, "year"] < sites["DNC", "extra"]) {
    DNC_nb[i,"all_nb"] <- sites["DNC","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["DNC",i])) {
    DNC_nb[row, "all_nb"] <- NA
  }
}

blutiDNC <- blutidf[which(blutidf$site == "DNC"),] %>% count(year)

for (i in 1:nrow(DNC_nb)) {
  if (DNC_nb[i,"year"] %in% blutiDNC$year) {
    DNC_nb[i,"occ_nb"] <- blutiDNC[which(blutiDNC$year == DNC_nb[i,"year"]),"n"]
  } else {
    DNC_nb[i,"occ_nb"] <- 0
  }
}

DNC_nb$occupancy <- DNC_nb$occ_nb/DNC_nb$all_nb

av_occupancy[which(av_occupancy$site == "DNC"),"occupancy"] <- mean(na.omit(DNC_nb$occupancy))

# DNS site

DNS_nb <- data.frame(year = c(2014:2024), site = rep("DNS", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(DNS_nb)) {
  if (is.na(sites["DNS", "extra"])) {
    DNS_nb[i,"all_nb"] <- sites["DNS","Current.Boxes"]
  } else if (DNS_nb[i, "year"] >= sites["DNS", "extra"]) {
    DNS_nb[i,"all_nb"] <- sites["DNS","Current.Boxes"]
  } else if (DNS_nb[i, "year"] < sites["DNS", "extra"]) {
    DNS_nb[i,"all_nb"] <- sites["DNS","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["DNS",i])) {
    DNS_nb[row, "all_nb"] <- NA
  }
}

blutiDNS <- blutidf[which(blutidf$site == "DNS"),] %>% count(year)

for (i in 1:nrow(DNS_nb)) {
  if (DNS_nb[i,"year"] %in% blutiDNS$year) {
    DNS_nb[i,"occ_nb"] <- blutiDNS[which(blutiDNS$year == DNS_nb[i,"year"]),"n"]
  } else {
    DNS_nb[i,"occ_nb"] <- 0
  }
}

DNS_nb$occupancy <- DNS_nb$occ_nb/DNS_nb$all_nb

av_occupancy[which(av_occupancy$site == "DNS"),"occupancy"] <- mean(na.omit(DNS_nb$occupancy))

# DLW site

DLW_nb <- data.frame(year = c(2014:2024), site = rep("DLW", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(DLW_nb)) {
  if (is.na(sites["DLW", "extra"])) {
    DLW_nb[i,"all_nb"] <- sites["DLW","Current.Boxes"]
  } else if (DLW_nb[i, "year"] >= sites["DLW", "extra"]) {
    DLW_nb[i,"all_nb"] <- sites["DLW","Current.Boxes"]
  } else if (DLW_nb[i, "year"] < sites["DLW", "extra"]) {
    DLW_nb[i,"all_nb"] <- sites["DLW","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["DLW",i])) {
    DLW_nb[row, "all_nb"] <- NA
  }
}

blutiDLW <- blutidf[which(blutidf$site == "DLW"),] %>% count(year)

for (i in 1:nrow(DLW_nb)) {
  if (DLW_nb[i,"year"] %in% blutiDLW$year) {
    DLW_nb[i,"occ_nb"] <- blutiDLW[which(blutiDLW$year == DLW_nb[i,"year"]),"n"]
  } else {
    DLW_nb[i,"occ_nb"] <- 0
  }
}

DLW_nb$occupancy <- DLW_nb$occ_nb/DLW_nb$all_nb

av_occupancy[which(av_occupancy$site == "DLW"),"occupancy"] <- mean(na.omit(DLW_nb$occupancy))

# CRU site

CRU_nb <- data.frame(year = c(2014:2024), site = rep("CRU", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(CRU_nb)) {
  if (is.na(sites["CRU", "extra"])) {
    CRU_nb[i,"all_nb"] <- sites["CRU","Current.Boxes"]
  } else if (CRU_nb[i, "year"] >= sites["CRU", "extra"]) {
    CRU_nb[i,"all_nb"] <- sites["CRU","Current.Boxes"]
  } else if (CRU_nb[i, "year"] < sites["CRU", "extra"]) {
    CRU_nb[i,"all_nb"] <- sites["CRU","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["CRU",i])) {
    CRU_nb[row, "all_nb"] <- NA
  }
}

blutiCRU <- blutidf[which(blutidf$site == "CRU"),] %>% count(year)

for (i in 1:nrow(CRU_nb)) {
  if (CRU_nb[i,"year"] %in% blutiCRU$year) {
    CRU_nb[i,"occ_nb"] <- blutiCRU[which(blutiCRU$year == CRU_nb[i,"year"]),"n"]
  } else {
    CRU_nb[i,"occ_nb"] <- 0
  }
}

CRU_nb$occupancy <- CRU_nb$occ_nb/CRU_nb$all_nb

av_occupancy[which(av_occupancy$site == "CRU"),"occupancy"] <- mean(na.omit(CRU_nb$occupancy))

# NEW site

NEW_nb <- data.frame(year = c(2014:2024), site = rep("NEW", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 1))

for (i in 1:nrow(NEW_nb)) {
  if (is.na(sites["NEW", "extra"])) {
    NEW_nb[i,"all_nb"] <- sites["NEW","Current.Boxes"]
  } else if (NEW_nb[i, "year"] >= sites["NEW", "extra"]) {
    NEW_nb[i,"all_nb"] <- sites["NEW","Current.Boxes"]
  } else if (NEW_nb[i, "year"] < sites["NEW", "extra"]) {
    NEW_nb[i,"all_nb"] <- sites["NEW","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["NEW",i])) {
    NEW_nb[row, "all_nb"] <- NA
  }
}

blutiNEW <- blutidf[which(blutidf$site == "NEW"),] %>% count(year)

for (i in 1:nrow(NEW_nb)) {
  if (NEW_nb[i,"year"] %in% blutiNEW$year) {
    NEW_nb[i,"occ_nb"] <- blutiNEW[which(blutiNEW$year == NEW_nb[i,"year"]),"n"]
  } else {
    NEW_nb[i,"occ_nb"] <- 0
  }
}

NEW_nb$occupancy <- NEW_nb$occ_nb/NEW_nb$all_nb

av_occupancy[which(av_occupancy$site == "NEW"),"occupancy"] <- mean(na.omit(NEW_nb$occupancy))

# HWP site

HWP_nb <- data.frame(year = c(2014:2024), site = rep("HWP", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(HWP_nb)) {
  if (is.na(sites["HWP", "extra"])) {
    HWP_nb[i,"all_nb"] <- sites["HWP","Current.Boxes"]
  } else if (HWP_nb[i, "year"] >= sites["HWP", "extra"]) {
    HWP_nb[i,"all_nb"] <- sites["HWP","Current.Boxes"]
  } else if (HWP_nb[i, "year"] < sites["HWP", "extra"]) {
    HWP_nb[i,"all_nb"] <- sites["HWP","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["HWP",i])) {
    HWP_nb[row, "all_nb"] <- NA
  }
}

blutiHWP <- blutidf[which(blutidf$site == "HWP"),] %>% count(year)

for (i in 1:nrow(HWP_nb)) {
  if (HWP_nb[i,"year"] %in% blutiHWP$year) {
    HWP_nb[i,"occ_nb"] <- blutiHWP[which(blutiHWP$year == HWP_nb[i,"year"]),"n"]
  } else {
    HWP_nb[i,"occ_nb"] <- 0
  }
}

HWP_nb$occupancy <- HWP_nb$occ_nb/HWP_nb$all_nb

av_occupancy[which(av_occupancy$site == "HWP"),"occupancy"] <- mean(na.omit(HWP_nb$occupancy))

# INS site

INS_nb <- data.frame(year = c(2014:2024), site = rep("INS", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(INS_nb)) {
  if (is.na(sites["INS", "extra"])) {
    INS_nb[i,"all_nb"] <- sites["INS","Current.Boxes"]
  } else if (INS_nb[i, "year"] >= sites["INS", "extra"]) {
    INS_nb[i,"all_nb"] <- sites["INS","Current.Boxes"]
  } else if (INS_nb[i, "year"] < sites["INS", "extra"]) {
    INS_nb[i,"all_nb"] <- sites["INS","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["INS",i])) {
    INS_nb[row, "all_nb"] <- NA
  }
}

blutiINS <- blutidf[which(blutidf$site == "INS"),] %>% count(year)

for (i in 1:nrow(INS_nb)) {
  if (INS_nb[i,"year"] %in% blutiINS$year) {
    INS_nb[i,"occ_nb"] <- blutiINS[which(blutiINS$year == INS_nb[i,"year"]),"n"]
  } else {
    INS_nb[i,"occ_nb"] <- 0
  }
}

INS_nb$occupancy <- INS_nb$occ_nb/INS_nb$all_nb

av_occupancy[which(av_occupancy$site == "INS"),"occupancy"] <- mean(na.omit(INS_nb$occupancy))

# FSH

FSH_nb <- data.frame(year = c(2014:2024), site = rep("FSH", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(FSH_nb)) {
  if (is.na(sites["FSH", "extra"])) {
    FSH_nb[i,"all_nb"] <- sites["FSH","Current.Boxes"]
  } else if (FSH_nb[i, "year"] >= sites["FSH", "extra"]) {
    FSH_nb[i,"all_nb"] <- sites["FSH","Current.Boxes"]
  } else if (FSH_nb[i, "year"] < sites["FSH", "extra"]) {
    FSH_nb[i,"all_nb"] <- sites["FSH","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["FSH",i])) {
    FSH_nb[row, "all_nb"] <- NA
  }
}

blutiFSH <- blutidf[which(blutidf$site == "FSH"),] %>% count(year)

for (i in 1:nrow(FSH_nb)) {
  if (FSH_nb[i,"year"] %in% blutiFSH$year) {
    FSH_nb[i,"occ_nb"] <- blutiFSH[which(blutiFSH$year == FSH_nb[i,"year"]),"n"]
  } else {
    FSH_nb[i,"occ_nb"] <- 0
  }
}

FSH_nb$occupancy <- FSH_nb$occ_nb/FSH_nb$all_nb

av_occupancy[which(av_occupancy$site == "FSH"),"occupancy"] <- mean(na.omit(FSH_nb$occupancy))

# RTH

RTH_nb <- data.frame(year = c(2014:2024), site = rep("RTH", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(RTH_nb)) {
  if (is.na(sites["RTH", "extra"])) {
    RTH_nb[i,"all_nb"] <- sites["RTH","Current.Boxes"]
  } else if (RTH_nb[i, "year"] >= sites["RTH", "extra"]) {
    RTH_nb[i,"all_nb"] <- sites["RTH","Current.Boxes"]
  } else if (RTH_nb[i, "year"] < sites["RTH", "extra"]) {
    RTH_nb[i,"all_nb"] <- sites["RTH","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["RTH",i])) {
    RTH_nb[row, "all_nb"] <- NA
  }
}

blutiRTH <- blutidf[which(blutidf$site == "RTH"),] %>% count(year)

for (i in 1:nrow(RTH_nb)) {
  if (RTH_nb[i,"year"] %in% blutiRTH$year) {
    RTH_nb[i,"occ_nb"] <- blutiRTH[which(blutiRTH$year == RTH_nb[i,"year"]),"n"]
  } else {
    RTH_nb[i,"occ_nb"] <- 0
  }
}

RTH_nb$occupancy <- RTH_nb$occ_nb/RTH_nb$all_nb

av_occupancy[which(av_occupancy$site == "RTH"),"occupancy"] <- mean(na.omit(RTH_nb$occupancy))

# AVI

AVI_nb <- data.frame(year = c(2014:2024), site = rep("AVI", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(AVI_nb)) {
  if (is.na(sites["AVI", "extra"])) {
    AVI_nb[i,"all_nb"] <- sites["AVI","Current.Boxes"]
  } else if (AVI_nb[i, "year"] >= sites["AVI", "extra"]) {
    AVI_nb[i,"all_nb"] <- sites["AVI","Current.Boxes"]
  } else if (AVI_nb[i, "year"] < sites["AVI", "extra"]) {
    AVI_nb[i,"all_nb"] <- sites["AVI","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["AVI",i])) {
    AVI_nb[row, "all_nb"] <- NA
  }
}

blutiAVI <- blutidf[which(blutidf$site == "AVI"),] %>% count(year)

for (i in 1:nrow(AVI_nb)) {
  if (AVI_nb[i,"year"] %in% blutiAVI$year) {
    AVI_nb[i,"occ_nb"] <- blutiAVI[which(blutiAVI$year == AVI_nb[i,"year"]),"n"]
  } else {
    AVI_nb[i,"occ_nb"] <- 0
  }
}

AVI_nb$occupancy <- AVI_nb$occ_nb/AVI_nb$all_nb

av_occupancy[which(av_occupancy$site == "AVI"),"occupancy"] <- mean(na.omit(AVI_nb$occupancy))

# AVN

AVN_nb <- data.frame(year = c(2014:2024), site = rep("AVN", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(AVN_nb)) {
  if (is.na(sites["AVN", "extra"])) {
    AVN_nb[i,"all_nb"] <- sites["AVN","Current.Boxes"]
  } else if (AVN_nb[i, "year"] >= sites["AVN", "extra"]) {
    AVN_nb[i,"all_nb"] <- sites["AVN","Current.Boxes"]
  } else if (AVN_nb[i, "year"] < sites["AVN", "extra"]) {
    AVN_nb[i,"all_nb"] <- sites["AVN","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["AVN",i])) {
    AVN_nb[row, "all_nb"] <- NA
  }
}

blutiAVN <- blutidf[which(blutidf$site == "AVN"),] %>% count(year)

for (i in 1:nrow(AVN_nb)) {
  if (AVN_nb[i,"year"] %in% blutiAVN$year) {
    AVN_nb[i,"occ_nb"] <- blutiAVN[which(blutiAVN$year == AVN_nb[i,"year"]),"n"]
  } else {
    AVN_nb[i,"occ_nb"] <- 0
  }
}

AVN_nb$occupancy <- AVN_nb$occ_nb/AVN_nb$all_nb

av_occupancy[which(av_occupancy$site == "AVN"),"occupancy"] <- mean(na.omit(AVN_nb$occupancy))

# CAR

CAR_nb <- data.frame(year = c(2014:2024), site = rep("CAR", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(CAR_nb)) {
  if (is.na(sites["CAR", "extra"])) {
    CAR_nb[i,"all_nb"] <- sites["CAR","Current.Boxes"]
  } else if (CAR_nb[i, "year"] >= sites["CAR", "extra"]) {
    CAR_nb[i,"all_nb"] <- sites["CAR","Current.Boxes"]
  } else if (CAR_nb[i, "year"] < sites["CAR", "extra"]) {
    CAR_nb[i,"all_nb"] <- sites["CAR","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["CAR",i])) {
    CAR_nb[row, "all_nb"] <- NA
  }
}

blutiCAR <- blutidf[which(blutidf$site == "CAR"),] %>% count(year)

for (i in 1:nrow(CAR_nb)) {
  if (CAR_nb[i,"year"] %in% blutiCAR$year) {
    CAR_nb[i,"occ_nb"] <- blutiCAR[which(blutiCAR$year == CAR_nb[i,"year"]),"n"]
  } else {
    CAR_nb[i,"occ_nb"] <- 0
  }
}

CAR_nb$occupancy <- CAR_nb$occ_nb/CAR_nb$all_nb

av_occupancy[which(av_occupancy$site == "CAR"),"occupancy"] <- mean(na.omit(CAR_nb$occupancy))

# SLS

SLS_nb <- data.frame(year = c(2014:2024), site = rep("SLS", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(SLS_nb)) {
  if (is.na(sites["SLS", "extra"])) {
    SLS_nb[i,"all_nb"] <- sites["SLS","Current.Boxes"]
  } else if (SLS_nb[i, "year"] >= sites["SLS", "extra"]) {
    SLS_nb[i,"all_nb"] <- sites["SLS","Current.Boxes"]
  } else if (SLS_nb[i, "year"] < sites["SLS", "extra"]) {
    SLS_nb[i,"all_nb"] <- sites["SLS","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["SLS",i])) {
    SLS_nb[row, "all_nb"] <- NA
  }
}

blutiSLS <- blutidf[which(blutidf$site == "SLS"),] %>% count(year)

for (i in 1:nrow(SLS_nb)) {
  if (SLS_nb[i,"year"] %in% blutiSLS$year) {
    SLS_nb[i,"occ_nb"] <- blutiSLS[which(blutiSLS$year == SLS_nb[i,"year"]),"n"]
  } else {
    SLS_nb[i,"occ_nb"] <- 0
  }
}

SLS_nb$occupancy <- SLS_nb$occ_nb/SLS_nb$all_nb

av_occupancy[which(av_occupancy$site == "SLS"),"occupancy"] <- mean(na.omit(SLS_nb$occupancy))

# TOM

TOM_nb <- data.frame(year = c(2014:2024), site = rep("TOM", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(TOM_nb)) {
  if (is.na(sites["TOM", "extra"])) {
    TOM_nb[i,"all_nb"] <- sites["TOM","Current.Boxes"]
  } else if (TOM_nb[i, "year"] >= sites["TOM", "extra"]) {
    TOM_nb[i,"all_nb"] <- sites["TOM","Current.Boxes"]
  } else if (TOM_nb[i, "year"] < sites["TOM", "extra"]) {
    TOM_nb[i,"all_nb"] <- sites["TOM","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["TOM",i])) {
    TOM_nb[row, "all_nb"] <- NA
  }
}

blutiTOM <- blutidf[which(blutidf$site == "TOM"),] %>% count(year)

for (i in 1:nrow(TOM_nb)) {
  if (TOM_nb[i,"year"] %in% blutiTOM$year) {
    TOM_nb[i,"occ_nb"] <- blutiTOM[which(blutiTOM$year == TOM_nb[i,"year"]),"n"]
  } else {
    TOM_nb[i,"occ_nb"] <- 0
  }
}

TOM_nb$occupancy <- TOM_nb$occ_nb/TOM_nb$all_nb

av_occupancy[which(av_occupancy$site == "TOM"),"occupancy"] <- mean(na.omit(TOM_nb$occupancy))

# DAV

DAV_nb <- data.frame(year = c(2014:2024), site = rep("DAV", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(DAV_nb)) {
  if (is.na(sites["DAV", "extra"])) {
    DAV_nb[i,"all_nb"] <- sites["DAV","Current.Boxes"]
  } else if (DAV_nb[i, "year"] >= sites["DAV", "extra"]) {
    DAV_nb[i,"all_nb"] <- sites["DAV","Current.Boxes"]
  } else if (DAV_nb[i, "year"] < sites["DAV", "extra"]) {
    DAV_nb[i,"all_nb"] <- sites["DAV","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["DAV",i])) {
    DAV_nb[row, "all_nb"] <- NA
  }
}

blutiDAV <- blutidf[which(blutidf$site == "DAV"),] %>% count(year)

for (i in 1:nrow(DAV_nb)) {
  if (DAV_nb[i,"year"] %in% blutiDAV$year) {
    DAV_nb[i,"occ_nb"] <- blutiDAV[which(blutiDAV$year == DAV_nb[i,"year"]),"n"]
  } else {
    DAV_nb[i,"occ_nb"] <- 0
  }
}

DAV_nb$occupancy <- DAV_nb$occ_nb/DAV_nb$all_nb

av_occupancy[which(av_occupancy$site == "DAV"),"occupancy"] <- mean(na.omit(DAV_nb$occupancy))

# ART

ART_nb <- data.frame(year = c(2014:2024), site = rep("ART", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(ART_nb)) {
  if (is.na(sites["ART", "extra"])) {
    ART_nb[i,"all_nb"] <- sites["ART","Current.Boxes"]
  } else if (ART_nb[i, "year"] >= sites["ART", "extra"]) {
    ART_nb[i,"all_nb"] <- sites["ART","Current.Boxes"]
  } else if (ART_nb[i, "year"] < sites["ART", "extra"]) {
    ART_nb[i,"all_nb"] <- sites["ART","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["ART",i])) {
    ART_nb[row, "all_nb"] <- NA
  }
}

blutiART <- blutidf[which(blutidf$site == "ART"),] %>% count(year)

for (i in 1:nrow(ART_nb)) {
  if (ART_nb[i,"year"] %in% blutiART$year) {
    ART_nb[i,"occ_nb"] <- blutiART[which(blutiART$year == ART_nb[i,"year"]),"n"]
  } else {
    ART_nb[i,"occ_nb"] <- 0
  }
}

ART_nb$occupancy <- ART_nb$occ_nb/ART_nb$all_nb

av_occupancy[which(av_occupancy$site == "ART"),"occupancy"] <- mean(na.omit(ART_nb$occupancy))

# MUN

MUN_nb <- data.frame(year = c(2014:2024), site = rep("MUN", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(MUN_nb)) {
  if (is.na(sites["MUN", "extra"])) {
    MUN_nb[i,"all_nb"] <- sites["MUN","Current.Boxes"]
  } else if (MUN_nb[i, "year"] >= sites["MUN", "extra"]) {
    MUN_nb[i,"all_nb"] <- sites["MUN","Current.Boxes"]
  } else if (MUN_nb[i, "year"] < sites["MUN", "extra"]) {
    MUN_nb[i,"all_nb"] <- sites["MUN","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["MUN",i])) {
    MUN_nb[row, "all_nb"] <- NA
  }
}

blutiMUN <- blutidf[which(blutidf$site == "MUN"),] %>% count(year)

for (i in 1:nrow(MUN_nb)) {
  if (MUN_nb[i,"year"] %in% blutiMUN$year) {
    MUN_nb[i,"occ_nb"] <- blutiMUN[which(blutiMUN$year == MUN_nb[i,"year"]),"n"]
  } else {
    MUN_nb[i,"occ_nb"] <- 0
  }
}

MUN_nb$occupancy <- MUN_nb$occ_nb/MUN_nb$all_nb

av_occupancy[which(av_occupancy$site == "MUN"),"occupancy"] <- mean(na.omit(MUN_nb$occupancy))

# FOU

FOU_nb <- data.frame(year = c(2014:2024), site = rep("FOU", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(FOU_nb)) {
  if (is.na(sites["FOU", "extra"])) {
    FOU_nb[i,"all_nb"] <- sites["FOU","Current.Boxes"]
  } else if (FOU_nb[i, "year"] >= sites["FOU", "extra"]) {
    FOU_nb[i,"all_nb"] <- sites["FOU","Current.Boxes"]
  } else if (FOU_nb[i, "year"] < sites["FOU", "extra"]) {
    FOU_nb[i,"all_nb"] <- sites["FOU","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["FOU",i])) {
    FOU_nb[row, "all_nb"] <- NA
  }
}

blutiFOU <- blutidf[which(blutidf$site == "FOU"),] %>% count(year)

for (i in 1:nrow(FOU_nb)) {
  if (FOU_nb[i,"year"] %in% blutiFOU$year) {
    FOU_nb[i,"occ_nb"] <- blutiFOU[which(blutiFOU$year == FOU_nb[i,"year"]),"n"]
  } else {
    FOU_nb[i,"occ_nb"] <- 0
  }
}

FOU_nb$occupancy <- FOU_nb$occ_nb/FOU_nb$all_nb

av_occupancy[which(av_occupancy$site == "FOU"),"occupancy"] <- mean(na.omit(FOU_nb$occupancy))

# ALN

ALN_nb <- data.frame(year = c(2014:2024), site = rep("ALN", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(ALN_nb)) {
  if (is.na(sites["ALN", "extra"])) {
    ALN_nb[i,"all_nb"] <- sites["ALN","Current.Boxes"]
  } else if (ALN_nb[i, "year"] >= sites["ALN", "extra"]) {
    ALN_nb[i,"all_nb"] <- sites["ALN","Current.Boxes"]
  } else if (ALN_nb[i, "year"] < sites["ALN", "extra"]) {
    ALN_nb[i,"all_nb"] <- sites["ALN","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["ALN",i])) {
    ALN_nb[row, "all_nb"] <- NA
  }
}

blutiALN <- blutidf[which(blutidf$site == "ALN"),] %>% count(year)

for (i in 1:nrow(ALN_nb)) {
  if (ALN_nb[i,"year"] %in% blutiALN$year) {
    ALN_nb[i,"occ_nb"] <- blutiALN[which(blutiALN$year == ALN_nb[i,"year"]),"n"]
  } else {
    ALN_nb[i,"occ_nb"] <- 0
  }
}

ALN_nb$occupancy <- ALN_nb$occ_nb/ALN_nb$all_nb

av_occupancy[which(av_occupancy$site == "ALN"),"occupancy"] <- mean(na.omit(ALN_nb$occupancy))

# DEL

DEL_nb <- data.frame(year = c(2014:2024), site = rep("DEL", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(DEL_nb)) {
  if (is.na(sites["DEL", "extra"])) {
    DEL_nb[i,"all_nb"] <- sites["DEL","Current.Boxes"]
  } else if (DEL_nb[i, "year"] >= sites["DEL", "extra"]) {
    DEL_nb[i,"all_nb"] <- sites["DEL","Current.Boxes"]
  } else if (DEL_nb[i, "year"] < sites["DEL", "extra"]) {
    DEL_nb[i,"all_nb"] <- sites["DEL","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["DEL",i])) {
    DEL_nb[row, "all_nb"] <- NA
  }
}

blutiDEL <- blutidf[which(blutidf$site == "DEL"),] %>% count(year)

for (i in 1:nrow(DEL_nb)) {
  if (DEL_nb[i,"year"] %in% blutiDEL$year) {
    DEL_nb[i,"occ_nb"] <- blutiDEL[which(blutiDEL$year == DEL_nb[i,"year"]),"n"]
  } else {
    DEL_nb[i,"occ_nb"] <- 0
  }
}

DEL_nb$occupancy <- DEL_nb$occ_nb/DEL_nb$all_nb

av_occupancy[which(av_occupancy$site == "DEL"),"occupancy"] <- mean(na.omit(DEL_nb$occupancy))

# TAI

TAI_nb <- data.frame(year = c(2014:2024), site = rep("TAI", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(TAI_nb)) {
  if (is.na(sites["TAI", "extra"])) {
    TAI_nb[i,"all_nb"] <- sites["TAI","Current.Boxes"]
  } else if (TAI_nb[i, "year"] >= sites["TAI", "extra"]) {
    TAI_nb[i,"all_nb"] <- sites["TAI","Current.Boxes"]
  } else if (TAI_nb[i, "year"] < sites["TAI", "extra"]) {
    TAI_nb[i,"all_nb"] <- sites["TAI","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["TAI",i])) {
    TAI_nb[row, "all_nb"] <- NA
  }
}

blutiTAI <- blutidf[which(blutidf$site == "TAI"),] %>% count(year)

for (i in 1:nrow(TAI_nb)) {
  if (TAI_nb[i,"year"] %in% blutiTAI$year) {
    TAI_nb[i,"occ_nb"] <- blutiTAI[which(blutiTAI$year == TAI_nb[i,"year"]),"n"]
  } else {
    TAI_nb[i,"occ_nb"] <- 0
  }
}

TAI_nb$occupancy <- TAI_nb$occ_nb/TAI_nb$all_nb

av_occupancy[which(av_occupancy$site == "TAI"),"occupancy"] <- mean(na.omit(TAI_nb$occupancy))

# SPD

SPD_nb <- data.frame(year = c(2014:2024), site = rep("SPD", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(SPD_nb)) {
  if (is.na(sites["SPD", "extra"])) {
    SPD_nb[i,"all_nb"] <- sites["SPD","Current.Boxes"]
  } else if (SPD_nb[i, "year"] >= sites["SPD", "extra"]) {
    SPD_nb[i,"all_nb"] <- sites["SPD","Current.Boxes"]
  } else if (SPD_nb[i, "year"] < sites["SPD", "extra"]) {
    SPD_nb[i,"all_nb"] <- sites["SPD","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["SPD",i])) {
    SPD_nb[row, "all_nb"] <- NA
  }
}

blutiSPD <- blutidf[which(blutidf$site == "SPD"),] %>% count(year)

for (i in 1:nrow(SPD_nb)) {
  if (SPD_nb[i,"year"] %in% blutiSPD$year) {
    SPD_nb[i,"occ_nb"] <- blutiSPD[which(blutiSPD$year == SPD_nb[i,"year"]),"n"]
  } else {
    SPD_nb[i,"occ_nb"] <- 0
  }
}

SPD_nb$occupancy <- SPD_nb$occ_nb/SPD_nb$all_nb

av_occupancy[which(av_occupancy$site == "SPD"),"occupancy"] <- mean(na.omit(SPD_nb$occupancy))

# OSP

OSP_nb <- data.frame(year = c(2014:2024), site = rep("OSP", 11), all_nb = seq(1,11,1), occ_nb = rep(NA, 11))

for (i in 1:nrow(OSP_nb)) {
  if (is.na(sites["OSP", "extra"])) {
    OSP_nb[i,"all_nb"] <- sites["OSP","Current.Boxes"]
  } else if (OSP_nb[i, "year"] >= sites["OSP", "extra"]) {
    OSP_nb[i,"all_nb"] <- sites["OSP","Current.Boxes"]
  } else if (OSP_nb[i, "year"] < sites["OSP", "extra"]) {
    OSP_nb[i,"all_nb"] <- sites["OSP","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["OSP",i])) {
    OSP_nb[row, "all_nb"] <- NA
  }
}

blutiOSP <- blutidf[which(blutidf$site == "OSP"),] %>% count(year)

for (i in 1:nrow(OSP_nb)) {
  if (OSP_nb[i,"year"] %in% blutiOSP$year) {
    OSP_nb[i,"occ_nb"] <- blutiOSP[which(blutiOSP$year == OSP_nb[i,"year"]),"n"]
  } else {
    OSP_nb[i,"occ_nb"] <- 0
  }
}

OSP_nb$occupancy <- OSP_nb$occ_nb/OSP_nb$all_nb

av_occupancy[which(av_occupancy$site == "OSP"),"occupancy"] <- mean(na.omit(OSP_nb$occupancy))

# DOR

DOR_nb <- data.frame(year = c(2014:2021), site = rep("DOR", 8), all_nb = seq(1,8,1), occ_nb = rep(NA, 8))

for (i in 1:nrow(DOR_nb)) {
  if (is.na(sites["DOR", "extra"])) {
    DOR_nb[i,"all_nb"] <- sites["DOR","Current.Boxes"]
  } else if (DOR_nb[i, "year"] >= sites["DOR", "extra"]) {
    DOR_nb[i,"all_nb"] <- sites["DOR","Current.Boxes"]
  } else if (DOR_nb[i, "year"] < sites["DOR", "extra"]) {
    DOR_nb[i,"all_nb"] <- sites["DOR","Original.Boxes"]
  }
}

row <- 0
for (i in 10:17) {
  row <- row + 1
  if (is.na(sites["DOR",i])) {
    DOR_nb[row, "all_nb"] <- NA
  }
}

blutiDOR <- blutidf[which(blutidf$site == "DOR"),] %>% count(year)

for (i in 1:nrow(DOR_nb)) {
  if (DOR_nb[i,"year"] %in% blutiDOR$year) {
    DOR_nb[i,"occ_nb"] <- blutiDOR[which(blutiDOR$year == DOR_nb[i,"year"]),"n"]
  } else {
    DOR_nb[i,"occ_nb"] <- 0
  }
}

DOR_nb$occupancy <- DOR_nb$occ_nb/DOR_nb$all_nb

av_occupancy[which(av_occupancy$site == "DOR"),"occupancy"] <- mean(na.omit(DOR_nb$occupancy))


#write.csv(av_occupancy,"data/site_occupancy.csv")

site_occupancy <- read.csv("data/site_occupancy.csv")
blutidf <- read.csv("data/blutidf.csv")
blutidf_3yo <- read.csv("data/blutidf_3yo.csv")
blutidf$site_occ <- site_occupancy$occupancy[match(blutidf$site, site_elevation$site)] 
blutidf_3yo$site_occ <- site_occupancy$occupancy[match(blutidf_3yo$site, site_elevation$site)] 



### Calculating average nest box occupancy per site using solely Bird_Phenology.csv ###


