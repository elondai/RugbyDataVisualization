########## Data Preparation ##############

# Profile
df.playerprofiles = read.table("./datasets/PlayerProfiles.csv", skip = 2, sep = ",",
                        quot="", header = T, stringsAsFactors = F)

colnames(df.playerprofiles) = gsub("\\.", "", colnames(df.playerprofiles))
df.playerprofiles[df.playerprofiles==0] = NA

df.profile = aggregate(df.playerprofiles[, 5:31],
          by = list(df.playerprofiles$Player, df.playerprofiles$PositionGroup, df.playerprofiles$Position),
          FUN = mean)

colnames(df.profile)[1:3] = c("Player", "PositionGroup", "Position")
# Defence
df.defence = read.table("./datasets/Defence.csv", skip = 2, sep = ",",
                        quot="", header = T, stringsAsFactors = F)  

colnames(df.defence) = gsub("\\.", "", colnames(df.defence))
df.defence$TackleCompletition = as.numeric(gsub("%", "", 
                                                df.defence$TackleCompletition)) 
df.def.overall = df.defence[which(is.na(df.defence$MatchID1)), ]
df.def.detail = df.defence[-which(is.na(df.defence$MatchID1)), ]
df.def.combine = merge(df.def.detail, df.profile, by = "Player")
df.def.cor = merge(df.def.overall, df.profile, by = "Player")

# Carries
df.carries = read.table("./datasets/Carries.csv", skip = 2, sep = ",",
                        quot="", header = T, stringsAsFactors = F)

colnames(df.carries) = gsub("\\.", "", colnames(df.carries))
df.carries$Linebreak = as.numeric(gsub("%", "", df.carries$Linebreak)) 
df.carries$DefendersBeatenPCT = as.numeric(gsub("%", "", df.carries$DefendersBeatenPCT)) 
df.carries$LQB = as.numeric(gsub("%", "", df.carries$LQB)) 
df.carries$Gainline = as.numeric(gsub("%", "", df.carries$Gainline))

df.car.overall = df.carries[which(is.na(df.carries$MatchID1)), ]
df.car.overall = df.car.overall[-1, ]
df.car.detail = df.carries[-which(is.na(df.carries$MatchID1)), ]
df.car.combine = merge(df.car.detail, df.profile, by = "Player")
df.car.cor = merge(df.car.overall, df.profile, by = "Player")

# Breakdown
df.breakdown = read.table("./datasets/Breakdown.csv", skip = 2, sep = ",",
                          quot="", header = T, stringsAsFactors = F)

colnames(df.breakdown) = gsub("\\.", "", colnames(df.breakdown))
df.breakdown$OOAAEffectiveness = as.numeric(gsub("%", "", df.breakdown$OOAAEffectiveness)) 
df.breakdown$OOADEffectiveness = as.numeric(gsub("%", "", df.breakdown$OOADEffectiveness))
df.brk.overall = df.breakdown[which(is.na(df.breakdown$MatchID1)), ]
df.brk.detail = df.breakdown[-which(is.na(df.breakdown$MatchID1)), ]
df.brk.combine = merge(df.brk.detail, df.profile, by = "Player")
df.brk.cor = merge(df.brk.overall, df.profile, by = "Player")
