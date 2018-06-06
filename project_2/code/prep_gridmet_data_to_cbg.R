# Date: 2018-05-02
# S Ogletree
# Description: prepare cbg wx data for UTCI

library(dplyr)
library(ggplot2)


cbgs <- readRDS("../../data/cbg_data/cbg_base.rds")

wxdata <- readRDS("../../data/GridMET_data/cbg_wx_data_long.rds")

wxsub <- wxdata[1:100,]

# UTCI function
myUTCI <- function(Ta, RH, ws, solar){
  solardirect <- ifelse(solar > 1000, 0.75 * solar, 
                        ifelse(solar > 250, (-0.001 * solar + 1.25) * solar, 0))
  solardiffuse <- ifelse(solar > 1000, 0.25 * solar,
                         ifelse(solar > 250, (0.001 * solar - 0.25) * solar, solar))
  
  Tmrt <-(0.97 * (Ta + 273.2) ^ 4 + 0.7 * solardiffuse / (0.97 * 0.0000000567) + 0.32 * 0.7 * solardirect / (0.97 * 0.0000000567)) ^ 0.25 - 273.2
  
  RHD <-  RH / 100
  Td <-  237.3 * (log(RHD) / 17.27 + Ta / (237.3 + Ta)) / (1 - log(RHD) / 17.27 - Ta / (237.3 + Ta))
  
  PA <-  0.6108 * exp(17.29 * Td / (Td + 237.3)) # takes Td and changes it into Pa - vapour pressure in hPa)
  DTmrt <-  Tmrt - Ta
  # calculate 6th order polynomial as approximation
  ET1 <-  Ta + 0.607562052 - 0.0227712343 * Ta + 0.000806470249 * Ta * Ta - 0.000154271372 * Ta * Ta * Ta - 0.00000324651735 * Ta * Ta * Ta * Ta + 7.32602852E-08 * Ta * Ta * Ta * Ta * Ta + 1.35959073E-09 * Ta * Ta * Ta * Ta * Ta * Ta - 2.2583652 * ws + 0.0880326035 * Ta * ws + 0.00216844454 * Ta * Ta * ws - 0.0000153347087 * Ta * Ta * Ta * ws - 0.000000572983704 * Ta * Ta * Ta * Ta * ws - 2.55090145E-09 * Ta * Ta * Ta * Ta * Ta * ws - 0.751269505 * ws * ws - 0.00408350271 * Ta * ws * ws - 0.0000521670675 * Ta * Ta * ws * ws + 0.00000194544667 * Ta * Ta * Ta * ws * ws + 1.14099531E-08 * Ta * Ta * Ta * Ta * ws * ws + 0.158137256 * ws * ws * ws - 0.0000657263143 * Ta * ws * ws * ws + 0.000000222697524 * Ta * Ta * ws * ws * ws - 4.16117031E-08 * Ta * Ta * Ta * ws * ws * ws - 0.0127762753 * ws * ws * ws * ws + 0.00000966891875 * Ta * ws * ws * ws * ws + 2.52785852E-09 * Ta * Ta * ws * ws * ws * ws + 0.000456306672 * ws * ws * ws * ws * ws - 0.000000174202546 * Ta * ws * ws * ws * ws * ws - 0.00000591491269 * ws * ws * ws * ws * ws * ws + 0.398374029 * DTmrt + 0.000183945314 * Ta * DTmrt - 0.00017375451 * Ta * Ta * DTmrt - 0.000000760781159 * Ta * Ta * Ta * DTmrt + 3.77830287E-08 * Ta * Ta * Ta * Ta * DTmrt + 5.43079673E-10 * Ta * Ta * Ta * Ta * Ta * DTmrt - 0.0200518269 * ws * DTmrt + 0.000892859837 * Ta * ws * DTmrt + 0.00000345433048 * Ta * Ta * ws * DTmrt - 0.000000377925774 * Ta * Ta * Ta * ws * DTmrt - 1.69699377E-09 * Ta * Ta * Ta * Ta * ws * DTmrt + 0.000169992415 * ws * ws * DTmrt - 0.0000499204314 * Ta * ws * ws * DTmrt + 0.000000247417178 * Ta * Ta * ws * ws * DTmrt + 1.07596466E-08 * Ta * Ta * Ta * ws * ws * DTmrt + 0.0000849242932 * ws * ws * ws * DTmrt + 0.00000135191328 * Ta * ws * ws * ws * DTmrt - 6.21531254E-09 * Ta * Ta * ws * ws * ws * DTmrt - 0.00000499410301 * ws * ws * ws * ws * DTmrt - 1.89489258E-08 * Ta * ws * ws * ws * ws * DTmrt + 8.15300114E-08 * ws * ws * ws * ws * ws * DTmrt + 0.00075504309 * DTmrt * DTmrt
  
  ET3 <- -0.0000565095215 * Ta * DTmrt * DTmrt + (-0.000000452166564) * Ta * Ta * DTmrt * DTmrt + (2.46688878E-08) * Ta * Ta * Ta * DTmrt * DTmrt + (2.42674348E-10) * Ta * Ta * Ta * Ta * DTmrt * DTmrt + (0.00015454725) * ws * DTmrt * DTmrt + (0.0000052411097) * Ta * ws * DTmrt * DTmrt + (-8.75874982E-08) * Ta * Ta * ws * DTmrt * DTmrt + (-1.50743064E-09) * Ta * Ta * Ta * ws * DTmrt * DTmrt + (-0.0000156236307) * ws * ws * DTmrt * DTmrt + (-0.000000133895614) * Ta * ws * ws * DTmrt * DTmrt + (2.49709824E-09) * Ta * Ta * ws * ws * DTmrt * DTmrt + (0.000000651711721) * ws * ws * ws * DTmrt * DTmrt + (1.94960053E-09) * Ta * ws * ws * ws * DTmrt * DTmrt + (-1.00361113E-08) * ws * ws * ws * ws * DTmrt * DTmrt + (-0.0000121206673) * DTmrt * DTmrt * DTmrt + (-0.00000021820366) * Ta * DTmrt * DTmrt * DTmrt + (7.51269482E-09) * Ta * Ta * DTmrt * DTmrt * DTmrt + (9.79063848E-11) * Ta * Ta * Ta * DTmrt * DTmrt * DTmrt + (0.00000125006734) * ws * DTmrt * DTmrt * DTmrt + (-1.81584736E-09) * Ta * ws * DTmrt * DTmrt * DTmrt + (-3.52197671E-10) * Ta * Ta * ws * DTmrt * DTmrt * DTmrt + (-0.000000033651463) * ws * ws * DTmrt * DTmrt * DTmrt + (1.35908359E-10) * Ta * ws * ws * DTmrt * DTmrt * DTmrt + (4.1703262E-10) * ws * ws * ws * DTmrt * DTmrt * DTmrt + (-1.30369025E-09) * DTmrt * DTmrt * DTmrt * DTmrt
  
  ET4 <- 4.13908461E-10 * Ta * DTmrt * DTmrt * DTmrt * DTmrt + (9.22652254E-12) * Ta * Ta * DTmrt * DTmrt * DTmrt * DTmrt + (-5.08220384E-09) * ws * DTmrt * DTmrt * DTmrt * DTmrt + (-2.24730961E-11) * Ta * ws * DTmrt * DTmrt * DTmrt * DTmrt + (1.17139133E-10) * ws * ws * DTmrt * DTmrt * DTmrt * DTmrt + (6.62154879E-10) * DTmrt * DTmrt * DTmrt * DTmrt * DTmrt + (4.0386326E-13) * Ta * DTmrt * DTmrt * DTmrt * DTmrt * DTmrt + (1.95087203E-12) * ws * DTmrt * DTmrt * DTmrt * DTmrt * DTmrt + (-4.73602469E-12) * DTmrt * DTmrt * DTmrt * DTmrt * DTmrt * DTmrt + (5.12733497) * PA + (-0.312788561) * Ta * PA + (-0.0196701861) * Ta * Ta * PA + (0.00099969087) * Ta * Ta * Ta * PA + (0.00000951738512) * Ta * Ta * Ta * Ta * PA + (-0.000000466426341) * Ta * Ta * Ta * Ta * Ta * PA + (0.548050612) * ws * PA + (-0.00330552823) * Ta * ws * PA + (-0.0016411944) * Ta * Ta * ws * PA + (-0.00000516670694) * Ta * Ta * Ta * ws * PA + (0.000000952692432) * Ta * Ta * Ta * Ta * ws * PA + (-0.0429223622) * ws * ws * PA + (0.00500845667) * Ta * ws * ws * PA + (0.00000100601257) * Ta * Ta * ws * ws * PA + (-0.00000181748644) * Ta * Ta * Ta * ws * ws * PA + (-0.00125813502) * ws * ws * ws * PA
  
  ET5 <- -0.000179330391 * Ta * ws * ws * ws * PA + (0.00000234994441) * Ta * Ta * ws * ws * ws * PA + (0.000129735808) * ws * ws * ws * ws * PA + (0.0000012906487) * Ta * ws * ws * ws * ws * PA + (-0.00000228558686) * ws * ws * ws * ws * ws * PA + (-0.0369476348) * DTmrt * PA + (0.00162325322) * Ta * DTmrt * PA + (-0.000031427968) * Ta * Ta * DTmrt * PA + (0.00000259835559) * Ta * Ta * Ta * DTmrt * PA + (-4.77136523E-08) * Ta * Ta * Ta * Ta * DTmrt * PA + (0.0086420339) * ws * DTmrt * PA + (-0.000687405181) * Ta * ws * DTmrt * PA + (-0.00000913863872) * Ta * Ta * ws * DTmrt * PA + (0.000000515916806) * Ta * Ta * Ta * ws * DTmrt * PA + (-0.0000359217476) * ws * ws * DTmrt * PA + (0.0000328696511) * Ta * ws * ws * DTmrt * PA + (-0.000000710542454) * Ta * Ta * ws * ws * DTmrt * PA + (-0.00001243823) * ws * ws * ws * DTmrt * PA + (-0.000000007385844) * Ta * ws * ws * ws * DTmrt * PA + (0.000000220609296) * ws * ws * ws * ws * DTmrt * PA + (-0.00073246918) * DTmrt * DTmrt * PA + (-0.0000187381964) * Ta * DTmrt * DTmrt * PA + (0.00000480925239) * Ta * Ta * DTmrt * DTmrt * PA + (-0.000000087549204) * Ta * Ta * Ta * DTmrt * DTmrt * PA + (0.000027786293) * ws * DTmrt * DTmrt * PA
  
  ET6 <-  -0.00000506004592 * Ta * ws * DTmrt * DTmrt * PA + (0.000000114325367) * Ta * Ta * ws * DTmrt * DTmrt * PA + (0.00000253016723) * ws * ws * DTmrt * DTmrt * PA + (-1.72857035E-08) * Ta * ws * ws * DTmrt * DTmrt * PA + (-3.95079398E-08) * ws * ws * ws * DTmrt * DTmrt * PA + (-0.000000359413173) * DTmrt * DTmrt * DTmrt * PA + (0.000000704388046) * Ta * DTmrt * DTmrt * DTmrt * PA + (-1.89309167E-08) * Ta * Ta * DTmrt * DTmrt * DTmrt * PA + (-0.000000479768731) * ws * DTmrt * DTmrt * DTmrt * PA + (7.96079978E-09) * Ta * ws * DTmrt * DTmrt * DTmrt * PA + (1.62897058E-09) * ws * ws * DTmrt * DTmrt * DTmrt * PA + (3.94367674E-08) * DTmrt * DTmrt * DTmrt * DTmrt * PA + (-1.18566247E-09) * Ta * DTmrt * DTmrt * DTmrt * DTmrt * PA + (3.34678041E-10) * ws * DTmrt * DTmrt * DTmrt * DTmrt * PA + (-1.15606447E-10) * DTmrt * DTmrt * DTmrt * DTmrt * DTmrt * PA + (-2.80626406) * PA * PA + (0.548712484) * Ta * PA * PA + (-0.0039942841) * Ta * Ta * PA * PA + (-0.000954009191) * Ta * Ta * Ta * PA * PA + (0.0000193090978) * Ta * Ta * Ta * Ta * PA * PA + (-0.308806365) * ws * PA * PA + (0.0116952364) * Ta * ws * PA * PA + (0.000495271903) * Ta * Ta * ws * PA * PA + (-0.0000190710882) * Ta * Ta * Ta * ws * PA * PA + (0.00210787756) * ws * ws * PA * PA
  
  ET7 <- -0.000698445738 * Ta * ws * ws * PA * PA + 
    (0.0000230109073) * Ta * Ta * ws * ws * PA * PA + 
    (0.00041785659) * ws * ws * ws * PA * PA + 
    (-0.0000127043871) * Ta * ws * ws * ws * PA * PA + 
    (-0.00000304620472) * ws * ws * ws * ws * PA * PA + 
    (0.0514507424) * DTmrt * PA * PA + 
    (-0.00432510997) * Ta * DTmrt * PA * PA + 
    (0.0000899281156) * Ta * Ta * DTmrt * PA * PA + 
    (-0.000000714663943) * Ta * Ta * Ta * DTmrt * PA * PA + 
    (-0.000266016305) * ws * DTmrt * PA * PA + 
    (0.000263789586) * Ta * ws * DTmrt * PA * PA + 
    (-0.00000701199003) * Ta * Ta * ws * DTmrt * PA * PA + 
    (-0.000106823306) * ws * ws * DTmrt * PA * PA + 
    (0.00000361341136) * Ta * ws * ws * DTmrt * PA * PA + 
    (0.000000229748967) * ws * ws * ws * DTmrt * PA * PA + 
    (0.000304788893) * DTmrt * DTmrt * PA * PA + 
    (-0.0000642070836) * Ta * DTmrt * DTmrt * PA * PA + 
    (0.00000116257971) * Ta * Ta * DTmrt * DTmrt * PA * PA + 
    (0.00000768023384) * ws * DTmrt * DTmrt * PA * PA + 
    (-0.000000547446896) * Ta * ws * DTmrt * DTmrt * PA * PA + 
    (-0.000000035993791) * ws * ws * DTmrt * DTmrt * PA * PA + 
    (-0.00000436497725) * DTmrt * DTmrt * DTmrt * PA * PA + 
    (0.000000168737969) * Ta * DTmrt * DTmrt * DTmrt * PA * PA + 
    (2.67489271E-08) * ws * DTmrt * DTmrt * DTmrt * PA * PA + 
    (3.23926897E-09) * DTmrt * DTmrt * DTmrt * DTmrt * PA * PA
  
  ET8 <- -0.0353874123 * PA * PA * PA + 
    (-0.22120119) * Ta * PA * PA * PA + 
    (0.0155126038) * Ta * Ta * PA * PA * PA + 
    (-0.000263917279) * Ta * Ta * Ta * PA * PA * PA + 
    (0.0453433455) * ws * PA * PA * PA + 
    (-0.00432943862) * Ta * ws * PA * PA * PA + 
    (0.000145389826) * Ta * Ta * ws * PA * PA * PA + 
    (0.00021750861) * ws * ws * PA * PA * PA + 
    (-0.0000666724702) * Ta * ws * ws * PA * PA * PA + 
    (0.000033321714) * ws * ws * ws * PA * PA * PA + 
    (-0.00226921615) * DTmrt * PA * PA * PA + 
    (0.000380261982) * Ta * DTmrt * PA * PA * PA + 
    (-5.45314314E-09) * Ta * Ta * DTmrt * PA * PA * PA + 
    (-0.000796355448) * ws * DTmrt * PA * PA * PA + 
    (0.0000253458034) * Ta * ws * DTmrt * PA * PA * PA + 
    (-0.00000631223658) * ws * ws * DTmrt * PA * PA * PA + 
    (0.000302122035) * DTmrt * DTmrt * PA * PA * PA + 
    (-0.00000477403547) * Ta * DTmrt * DTmrt * PA * PA * PA + 
    (0.00000173825715) * ws * DTmrt * DTmrt * PA * PA * PA + 
    (-0.000000409087898) * DTmrt * DTmrt * DTmrt * PA * PA * PA + 
    (0.614155345) * PA * PA * PA * PA + 
    (-0.0616755931) * Ta * PA * PA * PA * PA + 
    (0.00133374846) * Ta * Ta * PA * PA * PA * PA + 
    (0.00355375387) * ws * PA * PA * PA * PA + 
    (-0.000513027851) * Ta * ws * PA * PA * PA * PA
  
  ET9 <- 0.000102449757 * ws * ws * PA * PA * PA * PA + 
    (-0.00148526421) * DTmrt * PA * PA * PA * PA + 
    (-0.0000411469183) * Ta * DTmrt * PA * PA * PA * PA + 
    (-0.00000680434415) * ws * DTmrt * PA * PA * PA * PA + 
    (-0.00000977675906) * DTmrt * DTmrt * PA * PA * PA * PA + 
    (0.0882773108) * PA * PA * PA * PA * PA + 
    (-0.00301859306) * Ta * PA * PA * PA * PA * PA + 
    (0.00104452989) * ws * PA * PA * PA * PA * PA + 
    (0.000247090539) * DTmrt * PA * PA * PA * PA * PA + 
    (0.00148348065) * PA * PA * PA * PA * PA * PA
  
  fUTCI <-ET1 + ET3 + ET4 + ET5 + ET6 + ET7 + ET8 + ET9
  
  #tdf <- list()
  #tdf[1] <- Tmrt
  #tdf[2] <- fUTCI
  return(fUTCI)
}

utci <- myUTCI(Ta = wxsub$t_meanC, RH = wxsub$rh_mean, ws = wxsub$ws10m, solar = wxsub$s_rad)
utci_m <- myUTCI(Ta = wxsub$t_maxC, RH = wxsub$r_min, ws = wxsub$ws10m, solar = wxsub$s_rad)
hist(utci)
hist(utci_m)

wxdata$utciMean <- myUTCI(Ta = wxdata$t_meanC, RH = wxdata$rh_mean, ws = wxdata$ws10m, solar = wxdata$s_rad)
wxdata$utciMidday <- myUTCI(Ta = wxdata$t_maxC, RH = wxdata$r_min, ws = wxdata$ws10m, solar = wxdata$s_rad)
# a check

wxdata %>% filter(cbg_id == "360470656001") %>% ggplot(aes(dt, t_meanC, group=1)) + geom_line() + geom_line(aes(y = utciMean), color="red")

wxdata %>% filter(cbg_id == "360470656001") %>% View()
