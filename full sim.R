setwd('C:/Users/Owner/Documents/GitHub/IFL')

teams_df <- read.csv('teams.csv', stringsAsFactors=F)
games_df <- read.csv('sched.csv', stringsAsFactors = F)
implied_start_df <- read.csv('rate start.csv', stringsAsFactors = F)

K_fct <- 22
home_adv <- 40
reg_szn_gms <- 17
bye_wks <- 3
sim_cnt <- 10000

implied_start <- implied_start_df$Rate
(sum(implied_start) - 1500*13) / 25
names(implied_start) <- NULL

rate_start <- data.frame('Tm'=sort(teams_df$Abbr), Rate=implied_start, stringsAsFactors = F)
gms_ply_df <- games_df[which(!is.na(games_df$HomeScore)),]
reg_games_rem_df <- games_df[which(is.na(games_df$HomeScore) & games_df$Week <= reg_szn_gms),]


rate_curr <- rate_start
pred_gm_df <- c()
ply_gm_df <- c()

for (wk in unique(gms_ply_df$Week)) {
  this_wk <- gms_ply_df[which(gms_ply_df$Week==wk),]
  this_wk <- merge(merge(this_wk, rate_curr, by.x = 'Home', by.y = 'Tm'),rate_curr, by.x = 'Away', by.y = 'Tm', suffix = c('Home','Away'))
  this_wk$HomeWinProb <- 1/(1+10^(-(this_wk$RateHome - this_wk$RateAway + home_adv)/400))
  this_wk$HomeWin <- ifelse(this_wk$HomeScore > this_wk$AwayScore, 1, 0)
  this_wk$EloChng <-  (this_wk$HomeWin - this_wk$HomeWinProb) * 
    K_fct *
    log(abs(this_wk$HomeScore - this_wk$AwayScore) + 1) * 
    (2.2/(2.2 + (ifelse(this_wk$HomeWin==1,1/1000,-1/1000) * (this_wk$RateHome - this_wk$RateAway + home_adv))))
  this_wk$RateHomeNew <- this_wk$RateHome + this_wk$EloChng
  this_wk$RateAwayNew <- this_wk$RateAway - this_wk$EloChng
  this_wk$WinnerMargin <- abs(this_wk$HomeScore - this_wk$AwayScore)
  rate_new <- data.frame('Tm'=c(this_wk$Away, this_wk$Home), 'NewRate'=c(this_wk$RateAwayNew, this_wk$RateHomeNew), stringsAsFactors = F)
  rate_curr <- merge(rate_curr, rate_new, by = 'Tm', all.x = T)
  rate_curr$Rate <- ifelse(is.na(rate_curr$NewRate), rate_curr$Rate, rate_curr$NewRate)
  rate_curr$NewRate <- NULL
  ply_gm_df <- rbind(ply_gm_df, this_wk)
}

curr_ratings <- rate_curr$Rate

sim_res <- sapply(1:sim_cnt, function(sm) {
  
  for (wk in unique(reg_games_rem_df$Week)) {
    this_wk <- reg_games_rem_df[which(reg_games_rem_df$Week==wk),]
    this_wk <- merge(merge(this_wk, rate_curr, by.x = 'Home', by.y = 'Tm'),rate_curr, by.x = 'Away', by.y = 'Tm', suffix = c('Home','Away'))
    this_wk$HomeWinProb <- 1/(1+10^(-(this_wk$RateHome - this_wk$RateAway + home_adv)/400))
    this_wk$HomeWin <- ifelse(runif(nrow(this_wk)) < this_wk$HomeWinProb, 1, 0)
    this_wk$EloChng <- (this_wk$HomeWin - this_wk$HomeWinProb) * K_fct
    this_wk$RateHomeNew <- this_wk$RateHome + this_wk$EloChng
    this_wk$RateAwayNew <- this_wk$RateAway - this_wk$EloChng
    this_wk$WinnerMargin <- ceiling(abs(rnorm(nrow(this_wk), mean=0, sd=20)))
    rate_new <- data.frame('Tm'=c(this_wk$Away, this_wk$Home), 'NewRate'=c(this_wk$RateAwayNew, this_wk$RateHomeNew), stringsAsFactors = F)
    rate_curr <- merge(rate_curr, rate_new, by = 'Tm', all.x = T)
    rate_curr$Rate <- ifelse(is.na(rate_curr$NewRate), rate_curr$Rate, rate_curr$NewRate)
    rate_curr$NewRate <- NULL
    pred_gm_df <- rbind(pred_gm_df, this_wk)
  }

  all_gms <- rbind(ply_gm_df, pred_gm_df)
  
  all_gms$TmWinner <- factor(ifelse(all_gms$HomeWin==1 & all_gms$Week <= reg_szn_gms, all_gms$Home, all_gms$Away), levels = rate_curr$Tm)
  all_gms$TmLoser <- factor(ifelse(all_gms$HomeWin==0 & all_gms$Week <= reg_szn_gms, all_gms$Home, all_gms$Away), levels = rate_curr$Tm)
  
  rate_curr$Wins <- table(all_gms$TmWinner)
  rate_curr$WPCT <- rate_curr$Wins/(reg_szn_gms-bye_wks)
  
  all_gms <- merge(merge(all_gms, rate_curr[,c('Tm','WPCT')], by.x = 'Away', by.y = 'Tm', all.x = T), rate_curr[,c('Tm','WPCT')], by.x = 'Home', by.y = 'Tm', all.x = T, suffixes = c('Away','Home'))
  all_gms$LoserWPCT <- ifelse(all_gms$HomeWin==1, all_gms$WPCTAway, all_gms$WPCTHome)
  all_gms$WinnerWPCT <- ifelse(all_gms$HomeWin==0, all_gms$WPCTAway, all_gms$WPCTHome)
  SOV_calc <- aggregate(LoserWPCT ~ TmWinner, data = all_gms, FUN = mean)
  SOL_calc <- aggregate(WinnerWPCT ~ TmLoser, data = all_gms, FUN = mean)
  rate_curr <- merge(rate_curr, SOV_calc, by.x = 'Tm', by.y = 'TmWinner', all.x = T)
  rate_curr <- merge(rate_curr, SOL_calc, by.x = 'Tm', by.y = 'TmLoser', all.x = T)
  rate_curr$SOS <- rate_curr$LoserWPCT * (rate_curr$WPCT) + rate_curr$WinnerWPCT * (1 - rate_curr$WPCT)
  
  Mar_W <- aggregate(WinnerMargin ~ TmWinner, data = all_gms, FUN = sum)
  Mar_L <- aggregate(-WinnerMargin ~ TmLoser, data = all_gms, FUN = sum)
  rate_curr <- merge(rate_curr, Mar_W, by.x = 'Tm', by.y = 'TmWinner', all.x = T)
  rate_curr <- merge(rate_curr, Mar_L, by.x = 'Tm', by.y = 'TmLoser', all.x = T)
  rate_curr$Pts_mar <- rate_curr$WinnerMargin + rate_curr$'-WinnerMargin'
  
  h2h_calc <- function(all_gms, tms_vec) {
    all_gms$concat <- paste0(all_gms$Away,'_',all_gms$Home)
    com_gms <- which(apply(sapply(tms_vec, function(x) ifelse(grepl(x, all_gms$concat),1,0)),1,sum)==2)
    com_wins <- table(all_gms$TmWinner[com_gms])
    return(ifelse(is.na(match(tms_vec,names(which(com_wins==max(com_wins))))),0,1))
  }
  
  h2h_pts_calc <- function(all_gms, tms_vec) {
    all_gms$concat <- paste0(all_gms$Away,'_',all_gms$Home)
    com_gms <- which(apply(sapply(tms_vec, function(x) ifelse(grepl(x, all_gms$concat),1,0)),1,sum)==2)
    if (length(com_gms)==0) {
      return(rep(0, length(tms_vec)))
    } else {
      com_gms_W_mar <- aggregate(WinnerMargin ~ TmWinner, data = all_gms[com_gms,], FUN=sum, drop=F)
      com_gms_L_mar <- aggregate(-WinnerMargin ~ TmLoser, data = all_gms[com_gms,], FUN=sum, drop=F)
      com_gms_tot_mar <- ifelse(is.na(com_gms_W_mar$WinnerMargin),0,com_gms_W_mar$WinnerMargin) + ifelse(is.na(com_gms_L_mar$'-WinnerMargin'),0,com_gms_L_mar$'-WinnerMargin')
      names(com_gms_tot_mar) <- com_gms_W_mar$TmWinner
      return(ifelse(is.na(match(tms_vec,names(which(com_gms_tot_mar==max(com_gms_tot_mar))))),0,1))
    }
  }
  
  tied_wpct <- names(which(table(rate_curr$WPCT)>1))
  rate_curr$H2H <- 0
  rate_curr$H2H_pts <- 0
  for (x in tied_wpct) {
    rate_curr$H2H[which(rate_curr$WPCT==x)] <- h2h_calc(all_gms, rate_curr$Tm[which(rate_curr$WPCT==x)])
    rate_curr$H2H_pts[which(rate_curr$WPCT==x)] <- h2h_pts_calc(all_gms, rate_curr$Tm[which(rate_curr$WPCT==x)])
  }
  rate_curr$TB_ord <- rate_curr$WPCT + rate_curr$H2H/1000 + rate_curr$H2H_pts/10000 + rate_curr$SOS/1000 + (rate_curr$Pts_mar+1000)/100000000 + runif(nrow(rate_curr))/1000000000000
  rate_curr <- rate_curr[order(-rate_curr$TB_ord),]

  #quarter
  qtr_win_prob <- sapply(1:4, function(fav_sd) 1/(1+10^(-(rate_curr$Rate[fav_sd] - rate_curr$Rate[9-fav_sd] + home_adv)/400)))
  qtr_hm_win <- ifelse(runif(4) < qtr_win_prob, 1, 0)
  
  EloChng_PO <- (qtr_hm_win - qtr_win_prob) * K_fct
  PO_chng <- data.frame('Tm'=rate_curr$Tm[1:8],'Chng'=c(EloChng_PO,-rev(EloChng_PO)),stringsAsFactors = F)
  rate_curr <- merge(rate_curr, PO_chng, by = 'Tm', all.x = T)
  rate_curr$Chng[is.na(rate_curr$Chng)] <- 0
  rate_curr$Rate <- rate_curr$Rate + rate_curr$Chng
  rate_curr$Chng <- NULL
  rate_curr <- rate_curr[order(-rate_curr$TB_ord),]
  seeds_to_semi <- sort(ifelse(qtr_hm_win==1, 1:4, 8:5))
  
  #semi
  semi_win_prob <- sapply(1:2, function(fav_sd) 1/(1+10^(-(rate_curr$Rate[seeds_to_semi[fav_sd]] - rate_curr$Rate[seeds_to_semi[5-fav_sd]] + home_adv)/400)))
  semi_hm_win <- ifelse(runif(2) < semi_win_prob, 1, 0)
  
  EloChng_PO <- (semi_hm_win - semi_win_prob) * K_fct
  PO_chng <- data.frame('Tm'=rate_curr$Tm[seeds_to_semi],'Chng'=c(EloChng_PO,-rev(EloChng_PO)),stringsAsFactors = F)
  rate_curr <- merge(rate_curr, PO_chng, by = 'Tm', all.x = T)
  rate_curr$Chng[is.na(rate_curr$Chng)] <- 0
  rate_curr$Rate <- rate_curr$Rate + rate_curr$Chng
  rate_curr$Chng <- NULL
  rate_curr <- rate_curr[order(-rate_curr$TB_ord),]
  seeds_to_final <- sort(ifelse(semi_hm_win==1, seeds_to_semi[1:2], seeds_to_semi[4:3]))
  
  
  #champ
  chmp_win_prob <- 1/(1+10^(-(rate_curr$Rate[seeds_to_final[1]] - rate_curr$Rate[seeds_to_final[2]] + home_adv)/400))
  chmp_hm_win <- ifelse(runif(1) < chmp_win_prob, 1, 0)
  
  EloChng_chmp <- (chmp_hm_win - chmp_win_prob) * K_fct
  chmp_chng <- data.frame('Tm'=rate_curr$Tm[seeds_to_final],'Chng'=c(EloChng_chmp,-EloChng_chmp),stringsAsFactors = F)
  rate_curr <- merge(rate_curr, chmp_chng, by = 'Tm', all.x = T)
  rate_curr$Chng[is.na(rate_curr$Chng)] <- 0
  rate_curr$Rate <- rate_curr$Rate + rate_curr$Chng
  rate_curr$Chng <- NULL
  rate_curr <- rate_curr[order(-rate_curr$TB_ord),]

  po_seeds <- rate_curr$Tm[1:8]
  semi_losers <- ifelse(semi_hm_win==0,rate_curr$Tm[seeds_to_semi[1:2]],rate_curr$Tm[seeds_to_semi[4:3]])
  runner_up <- ifelse(chmp_hm_win==0,rate_curr$Tm[seeds_to_final[1]],rate_curr$Tm[seeds_to_final[2]])
  champ <- ifelse(chmp_hm_win==1,rate_curr$Tm[seeds_to_final[1]],rate_curr$Tm[seeds_to_final[2]])
  
  winners <- as.character(all_gms$TmWinner[which(all_gms$Week==min(reg_games_rem_df$Week))])
  winners <- c(winners,rep(NA, 6-length(winners)))
  
  #re order alpha
  rate_curr <- rate_curr[order(rate_curr$Tm),]
  
  return(c(rate_curr$Rate, rate_curr$Wins, po_seeds, semi_losers, runner_up, champ, winners))
})


sim_res_df <- data.frame(t(sim_res), stringsAsFactors = F)
for (i in 1:26) sim_res_df[,i] <- as.numeric(sim_res_df[,i])
ifl_tms <- sort(rate_start$Tm)
names(sim_res_df) <- c(paste0(ifl_tms,'_Rating'),paste0(ifl_tms,'_Wins'),paste0('SEED',1:8),paste0('SEMI_LOSER_',1:2),'IFL_RU','IFL_CHAMP',paste0('WeeklyWinner',1:6))

po_odds <- table(factor(unlist(sim_res_df[,paste0('SEED',1:8)]), levels = ifl_tms))/sim_cnt
home_po_odds <- table(factor(unlist(sim_res_df[,paste0('SEED',1:4)]), levels = ifl_tms))/sim_cnt
semi_odds <- table(factor(unlist(sim_res_df[,c(paste0('SEMI_LOSER_',1:2),'IFL_RU','IFL_CHAMP')]), levels = ifl_tms))/sim_cnt
final_odds <- table(factor(unlist(sim_res_df[,c('IFL_RU','IFL_CHAMP')]), levels = ifl_tms))/sim_cnt
champ_odds <- table(factor(sim_res_df$IFL_CHAMP, levels = ifl_tms))/sim_cnt
wins_avg <- apply(sim_res_df[,paste0(ifl_tms,'_Wins')],2,mean)
rate_avg <- apply(sim_res_df[,paste0(ifl_tms,'_Rating')],2,mean)

sim_final_df <- data.frame(cbind(po_odds,home_po_odds,semi_odds,final_odds,champ_odds,rate_avg,wins_avg),stringsAsFactors = F)

if (length(ply_gm_df)!=0) {
  winners <- ifelse(ply_gm_df$HomeWin==1,ply_gm_df$Home,ply_gm_df$Away)
  losers <- ifelse(ply_gm_df$HomeWin==0,ply_gm_df$Home,ply_gm_df$Away)
} else {
  winners <- NA
  losers <- NA
}

sim_final_df$pre_season_ratings <- implied_start
sim_final_df$current_ratings <- curr_ratings
sim_final_df$wins <- table(factor(winners, sort(teams_df$Abbr)))
sim_final_df$losses <- table(factor(losers, sort(teams_df$Abbr)))

write.csv(sim_final_df, 'sim results.csv')
write.csv(sim_res_df,'sim_res.csv',row.names=F)
