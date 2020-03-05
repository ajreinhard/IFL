library(xml2)
library(htmlTable)
library(scales)

setwd('C:/Users/Owner/Documents/GitHub/IFL')

sim_df <- read.csv('sim results.csv', stringsAsFactors = F, row.names = 1)
ifl_teams <- row.names(sim_df)
games_df <- read.csv('sched.csv', stringsAsFactors = F)
teams_df <- read.csv('teams.csv', stringsAsFactors = F)
implied_start_df <- read.csv('rate start.csv', stringsAsFactors = F)

K_fct <- 22
home_adv <- 40
implied_start <- implied_start_df$Rate

rate_start <- data.frame('Tm'=sort(teams_df$Abbr), Rate=implied_start, stringsAsFactors = F)
gms_ply_df <- games_df[which(!is.na(games_df$HomeScore)),]

rate_curr <- rate_start
ply_gm_df <- c()
rate_wk <- rate_start$Rate

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
  rate_new <- data.frame('Tm'=c(this_wk$Away, this_wk$Home), 'NewRate'=c(this_wk$RateAwayNew, this_wk$RateHomeNew), stringsAsFactors = F)
  rate_curr <- merge(rate_curr, rate_new, by = 'Tm', all.x = T)
  rate_curr$Rate <- ifelse(is.na(rate_curr$NewRate), rate_curr$Rate, rate_curr$NewRate)
  rate_curr$NewRate <- NULL
  ply_gm_df <- rbind(ply_gm_df, this_wk)
  rate_wk <- rbind(rate_curr$Rate,rate_wk)
}

if (!is.null(ply_gm_df)) {
games_df <- merge(games_df, ply_gm_df[,c('Away','Home','Week','RateHome','RateAway')], by = c('Away','Home','Week'), all.x = T)
rnk_chg <- rank(rate_wk[1,] + 1:13/10000)-rank(rate_wk[2,] + 1:13/10000)
rnk_chg <- ifelse(rnk_chg==0,'No',ifelse(rnk_chg>0,paste0('Up_',rate_curr$Tm),paste0('Down_',rate_curr$Tm)))
} else {
games_df$RateHome <- NA
games_df$RateAway <- NA
rnk_chg <- rep('No',13)
}

ifl_rank <- row.names(sim_df)[order(-(sim_df$current_ratings + sim_df$po_odds/1000))]

games_df <- merge(merge(games_df, sim_df[,c('pre_season_ratings','current_ratings')], by.x = 'Away', by.y = 'row.names', all.x = T), sim_df[,c('pre_season_ratings','current_ratings')], by.x = 'Home', by.y = 'row.names', all.x = T, suffixes = c('Away','Home'))
games_df$pre_season_ratingsAway <- NULL
games_df$pre_season_ratingsHome <- NULL

games_df$current_ratingsHome <- ifelse(is.na(games_df$RateHome),games_df$current_ratingsHome,games_df$RateHome)
games_df$current_ratingsAway <- ifelse(is.na(games_df$RateAway),games_df$current_ratingsAway,games_df$RateAway)

games_df$Time <- as.numeric(games_df$Time)
games_df$GameDateTime <- as.POSIXct(strptime(paste0(games_df$Date,' ', ifelse(games_df$Time==12,games_df$Time, games_df$Time+12),':00:00'),'%m/%d/%Y %H:%M:%S'))
games_df$HomeWinProb <- 1/(1+10^(-(games_df$current_ratingsHome - games_df$current_ratingsAway + home_adv)/400))
games_df$HomeSpread <- (games_df$current_ratingsHome - games_df$current_ratingsAway + home_adv)/17.5
games_df$weekday <- format(games_df$GameDateTime, format = "%a")

games_df <- games_df[order(games_df$GameDateTime),]
games_df$GameID <- 1:nrow(games_df)

sim_df$pretty_rate_curr <- sprintf("%+.2f",round((sim_df$current_ratings - 1500)/17.5,2))
sim_df$arrow <- paste0('<img src="tv-logo/arrow',rnk_chg,'.png" height=26em align="right" vertical-align="middle">')
sim_df$Record <- paste0(sim_df$wins,'-',sim_df$losses)

primary_df <- data.frame('arrow'=sim_df$arrow,'Team' = ifl_teams, 'Record'=sim_df$Record, 'Rating'=sim_df$pretty_rate_curr, stringsAsFactors = F)
primary_df$'Playoff %' <- percent(sim_df$po_odds,.1)
primary_df$'Champion %' <- percent(sim_df$champ_odds,.1)
primary_df$'Expected Wins' <- sprintf("%.1f",round(sim_df$wins_avg,1))
primary_df <- primary_df[order(-(sim_df$current_ratings + sim_df$po_odds/1000)),]

color_df <- merge(primary_df[,c('Team','Team')], teams_df[,c('Abbr','Color1','Color2')], by.x = 'Team', by.y = 'Abbr', all.x = T, sort = F)
cell_color <- matrix(paste0('color:',color_df$Color2,';background-color:',color_df$Color1),ncol = ncol(primary_df)-2, nrow = nrow(primary_df))
align_arg <- 'lccc'

primary_df$Team <- paste0('#',1:nrow(primary_df),' ',primary_df$Team, ' (',primary_df$Record,')',primary_df$arrow)
primary_df$Record <- NULL
primary_df$arrow <- NULL

games_df$concat <- paste0(games_df$Away,'_',games_df$Home)


tm <- ifl_teams[1]
tm_tbls <- sapply(ifl_teams, function(tm) {
  tm_games <- games_df[grepl(tm, games_df$concat),]
  tm_games$'Opponent' <- ifelse(tm_games$Home==tm, tm_games$Away, tm_games$Home)
  tm_games$'Win Probability' <- percent(ifelse(tm_games$Home==tm, tm_games$HomeWinProb, 1 - tm_games$HomeWinProb),.1)
  tm_games$tm_spread <- round(ifelse(tm_games$Home==tm, tm_games$HomeSpread, -tm_games$HomeSpread),1)
  tm_games$'Point Spread' <- ifelse(tm_games$tm_spread == 0,'Toss Up',ifelse(tm_games$tm_spread > 0, paste0('Fav by ',tm_games$tm_spread), paste0('Dog by ',-tm_games$tm_spread)))
  tm_games$'Game Info' <- paste0(tm_games$weekday,' ',gsub('/2020','',tm_games$Date),' @',tm_games$Time,'pm ET')
  tm_games$Score <- paste0(ifelse(tm_games$Home==tm, ifelse(tm_games$HomeScore > tm_games$AwayScore, 'W ','L '),ifelse(tm_games$HomeScore > tm_games$AwayScore, 'L ','W ')), ifelse(tm_games$Home==tm, paste0(tm_games$HomeScore,'-',tm_games$AwayScore), paste0(tm_games$AwayScore,'-',tm_games$HomeScore)))
  tm_games$Score[which(is.na(tm_games$HomeScore))] <- ''
  tm_games$Score <- ifelse(tm_games$Score=='','',tm_games$Score)
  tm_games$OppRank <- match(tm_games$'Opponent',ifl_rank)
  tm_games <- merge(tm_games, teams_df[,c('Abbr','Full','Color1','Color2')], by.x = 'Opponent', by.y = 'Abbr', all.x = T, sort = F)
  tm_games$'Opponent' <- paste0(ifelse(tm_games$Home==tm,'','@  '),'#',tm_games$OppRank,' ',tm_games$Full)
  bye_wk <- which(is.na(match(1:17,tm_games$Week)))
  bye_wk_df <- data.frame(t(sapply(1:3, function(x) rep('',length(tm_games[0,])))),stringsAsFactors=F)
  names(bye_wk_df) <- names(tm_games)
  bye_wk_df$Week <- bye_wk
  bye_wk_df$'Opponent' <- 'BYE'
  tm_games <- data.frame(rbind(bye_wk_df, tm_games),stringsAsFactors=F)
  tm_games$Week <- as.numeric(tm_games$Week)
  names(tm_games) <- gsub('\\.',' ',names(tm_games))
  tbl_only <- htmlTable(tm_games[order(tm_games$Week),c('Week','Game Info','Opponent','Win Probability','Point Spread','Score')], rnames = F, css.class='team')
  button_top  <- paste0('<button type="button" class="collapsible" style="background-image: url(\'logos/',tm,'.png\');">',teams_df$Full[match(tm,teams_df$Abbr)],'</button><div class="content"><p>')
  button_bottom <- '</p></div>'
  return(paste0(button_top,tbl_only,button_bottom))
})

tm_tbls_header <- '<h2>Team Schedules</h2><p>Below are my full-season forecasts for each IFL team. Click on each team to expand their schedule.</p>'

curr_week <- games_df$Week[match(NA,games_df$HomeScore)]
upcom_gms <- games_df[which(games_df$Week==curr_week),]
upcom_gms <- merge(merge(upcom_gms, teams_df, by.x = 'Away', by.y = 'Abbr', all.x = T), teams_df, by.x = 'Home', by.y = 'Abbr', all.x = T, suffixes = c('Away','Home'))
upcom_gms <- upcom_gms[order(upcom_gms$GameDateTime),]

upcom_gms$GameHeaderAway <- paste0('#',match(upcom_gms$Away,ifl_rank),' ',upcom_gms$LocAway,ifelse(upcom_gms$HomeSpread<0,paste0('(',sprintf("%.1f",round(upcom_gms$HomeSpread,1)),')'),''))
upcom_gms$GameHeaderHome <- paste0('#',match(upcom_gms$Home,ifl_rank),' ',upcom_gms$LocHome,ifelse(upcom_gms$HomeSpread>0,paste0('(-',sprintf("%.1f",round(upcom_gms$HomeSpread,1)),')'),''))

upcom_gms$GameByLine <- ifelse(is.na(upcom_gms$HomeScore), paste0(upcom_gms$weekday,' at ',upcom_gms$Time,'pm ET'), paste0(upcom_gms$Away,' ',upcom_gms$AwayScore,', ',upcom_gms$Home,' ',upcom_gms$HomeScore))

upcoming_games <- paste(paste0('<tr>',
			'<td colspan=3 class="win-bar">',
			'<div class="team-bar" style="width:',((1-upcom_gms$HomeWinProb)/2)*100,'%;left:',(upcom_gms$HomeWinProb/2)*100,'%;background-color:',upcom_gms$Color1Away,';border-color:',upcom_gms$Color2Away,';text-align:left;color:',upcom_gms$Color2Away,'">',percent(1-upcom_gms$HomeWinProb,.1),'</div>',
			'<div class="team-bar" style="width:',(upcom_gms$HomeWinProb/2)*100,'%;left:',(upcom_gms$HomeWinProb/2)*100,'%;background-color:',upcom_gms$Color1Home,';border-color:',upcom_gms$Color2Home,';text-align:right;color:',upcom_gms$Color2Home,'">',percent(upcom_gms$HomeWinProb,.1),'</div>',
			'</td>',
			'</tr><tr>',
			'<td rowspan="4"><img src="logos/',upcom_gms$Away,'.png" height="130em"></td>',
			'<td align="left">',upcom_gms$GameHeaderAway,'</td>',
			'<td align="right" rowspan="4"><img src="logos/',upcom_gms$Home,'.png" height="130em"></td>',
			'</tr><tr>',
			'<td align="center">At</td>',
			'</tr><tr>',
			'<td align="right">',upcom_gms$GameHeaderHome,'</td>',
			'</tr><tr>',
			'<td align="center">',upcom_gms$GameByLine,'</td>',
			'</tr>')
			,collapse='')

up_gms_head <- paste0('<h2>Upcoming Games</h2><p>Here are my forecasts for this week\'s IFL games! The length of the bars below reflect win probability and point spread can be found in the parenthesis.</p>')
upcoming_games <- paste0(up_gms_head,'<table class="games-now"><tbody>',upcoming_games,'</tbody></table>')


###create champ matrix
sim_res_df <- read.csv('sim_res.csv', stringsAsFactors=F)
champ_match <- apply(sim_res_df[,c('IFL_CHAMP','IFL_RU')], 1, function(x) paste(sort(x), collapse = '_'))

east <- ifl_teams
west <- ifl_teams

chmp_mx <- sapply(east, function(tm) ifelse(grepl(tm, champ_match),1,0))

tm1 <- sapply(strsplit(champ_match,'_'),function(x) x[1])
tm2 <- sapply(strsplit(champ_match,'_'),function(x) x[2])

chmp_mx <- rbind(chmp_mx,chmp_mx)
chmp_teams <- c(tm1,tm2)

sim_cnt <- aggregate(chmp_mx~chmp_teams, FUN = sum)
sim_cnt$chmp_teams <-NULL
sim_cnt <- sim_cnt/nrow(sim_res_df)
diag(sim_cnt) <- 0
chmp_color_mx <- sapply(sim_cnt, function(x) paste0('background-color:',rgb(1-((x)/max(sim_cnt)),1,1-((x)/max(sim_cnt)))))
sim_cnt <- sapply(sim_cnt, percent, accuracy = .1)
diag(sim_cnt) <- ''
row.names(sim_cnt) <- paste0('<img src="logos/',ifl_teams,'.png" height=40em width=auto>')
colnames(sim_cnt) <- paste0('<img src="logos/',ifl_teams,'.png" height=40em width=auto>')


chmp_intro <- paste0('<h2>2020 United Bowl Matchup Odds</h2><p>The matrix below shows the probability of each possible United Bowl matchup. ',
			'The United Bowl will be played at the home of the highest seeded team on the weekend of July 25th.</p>')

chmp_html <- paste0(chmp_intro,htmlTable(sim_cnt, css.class = 'championship-matrix', css.cell = chmp_color_mx))
chmp_html <- gsub('border-bottom: 1px solid grey','',chmp_html)
###end champ matrix

header <- '<head><title>IFL Elo Projections</title><link rel="stylesheet" type="text/css" href="ajr-theme.css?v=4"></head>'


last_update <- format(file.info('sim results.csv')$mtime,'%h %d @ %I:%M%p %Z')
my_sect <- paste0('<body><h1>IFL Elo Projections</h1>',
		'<p class="sect">Welcome! This page is dedicated IFL Elo rankings. The methodology I\'m using is ',
		'based on <a href = "https://fivethirtyeight.com/features/introducing-nfl-elo-ratings/">FiveThirtyEight\'s ',
		'pre-2019 NFL elo model</a>. Pre-season ratings were computed based on ',
		'IFL Elo ratings from prior seasons. ',
		'The difference between two team\'s ratings is equal to the point spread between those two teams. ',
		'The home team will also get a 2.3 point home-field advantage. An average team will have a rating of zero. ',
		'Projections below are based on 10,000 simulations. ',
		'This page will be updated as games are played.</p>',
		'<p><b>Last Update: ',last_update,'</b></p>')
coll_scrpt <- '<script src="collapsible.js"></script>'

primary_html <- htmlTable(primary_df, css.class='primary', css.cell=cell_color, align=align_arg, rnames = F)
primary_html <- gsub('border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;','',primary_html)

my_html <- paste0(header,paste(c(my_sect,primary_html,chmp_html,upcoming_games,tm_tbls_header,tm_tbls,coll_scrpt), collapse = ''))

write_html(read_html(my_html), 'index.html')
