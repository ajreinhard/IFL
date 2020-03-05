
all_gms
all_gms$concat <- paste0(all_gms$Away,'_',all_gms$Home)
com_gms <- which(apply(sapply(tms_vec, function(x) ifelse(grepl(x, all_gms$concat),1,0)),1,sum)==2)

all_gms[grepl('CDR', all_gms$concat),]

table(all_gms$Week)
rate_curr


str(games_df)


install.packages('webshot')
library(webshot)
webshot::install_phantomjs()
webshot("C:/Users/rei1740/Desktop/Anthony/XFL/xfl.html", "r.png")
