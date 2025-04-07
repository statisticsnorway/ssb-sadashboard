


dat <- PxWebApiData::ApiData("https://data.ssb.no/api/v0/no/table/08771/",NACE2007=TRUE,Contents="LedigeStillinger",Tid=TRUE)$data[,-2]

df <- data.frame(NACE = dat$NACE, Tid  = dat$Tid, value= dat$value)

ledige <- tidyr::spread(df, NACE, value)
#ledige <- vhi[,-which(nchar(colnames(hi))>5)]

tidsserier <- ts(ledige,freq=4,start=c(2010,1))[,-1]
ledige <- tidsserier


mysa <- list()

spec_now <- RJDemetra::x13_spec(spec="RSA5c")

for(i in 1:ncol(tidsserier)){
  tidsserier_now <- tidsserier[,i]
  mysa[[i]] <- pickmdl::x13_pickmdl(tidsserier_now,spec_now,corona=FALSE,pickmdl_method="first_tryautomdl",old_crit2=FALSE,
                                    userdefined = c("decomposition.a1","decomposition.a8","decomposition.b1","decomposition.d11",
                                                    "decomposition.d12","diagnostics.seas-sa-f","diagnostics.seas-sa-qs",
                                                    "diagnostics.seas-sa-friedman","diagnostics.residual.all","residuals.normality.value",
                                                    "residuals.normality","residuals.independence.value","residuals.independence"))
}

names(mysa) <- colnames(tidsserier)

#groups_series <- c("45","46",paste0("47.",1:7),"47.9")
#groups_now <- lapply(groups_series,function(x){colnames(tidsserier)[which(grepl(x,colnames(tidsserier)))]})
#names(groups_now) <- groups_series


to_html <- "/home/onyxia/work/sadashboard/development/my_report.html"

sa_quality_report(mysa,report_file=to_html,title_report = "Testrapport",author_report="S811",#group_series=groups_now,
                  linearized=TRUE,ma_filter=TRUE,cal_adjust=TRUE)
