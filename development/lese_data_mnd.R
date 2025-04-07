
dat <- PxWebApiData::ApiData("https://data.ssb.no/api/v0/en/table/07129", NACE=TRUE,Contents=c("Verdiindeks"),Tid=TRUE)$data[,-2]

df <- data.frame(NACE = dat$NACE, Tid  = paste0(substr(dat$Tid ,1,4), '-', substr(dat$Tid,6,8), '-01'),
                 value= dat$value)

ok_nace <- dplyr::filter(dplyr::summarize(dplyr::group_by(df,NACE),x = sum(value,na.rm=T)),x>0)$NACE
df <- dplyr::filter(df,NACE %in% ok_nace)

vhi <- tidyr::spread(df, NACE, value)
vhi <- vhi[,-which(nchar(colnames(vhi))>5)]


tidsserier <- ts(vhi,freq=12,start=c(2000,1))[,-1]

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

groups_series <- c("45","46",paste0("47.",1:7),"47.9")
groups_now <- lapply(groups_series,function(x){colnames(tidsserier)[which(grepl(x,colnames(tidsserier)))]})
names(groups_now) <- groups_series


to_html <- "/home/onyxia/work/sadashboarddemo/development/my_report.html"

sa_quality_report(mysa,report_file=to_html,title_report = "Testrapport",author_report="S811",group_series=groups_now,
                  linearized=TRUE,ma_filter=TRUE,cal_adjust=TRUE)
