

dat <- PxWebApiData::ApiData("https://data.ssb.no/api/v0/en/table/07129", NACE=TRUE,Contents=c("Verdiindeks","VolumUjustert"),Tid=TRUE)$data[,-5]

dat_verdi <- dplyr::filter(dat,ContentsCode == "Verdiindeks")
dat_volum <- dplyr::filter(dat,ContentsCode == "VolumUjustert")

df_verdi <- data.frame(NACE = dat_verdi$NACE, Tid  = paste0(substr(dat_verdi$Tid ,1,4), '-', substr(dat_verdi$Tid,6,8), '-01'),
                       value= dat_verdi$value)
df_verdi <- dplyr::mutate(df_verdi,NACE = paste0(paste0("SNN_",NACE),"_IVR"))
df_volum <- data.frame(NACE = dat_volum$NACE, Tid  = paste0(substr(dat_volum$Tid ,1,4), '-', substr(dat_volum$Tid,6,8), '-01'),
                       value= dat_volum$value)
df_volum <- dplyr::mutate(df_volum,NACE = paste0(paste0("SNN_",NACE),"_IVL"))

df <- dplyr::bind_rows(df_verdi,df_volum)

ok_nace <- dplyr::filter(dplyr::summarize(dplyr::group_by(df,NACE),x = sum(value,na.rm=T)),x>0)$NACE
df <- dplyr::filter(df,NACE %in% ok_nace)
#ok_nace <- dplyr::filter(dplyr::summarize(dplyr::group_by(df,NACE),x = sum(value,na.rm=T)),x>0)$NACE
#df <- dplyr::filter(df,NACE %in% ok_nace)


vhi <- tidyr::spread(df, NACE, value)
vhi_ts <- stats::ts(vhi[,-1],freq=12,start=c(2000,1))
