#####################################################
###                                               ###
###    Eksempler fra presentasjon av R-pakker     ###
###    for sesongjustering på Dapla.              ###
###    Dapladagen 2025.                           ###
###                                               ###
#####################################################


## installasjon med renv på Dapla Lab:

#renv::init()

renv::install("statisticsnorway/ssb-pickmdl")
renv::install("statisticsnorway/ssb-sadashboard")

renv::install("DataEditR")


###########
###
### 1. RJDemetra, enkel kjøring
###
###########

library(RJDemetra)

# Eksempeldata fra varehandels-indeksen, ts-objekt.

vhi_ts <- sadashboard::vhi

# Definerer én enkel tidsserie som skal sesongjusteres:

vhi_47 <- vhi_ts[,"47.1"]

# definerer spesifikasjon:

spec_now <- x13_spec(spec="RSA3",
                     transform.function = "Log",
                     tradingdays.option ="TradingDays",
                     outlier.ao = TRUE,
                     outlier.ls = TRUE,
                     outlier.tc = FALSE,
                     easter.enabled = TRUE,
                     easter.duration = 3)

# sesongjusterer med definert spesifikasjon:

sa_47 <- x13(vhi_47,spec=spec_now)

###########
###
### 2. Sesongjustering av flere serier med utgangspunkt i data frame med spesifikasjoner
###
###########

library(sadashboard)
library(pickmdl)

# ts-objekt med alle tidsseriene i NACE 47:

vhi_ts

# definerer data_frame med spesfikasjoner for alle serier i vhi_ts:

spec_file <- make_paramfile(inndat = vhi_ts,
                            spec= "RSA3",
                            transform.function = "Log",
                            tradingdays.option ="TradingDays",
                            outlier.ao = TRUE,
                            outlier.ls = TRUE,
                            outlier.tc = FALSE,
                            corona = TRUE,
                            usrdef.outliersEnabled= TRUE,
                            usrdef.outliersType = c("AO","LS","LS"),
                            usrdef.outliersDate = c("2020-01-01","2020-03-01","2022-04-01"),
                            easter.enabled = TRUE,
                            easter.duration = 3)

# lagrer data frame som .csv et passende sted:

write.csv(spec_file,"spec_file_VHI.csv",row.names = FALSE)


# henter data frame med spesifikasjoner fra fil:
# obs! alle kolonner må være character.

spec_file <- read.csv("spec_file_VHI.csv",sep=",")
spec_file <- as.data.frame(lapply(spec_file,as.character))

# sesongjustererer alle serier i vhi_ts med x13_text_frame :

mysa <- x13_text_frame(spec_file,series= "vhi_ts",pickmdl_method="first_tryautomdl")

###########
####
#### 3. Se og editere spesfikasjoner
####
###########

# se og editere spesifikasjonsfil:

spec_file <- edit_constraints(spec_file)
spec_file <- as.data.frame(lapply(spec_file,as.character))

# legge til parametere:

spec_file <- add_constraint(spec_file,
                            constraint = "identification.end",
                            default = "c(identaar,12)")
spec_file <- as.data.frame(lapply(spec_file,as.character))


#########
###
### 4. Egendefinert kalender med konstruksjon()
###
##########


egen_kalender <- konstruksjon(forste_ar = 2000, siste_ar = 2050,
                              k_td = TRUE, td_type = "TD6",
                              k_grupper = TRUE, monster = c(1, 1, 2, 2, 3, 3))$samle_mnd

egen_kalender <- ts(egen_kalender,freq=12,start=c(2000,1))[,-1]


# lage spesifikasjonsfil hvor egendefinert kalender skal brukes:

spec_file <- make_paramfile(inndat = vhi_ts,
                            spec= "RSA3",
                            transform.function = "Log",
                            outlier.ao = TRUE,
                            outlier.ls = TRUE,
                            outlier.tc = FALSE,
                            corona = TRUE,
                            usrdef.varEnabled = TRUE,
                            usrdef.varType="Calendar",
                            usrdef.var = "egen_kalender")

spec_file <- as.data.frame(lapply(spec_file,as.character))

# sesongjustere alle serier:

mysa <- x13_text_frame(spec_file,series= "vhi_ts",pickmdl_method="first_tryautomdl")



#############
###
### 5.Modellseleksjon, låsing av modell
###
#############

# for enkelt serie:

spec_now <- x13_spec(spec="RSA3",
                     transform.function = "Log",
                     tradingdays.option ="TradingDays")

## finner det siste fulle året i tidsserien.

identyear <- as.integer(last(time(vhi_ts))) - 1

# x13_pickmdl låser ARIMA-modellen med identification_end

sa_47 <- x13_pickmdl(vhi_47,spec = spec_now,corona=TRUE,identification_end = c(identyear,12))

# for flere serier
# identification_end = "c(identyear,12)" kan defineres i spesifikasjons-fila:

spec_file <- make_paramfile(inndat = vhi_ts,
                            spec= "RSA3",
                            transform.function = "Log",
                            tradingdays.option ="TradingDays",
                            corona = TRUE,
                            identification_end = "c(identyear,12)")

identyear <- as.integer(last(time(vhi_ts))) - 1

mysa <- x13_text_frame(spec_file, series="vhi_ts")


#############
###
### 5.Modellseleksjon, seleksjonsprosedyre
###
#############


# pickmdl og automodell:

mysa <- x13_text_frame(spec_file,series="vhi_ts",pickmdl_method="first")            # Pickmodell
mysa <- x13_text_frame(spec_file,series="vhi_ts",automdl.enabled= TRUE)             # Automodell

mysa <- x13_text_frame(spec_file,series="vhi_ts",pickmdl_method="first_tryautomdl") # Anbefalt


#############
###
### 6. Kvalitetsrapport med sadashboard
###
#############


mysa <- x13_text_frame(spec_file,series="vhi_ts",pickmdl_method="first_tryautomdl")

# definerer path og navn på kvalitetsrapporten:
path_now <- paste0(getwd(),"/dapladagen25.html")


# Valgfri: gruppering av serier i rapporten.

groups_names <- c(paste0("47.",1:7),"47.9")
groups_now <- lapply(groups_names,function(x){spec_file$name[which(grepl(x,spec_file$name))]})
groups_series <- c("Main",groups_names)
groups_now <- c(list(c(paste0("47.",1:7),"47.9")),groups_now)
names(groups_now) <- c("Main",groups_names)

# Rapporten genereres med RMarkdown:

sadashboard::sa_quality_report(mysa,report_file = path_now,title="Dapladagen 2025",author = "S811",
                               plot_start = "2018-01-01",cal_adjust = TRUE,linearized=TRUE,
                               group_series = groups_now)

