test1 = read.table('~/Downloads/GI_SequencedSamples_9385_Sep13.txt', sep = '\t', header = T)

seqsamples = read_excel("~/Downloads/All_CENTER_FINAL_gVCF_VC_metrics_metadata(2).xlsx", sheet = 6)
seqsamples$`Local ID` = sub("AIIMS/", "AIIM/", seqsamples$`Local ID`)

rajbangshi_swaps = read_excel("~/Downloads/Rajbangshi_ID_map.xlsx")


samples_9772 = read.table('~/Downloads/GI_QCd_sample_ids.txt', sep = '\t', header = F)
samples_9772$LocalID = seqsamples$`Local ID`[match(samples_9772$V1, seqsamples$`Random barcode`)]

seq_9772 = seqsamples[seqsamples$`Random barcode` %in% samples_9772$V1,]


test1$LocalID = seqsamples$`Local ID`[match(test1$SeqID, seqsamples$`Random barcode`)]
# NIBG KarnKayastha -------------------------------------------------------
KarnKayastha = read_xlsx("~/Downloads/Questionare_anthopometry_KARAN_KAYASTHA_bhubaneshwer.xlsx")
KarnKayastha$LocalID = paste0("NIBG/T/1", KarnKayastha$`Individual ID`)
KarnKayastha = KarnKayastha[,-c(1:2)]
KarnKayastha = KarnKayastha[,-c(6:38)]
KarnKayastha = KarnKayastha[,-c(25:36)]
KarnKayastha = KarnKayastha[,-c(204:208)]

KarnKayastha$history_illness.history_illness_family[KarnKayastha$LocalID=="NIBG/T/1H032F"] = "Child - Down Syndrome"
KarnKayastha = KarnKayastha[,-c(176:187)]
KarnKayastha$history_illness.history_illness_family[KarnKayastha$LocalID=="NIBG/T/1H182F"] = "1 Preterm Birth"
KarnKayastha = KarnKayastha[,-c(172:175)]
KarnKayastha = subset(KarnKayastha, select = -RM)
KarnKayastha = subset(KarnKayastha, select = -`Visceral Fat`)
KarnKayastha = subset(KarnKayastha, select = -`Body Age`)
KarnKayastha = subset(KarnKayastha, select = -`SpO2`)
KarnKayastha = subset(KarnKayastha, select = -`Pulse`)
KarnKayastha = subset(KarnKayastha, select = -`specify...47`)
KarnKayastha = KarnKayastha[,-c(13:16)]


KarnKayastha$`Occupation (secondary)`[KarnKayastha$`Occupation (secondary)`=="No"] = NA
KarnKayastha$`Occupation (secondary)`[KarnKayastha$`Occupation (secondary)`=="NA"] = NA

KarnKayastha$occupation <- apply(KarnKayastha[, 17:19], 1, 
                                 function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$occupation[KarnKayastha$occupation==""] = NA
KarnKayastha = KarnKayastha[,-c(16:19)]
names(KarnKayastha)[names(KarnKayastha)=="occupation"] = "socio_demographics.occupation"

KarnKayastha$name_dob_1.state = "orissa"


KarnKayastha$smoking_tobacco_alcohol.smoking_status[KarnKayastha$`Do you smoke any tobacco related products, such as cigarettes or bidis?`=="Yes"] = "Present"
KarnKayastha$smoking_tobacco_alcohol.smoking_status[KarnKayastha$`Did you smoke in the past?`=="Yes"] = "Past"
KarnKayastha$smoking_tobacco_alcohol.smoking_status[is.na(KarnKayastha$smoking_tobacco_alcohol.smoking_status)] = "Never"

KarnKayastha = KarnKayastha[,-c(16:19)]

KarnKayastha$smoking_tobacco_alcohol.chewing_tobacco_status[KarnKayastha$`Have you ever used any smokeless tobacco such as snuff, betelnut with tobacco or`=="Yes"] = "Present"
KarnKayastha$smoking_tobacco_alcohol.chewing_tobacco_status[KarnKayastha$`Did you use smokeless tobacco in the past?`=="Yes"] = "Past"
KarnKayastha$smoking_tobacco_alcohol.chewing_tobacco_status[is.na(KarnKayastha$smoking_tobacco_alcohol.chewing_tobacco_status)] = "Never"

KarnKayastha = KarnKayastha[,-c(16:19)]

KarnKayastha$smoking_tobacco_alcohol.alcohol_status[KarnKayastha$`Have you ever consumed any alcoholic drink?`=="Yes"] = "Present"
KarnKayastha$smoking_tobacco_alcohol.alcohol_status[KarnKayastha$`Did you consume alcohol in the past?`=="Yes"] = "Past"
KarnKayastha$smoking_tobacco_alcohol.alcohol_status[is.na(KarnKayastha$smoking_tobacco_alcohol.alcohol_status)] = "Never"

KarnKayastha = KarnKayastha[,-c(16:30)]

KarnKayastha$name_dob_1.ethnicity = "karn_kayastha"

KarnKayastha = subset(KarnKayastha, select = -Population)
KarnKayastha = subset(KarnKayastha, select = -Village)

KarnKayastha$end <- trimws(sub(" .*", "", KarnKayastha$end))
KarnKayastha= subset(KarnKayastha, select = -start)
names(KarnKayastha)[names(KarnKayastha)=="end"] = "introduction_1.examination_date"


KarnKayastha$history_illness.history_illness_self[KarnKayastha$`Any infertility related complications?`=="Yes"] = "Unknown infertility related complications"
KarnKayastha = KarnKayastha[,-c(126:131)]

KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(150, 120)], 1, 
                                 function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA

KarnKayastha = KarnKayastha[,-c(119:124)]


KarnKayastha$covid[KarnKayastha$`Did you get infected with COVID19?`=="Yes"] = "COVID-19"
KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(144, 145)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA
KarnKayastha = KarnKayastha[,-c(100:109, 145)]

KarnKayastha$history_illness.name_medication[KarnKayastha$`Insulin therapy ever received`=="Present"] = "Insulin therapy"
KarnKayastha = KarnKayastha[,-c(20:22)]
KarnKayastha = KarnKayastha[,-c(15, 17:19, 21, 23:27)]

KarnKayastha$iron[KarnKayastha$`Have you  ever taken Iron supplements?`=="Present"] = "iron supplement"
KarnKayastha$folic[KarnKayastha$`Have you  ever taken Folic acid supplements?`=="Present"] = "folic supplement"

KarnKayastha$history_illness.name_medication <- apply(KarnKayastha[, c(122, 123, 124)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.name_medication[KarnKayastha$history_illness.name_medication==""] = NA

KarnKayastha = subset(KarnKayastha, select = -`What is the reason?...115`)
KarnKayastha = subset(KarnKayastha, select = -`iron`)
KarnKayastha = subset(KarnKayastha, select = -`folic`)

KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(120, 35)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA
KarnKayastha = KarnKayastha[,-c(34:39)]



KarnKayastha$diarrhoea[KarnKayastha$`Have you experienced any recent episodes of diarrheal disease?`=="Yes"] = "diarrhoea"
KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(114, 116)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA
KarnKayastha = KarnKayastha[,-c(65:76, 116)]
KarnKayastha = KarnKayastha[,-c(45:55)]

KarnKayastha$socio_demographics.n_children<- rowSums(
  cbind(
    as.numeric(KarnKayastha$`Number of Male child`), 
    as.numeric(KarnKayastha$`Number of Female Child`)
  ), 
  na.rm = TRUE
)

KarnKayastha = KarnKayastha[,-c(5:6)]

KarnKayastha$history_illness.name_medication <- apply(KarnKayastha[, c(90, 13, 15, 31)], 1, 
                                                      function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.name_medication[KarnKayastha$history_illness.name_medication==""] = NA

KarnKayastha = KarnKayastha[,-c(13, 15, 16, 18, 31)]

KarnKayastha$whitedischarge[KarnKayastha$`Are you suffering from white discharge?`=="Yes"] = "white discharge"

KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(84, 87)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA
KarnKayastha = KarnKayastha[,-c(52:56, 87)]

KarnKayastha$childunderfive <- apply(KarnKayastha[, 6, drop = FALSE], 1, 
                                     function(x) if (!is.na(x)) paste(x, " child died under 5") else NA)
KarnKayastha = KarnKayastha[,-c(5:6)]

KarnKayastha$history_illness.history_illness_family <- apply(KarnKayastha[, c(70, 80)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_family[KarnKayastha$history_illness.history_illness_family==""] = NA
KarnKayastha = KarnKayastha[,-c(80)]

KarnKayastha$diab[KarnKayastha$Diabetes=="Yes"] = "diabetes"
KarnKayastha$hyp[KarnKayastha$Hypertension=="Yes"] = "hypertension"

KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(77, 80, 81)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA
KarnKayastha = KarnKayastha[,-c(10, 11, 80, 81)]

names(KarnKayastha)[names(KarnKayastha)=="History of major illness or surgeries of Father (or cause of death)"] = "history_illness.history_illness_father"
names(KarnKayastha)[names(KarnKayastha)=="History of major illness or surgeries of Mother (or cause of death)"] = "history_illness.history_illness_mother"

KarnKayastha$`What is the reason?...113`[KarnKayastha$`What is the reason?...113`=="Pregnancy"] = NA
KarnKayastha$`What is the reason?...113`[KarnKayastha$`What is the reason?...113`=="Pregnancy ,other medications"] = NA
KarnKayastha$`What is the reason?...113`[KarnKayastha$`What is the reason?...113`=="During pregnancy"] = NA
KarnKayastha$`What is the reason?...113`[KarnKayastha$`What is the reason?...113`=="pregnancy"] = NA

KarnKayastha$`What is the reason?...113`[KarnKayastha$`What is the reason?...113`=="Diabetes"] = NA
KarnKayastha$`What is the reason?...113`[KarnKayastha$`What is the reason?...113`=="Tumor operation"] = NA

KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(75, 10)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA
KarnKayastha = KarnKayastha[,-c(10)]

KarnKayastha$infectious <- apply(KarnKayastha[, c(24, 32)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$infectious[KarnKayastha$infectious==""] = NA
KarnKayastha$infectious = gsub("None of the above, ", "", KarnKayastha$infectious)

KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(74, 77)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA

KarnKayastha = KarnKayastha[,-c(22:32)]
KarnKayastha$`History of major illness or surgeries of other Family member`[KarnKayastha$`History of major illness or surgeries of other Family member`=="NA"] = NA
KarnKayastha$`History of major illness or surgeries of other Family member`[KarnKayastha$`History of major illness or surgeries of other Family member`=="No"] = NA
KarnKayastha$`History of major illness or surgeries of other Family member`[KarnKayastha$`History of major illness or surgeries of other Family member`=="Na"] = NA
KarnKayastha$`History of major illness or surgeries of other Family member`[KarnKayastha$`History of major illness or surgeries of other Family member`=="0"] = NA
KarnKayastha$`History of major illness or surgeries of other Family member`[KarnKayastha$`History of major illness or surgeries of other Family member`=="Yes"] = NA

KarnKayastha$history_illness.history_illness_family <- apply(KarnKayastha[, c(56, 33)], 1, 
                                                             function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_family[KarnKayastha$history_illness.history_illness_family==""] = NA
KarnKayastha = KarnKayastha[,-c(66, 33)]

KarnKayastha$history_illness.history_illness_self[KarnKayastha$LocalID=="NIBG/T/1H134M"] = "TB (treated)"
KarnKayastha$TB_family <- apply(KarnKayastha[, 29, drop = FALSE], 1, 
                                     function(x) if (!is.na(x)) paste(x, " - Tuberculosis") else NA)

KarnKayastha$history_illness.history_illness_family <- apply(KarnKayastha[, c(55, 65)], 1, 
                                                             function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_family[KarnKayastha$history_illness.history_illness_family==""] = NA
KarnKayastha = KarnKayastha[,-c(22:30, 65)]

KarnKayastha$BMI_calculated = as.numeric(KarnKayastha$Weight)/((as.numeric(KarnKayastha$Height)/100)^2)

KarnKayastha = subset(KarnKayastha, select = -BMI)

names(KarnKayastha)[names(KarnKayastha)=="BMI_calculated"] = "BMI"
KarnKayastha = KarnKayastha[,-c(24:27)]


KarnKayastha$socio_demographics.n_pregnancies<- rowSums(
  cbind(
    as.numeric(KarnKayastha$`Vaginal Delivery (numbers)`), 
    as.numeric(KarnKayastha$`C-Section(numbers)`)
  ), 
  na.rm = TRUE
)

KarnKayastha$socio_demographics.n_pregnancies[KarnKayastha$Sex=="Male"] = NA
KarnKayastha$socio_demographics.n_pregnancies[KarnKayastha$LocalID %in% c("NIBG/T/1H016F", "NIBG/T/1H018F", "NIBG/T/1H064F", "NIBG/T/1H083F", "NIBG/T/1H085F",
                                                                          "NIBG/T/1H096F", "NIBG/T/1H098F", "NIBG/T/1H114F", "NIBG/T/1H126F", "NIBG/T/1H212F",
                                                                          "NIBG/T/1H192F", "NIBG/T/1H190F", "NIBG/T/1H162F", "NIBG/T/1H164F", "NIBG/T/1H170F")] = NA

KarnKayastha$miscarriage[KarnKayastha$LocalID %in% c("NIBG/T/1H041F", "NIBG/T/1H048F", "NIBG/T/1H065F", "NIBG/T/1H071F", "NIBG/T/1H078F", "NIBG/T/1H084F", "NIBG/T/1H088F",
                                                     "NIBG/T/1H089F", "NIBG/T/1H090F", "NIBG/T/1H091F", "NIBG/T/1H092F", "NIBG/T/1H209F", "NIBG/T/1H122F", "NIBG/T/1H142F")] = "1 miscarriage"

KarnKayastha = KarnKayastha[,-c(24:30)]

names(KarnKayastha)[names(KarnKayastha)=="Sex"] = "name_dob_1.gender"
names(KarnKayastha)[names(KarnKayastha)=="Family Income Per Month"] = "socio_demographics.income_family"
names(KarnKayastha)[names(KarnKayastha)=="Marital Status"] = "socio_demographics.marital_status"
names(KarnKayastha)[names(KarnKayastha)=="Language Spoken (Mother tongue)"] = "name_dob_1.mother_tongue"
names(KarnKayastha)[names(KarnKayastha)=="Language write"] = "socio_demographics.lang_write"
names(KarnKayastha)[names(KarnKayastha)=="Highest education"] = "socio_demographics.highest_edu"
names(KarnKayastha)[names(KarnKayastha)=="Total Years of Education"] = "socio_demographics.years_edu"
names(KarnKayastha)[names(KarnKayastha)=="Medium of Education"] = "socio_demographics.medium_edu"


names(KarnKayastha)[names(KarnKayastha)=="Height"] = "anthropometry.height"
names(KarnKayastha)[names(KarnKayastha)=="Head Circumference"] = "anthropometry.head_cir"
names(KarnKayastha)[names(KarnKayastha)=="Waist Circumference"] = "anthropometry.waist_cir"
names(KarnKayastha)[names(KarnKayastha)=="Hip Circumference"] = "anthropometry.hip_cir"
names(KarnKayastha)[names(KarnKayastha)=="Weight"] = "anthropometry.weight"
names(KarnKayastha)[names(KarnKayastha)=="Total Body Fat Percentage(%)"] = "anthropometry.body_fat"
names(KarnKayastha)[names(KarnKayastha)=="BP systolic"] = "anthropometry.sys_bp"
names(KarnKayastha)[names(KarnKayastha)=="BP Diastolic"] = "anthropometry.dia_bp"

KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(41, 46)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA

KarnKayastha = KarnKayastha[,-c(46)]

KarnKayastha$chronic = apply(KarnKayastha[, c(12, 21)], 1, 
                             function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$chronic[KarnKayastha$chronic==""] = NA
KarnKayastha$chronic = gsub("others, ", "", KarnKayastha$chronic)

KarnKayastha$history_illness.history_illness_self <- apply(KarnKayastha[, c(41, 46)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
KarnKayastha$history_illness.history_illness_self[KarnKayastha$history_illness.history_illness_self==""] = NA

KarnKayastha = KarnKayastha[,-c(46)]

KarnKayastha = KarnKayastha[,-c(10:21)]
KarnKayastha = KarnKayastha[,-c(33)]
#KarnKayastha = KarnKayastha[,-c(32)]

agecheck = subset(KarnKayastha, select = c("LocalID", "Age"))
agecheck$age_phen = test1$age[match(agecheck$LocalID, test1$LocalID)]

names(KarnKayastha)[names(KarnKayastha) == "Age"] = "age"
KarnKayastha$age_withBBC = KarnKayastha$age
KarnKayastha$center = "NIBG"

KarnKayastha$socio_demographics.lang_speak = KarnKayastha$socio_demographics.lang_write
KarnKayastha$history_illness.medication_currently_status[!is.na(KarnKayastha$history_illness.name_medication)] = "yes"
KarnKayastha$history_illness.medication_currently_status[is.na(KarnKayastha$history_illness.name_medication)] = "no"
#subs = subset(KarnKayastha, select = c("LocalID", "Sex", "socio_demographics.n_children", "socio_demographics.n_pregnancies", "Number of Pregnancies"))

KarnKayastha_seq = KarnKayastha[KarnKayastha$LocalID %in% samples_9772$LocalID,]

idx <- match(test1$LocalID, KarnKayastha_seq$LocalID)

# Loop through the columns present in both test1 and KarnKayastha
columns_to_update <- intersect(names(test1), names(KarnKayastha_seq))

for (col in columns_to_update) {
  # Replace non-NA values from KarnKayastha where LocalID matches
  test1[[col]][!is.na(idx)] <- KarnKayastha_seq[[col]][idx[!is.na(idx)]]
}

#KarnKayastha$geneticsex = seqsamples$`Genetic Sex`[match(KarnKayastha$LocalID, seqsamples$`Local ID`)]
#gendercheck_karn = subset(KarnKayastha, select = c("LocalID", "Sex", "geneticsex"))

# NIBG odk data -----------------------------------------------------------

NIBG_anthro = read_xlsx("~/Downloads/Anthropometry(1).xlsx")
NIBG_odk = read_xlsx("~/Downloads/output_2023-10-31.xlsx")

NIBG_odk_new = merge(NIBG_anthro, NIBG_odk, by = "GI_barcode")
NIBG_seq = NIBG_odk_new[NIBG_odk_new$GI_barcode %in% samples_9772$LocalID,]

Resolved_Sex = read_csv("~/Downloads/Resolved_Sex.csv")
NIBG_seq$Gender <- ifelse(
  NIBG_seq$GI_barcode %in% Resolved_Sex$ID,
  Resolved_Sex$Sex[match(NIBG_seq$GI_barcode, Resolved_Sex$ID)],
  NIBG_seq$Gender  # Retain the original value if no match
)

NIBG_seq$geneticsex = seqsamples$`Ploidy estimation`[match(NIBG_seq$GI_barcode, seqsamples$`Local ID`)]
NIBG_seq$Gender[NIBG_seq$GI_barcode=="NIBG/B/807094"] = "M"


gendercheck = subset(NIBG_seq, select = c("GI_barcode", "Gender", "geneticsex"))
gendercheck$Gender[gendercheck$Gender=="Female"] = "F"
gendercheck$Gender[gendercheck$Gender=="Male"] = "M"

gendercheck$geneticsex[gendercheck$geneticsex=="XX"] = "F"
gendercheck$geneticsex[gendercheck$geneticsex=="XY"] = "M"

#NIBG_BBC = read_xlsx("~/Downloads/NIBMG_biochemistry_july2023_N1330.xlsx")
#gendercheck$BBCGender = NIBG_BBC$Sex[match(gendercheck$GI_barcode, NIBG_BBC$`Sample barcode`)]

gendercheck <- gendercheck[!(gendercheck$Gender == gendercheck$geneticsex), ]
gendercheck = gendercheck[!is.na(gendercheck$GI_barcode),]


NIBG_seq = subset(NIBG_seq, select = -geneticsex)
NIBG_seq = subset(NIBG_seq, select = -Genomics_Lab_ID)
NIBG_seq = subset(NIBG_seq, select = -Collection_Centre)
NIBG_seq = subset(NIBG_seq, select = -`Data present`)
NIBG_seq = subset(NIBG_seq, select = -`Sex`)
NIBG_seq = subset(NIBG_seq, select = -`Population.y`)
NIBG_seq = subset(NIBG_seq, select = -`Visceral Fat`)
NIBG_seq = subset(NIBG_seq, select = -`RM`)
NIBG_seq = subset(NIBG_seq, select = -`BMI`)
NIBG_seq = subset(NIBG_seq, select = -`Body Age`)
NIBG_seq = subset(NIBG_seq, select = -`Pulse`)
NIBG_seq = subset(NIBG_seq, select = -`SpO2`)
NIBG_seq = subset(NIBG_seq, select = -`...1`)
NIBG_seq = subset(NIBG_seq, select = -`Genomics Lab ID`)
NIBG_seq = subset(NIBG_seq, select = -`Collection Center`)

NIBG_seq = subset(NIBG_seq, select = -`Name of the interviewer`)
NIBG_seq = subset(NIBG_seq, select = -`Name of the participant`)
NIBG_seq = subset(NIBG_seq, select = -`Cohort ID (if any)`)
NIBG_seq = subset(NIBG_seq, select = -`Date of data collection`)
NIBG_seq = subset(NIBG_seq, select = -`Date of Birth`)

NIBG_seq = subset(NIBG_seq, select = -`Caste`)
NIBG_seq = subset(NIBG_seq, select = -`Oxygen saturation (SpO2)`)
NIBG_seq = subset(NIBG_seq, select = -`Spirometry`)

NIBG_seq = NIBG_seq[, -c(28:36, 38:43, 47:80, 94:99, 102, 104, 106, 108, 110, 111, 114:122,
                         137:140, 157:170)]

NIBG_seq$Population.x = tolower(NIBG_seq$Population.x)
NIBG_seq$Population.x[NIBG_seq$Population.x=="karn kayastha"] = "karn_kayastha"
NIBG_seq$Population.x[NIBG_seq$Population.x=="kudmi mahato"] = "kudmi_mahato"
NIBG_seq$Population.x[NIBG_seq$Population.x=="rajbangsi"] = "rajbangshi"
NIBG_seq$Population.x[NIBG_seq$Population.x=="rarhi brahmin"] = "rahri_brahmin"
NIBG_seq$Population.x[NIBG_seq$Population.x=="santal"] = "santhal"

NIBG_seq$socio_demographics.occupation <- apply(NIBG_seq[, c(24:27)], 1, 
                                                           function(x) paste(x[!is.na(x)], collapse = ", "))
NIBG_seq$socio_demographics.occupation[NIBG_seq$socio_demographics.occupation==""] = NA
NIBG_seq$socio_demographics.occupation = gsub("Others, ", "", NIBG_seq$socio_demographics.occupation)

NIBG_seq$socio_demographics.occupation[NIBG_seq$socio_demographics.occupation=="HOUSE WIFE" & NIBG_seq$Gender=="Male"] = NA
NIBG_seq$socio_demographics.occupation[NIBG_seq$socio_demographics.occupation=="House Wife" & NIBG_seq$Gender=="Male"] = NA

NIBG_seq$Gender[is.na(NIBG_seq$Gender)] <- seqsamples$`Genetic Sex`[match(NIBG_seq$GI_barcode[is.na(NIBG_seq$Gender)], seqsamples$`Local ID`)]

NIBG_seq = NIBG_seq[, -c(24:27)]

names(NIBG_seq)[names(NIBG_seq)=="Do you smoke any tobacco related products, such as cigarettes or bidis?"] = "smoking_tobacco_alcohol.smoking_status"
names(NIBG_seq)[names(NIBG_seq)=="Have you ever used any smokeless tobacco such as snuff, betelnut with tobacco or chewing tobacco?"] = "smoking_tobacco_alcohol.chewing_tobacco_status"
names(NIBG_seq)[names(NIBG_seq)=="Have you ever used any alcoholic drink?"] = "smoking_tobacco_alcohol.alcohol_status"


NIBG_seq = NIBG_seq[, -c(29:30, 32:33, 35:40)]

NIBG_seq = subset(NIBG_seq, select = -`Neonatal Feeding Practice`)

NIBG_seq$`Are you suffering from white discharge?`[NIBG_seq$Gender=="Male"] = NA
NIBG_seq = subset(NIBG_seq, select = -`Menstrual complication`)
NIBG_seq = NIBG_seq[, -c(71:73)]

NIBG_seq$diabetes[NIBG_seq$`Do you have diabetes?`=="Yes"] = "diabetes"
NIBG_seq$htn[NIBG_seq$`Do you have hypertension?`=="Yes"] = "hypertension"
NIBG_seq$insulin[NIBG_seq$`Have you ever received insulin therapy?`=="Present"] = "insulin therapy"

NIBG_seq = NIBG_seq[, -c(32, 34, 35)]

NIBG_seq$resp[NIBG_seq$`Have you had any respiratory distress?`=="Yes"] = "respiratory distress"
NIBG_seq$pneu[NIBG_seq$`Have you ever had pneumonia?`=="Yes"] = "pneumonia"
NIBG_seq$emp[NIBG_seq$`Have you ever had emphysema (shortness of breath)?`=="Yes"] = "emphysema"
NIBG_seq$asthma[NIBG_seq$`Have you ever been diagnosed by asthma?`=="Yes"] = "asthma"
NIBG_seq = NIBG_seq[, -c(35:41)]

NIBG_seq$iron[NIBG_seq$`Have you ever taken iron supplements?`=="Present"] = "iron supplement"
NIBG_seq$folic[NIBG_seq$`Have you ever taken folic acid supplements?`=="Present"] = "folic acid supplement"
NIBG_seq = NIBG_seq[, -c(36, 38, 40)]

NIBG_seq$tbtreat[NIBG_seq$`Have you ever been treated with medication for TB?`=="Present"] = "tb treatment"


NIBG_seq$history_illness.name_mediation <- apply(NIBG_seq[, c(70, 75, 76, 77, 32, 33, 34, 35)], 1, 
                                                function(x) paste(x[!is.na(x)], collapse = ", "))
NIBG_seq$history_illness.name_mediation[NIBG_seq$history_illness.name_mediation==""] = NA
NIBG_seq = NIBG_seq[, -c(70, 32:35, 75:77)]

NIBG_seq$TB_family <- apply(NIBG_seq[, 50, drop = FALSE], 1, 
                                function(x) if (!is.na(x)) paste(x, " - Tuberculosis") else NA)
NIBG_seq$TB[NIBG_seq$`In the past, have you been told that you have TB?`=="Yes"] = "Tuberculosis"

NIBG_seq = NIBG_seq[, -c(43:51)]
NIBG_seq$whitedischarge[NIBG_seq$`Are you suffering from white discharge?`=="Yes"] = "white discharge"

NIBG_seq$postmenopausal <- apply(NIBG_seq[, 46, drop = FALSE], 1, 
                            function(x) if (!is.na(x)) paste("Post menopausal ", x) else NA)
NIBG_seq = NIBG_seq[, -c(38, 41, 45, 46)]

NIBG_seq = subset(NIBG_seq, select = -`Address`)

NIBG_seq$history_illness.history_illness_self <- apply(NIBG_seq[, c(30:36, 41, 42, 50:55, 58:60)], 1, 
                                                 function(x) paste(x[!is.na(x)], collapse = ", "))
NIBG_seq$history_illness.history_illness_self[NIBG_seq$history_illness.history_illness_self==""] = NA
NIBG_seq$history_illness.history_illness_self = gsub("\n", "", NIBG_seq$history_illness.history_illness_self)
NIBG_seq = NIBG_seq[, -c(30:36, 41, 42, 50:55, 58:60)]

NIBG_seq$socio_demographics.n_children<- rowSums(
  cbind(
    as.numeric(NIBG_seq$`How many male child(s)?`), 
    as.numeric(NIBG_seq$`How many female child(s)`)
  ), 
  na.rm = TRUE
)

NIBG_seq = NIBG_seq[, -c(15, 16)]

NIBG_seq$family_inf <- apply(NIBG_seq[, c(28:29)], 1, 
                                                       function(x) paste(x[!is.na(x)], collapse = ", "))
NIBG_seq$family_inf[NIBG_seq$family_inf==""] = NA

NIBG_seq$family_inf_full <- apply(NIBG_seq[, c(43, 30)], 1, 
                             function(x) paste(x[!is.na(x)], collapse = " - "))
NIBG_seq$family_inf_full[NIBG_seq$family_inf_full==""] = NA
NIBG_seq = NIBG_seq[, -c(28:30, 43)]

NIBG_seq$history_illness.history_illness_family <- apply(NIBG_seq[, c(24, 34, 37, 40)], 1, 
                                                       function(x) paste(x[!is.na(x)], collapse = ", "))
NIBG_seq$history_illness.history_illness_family[NIBG_seq$history_illness.history_illness_family==""] = NA

NIBG_seq$history_illness.history_illness_family[NIBG_seq$history_illness.history_illness_family=="NEIGHBOUR  - Tuberculosis, Tuberculosis - 2 YEAR AGO FOR 3 DAYS.(10 MINITS )"] = NA
NIBG_seq$history_illness.history_illness_family[NIBG_seq$history_illness.history_illness_family=="Cerebral Palsy"] = "Child - Cerebral palsy"
NIBG_seq$history_illness.history_illness_family[NIBG_seq$history_illness.history_illness_family=="Cleft lip & Cleft palate"] = "Child - Cleft lip & Cleft palate"

NIBG_seq = NIBG_seq[, -c(24, 34, 37, 40)]

names(NIBG_seq)[names(NIBG_seq)=="Population.x"] = "name_dob_1.ethnicity"
names(NIBG_seq)[names(NIBG_seq)=="Height"] = "anthropometry.height"
names(NIBG_seq)[names(NIBG_seq)=="Head Circumference"] = "anthropometry.head_cir"
names(NIBG_seq)[names(NIBG_seq)=="Waist Circumference"] = "anthropometry.waist_cir"
names(NIBG_seq)[names(NIBG_seq)=="Hip Circumference"] = "anthropometry.hip_cir"
names(NIBG_seq)[names(NIBG_seq)=="Weight"] = "anthropometry.weight"
names(NIBG_seq)[names(NIBG_seq)=="Total Body Fat Percentage"] = "anthropometry.body_fat"
names(NIBG_seq)[names(NIBG_seq)=="Systol"] = "anthropometry.sys_bp"
names(NIBG_seq)[names(NIBG_seq)=="Diastole"] = "anthropometry.dia_bp"
names(NIBG_seq)[names(NIBG_seq)=="Gender"] = "name_dob_1.gender"
names(NIBG_seq)[names(NIBG_seq)=="Marital Status"] = "socio_demographics.marital_status"
names(NIBG_seq)[names(NIBG_seq)=="Language spoken (mother tongue)"] = "name_dob_1.mother_tongue"
names(NIBG_seq)[names(NIBG_seq)=="Language write"] = "socio_demographics.lang_write"
names(NIBG_seq)[names(NIBG_seq)=="Highest Education"] = "socio_demographics.highest_edu"
names(NIBG_seq)[names(NIBG_seq)=="Total years of Education"] = "socio_demographics.years_edu"
names(NIBG_seq)[names(NIBG_seq)=="Medium of Education"] = "socio_demographics.medium_edu"
names(NIBG_seq)[names(NIBG_seq)=="Family Income per Month"] = "socio_demographics.income_family"
names(NIBG_seq)[names(NIBG_seq)=="History of major illness or surgeries of father"] = "history_illness.history_illness_father"
names(NIBG_seq)[names(NIBG_seq)=="History of major illness or surgeries of mother"] = "history_illness.history_illness_mother"

NIBG_BBC = read_xlsx("~/Downloads/NIBMG_biochemistry_july2023_N1330.xlsx")

agecheck = subset(NIBG_seq, select = c("GI_barcode", "Age.x", "Age.y"))
agecheck$BBCAge = NIBG_BBC$`AGE (Years)`[match(agecheck$GI_barcode, NIBG_BBC$`Sample barcode`)]

agecheck = agecheck[!((agecheck$Age.x==agecheck$Age.y) & (agecheck$Age.x==agecheck$BBCAge)),]
agecheck = agecheck[!is.na(agecheck$GI_barcode),]

NIBG_seq$age = NIBG_BBC$`AGE (Years)`[match(NIBG_seq$GI_barcode, NIBG_BBC$`Sample barcode`)]

NIBG_seq$SampleID <- str_sub(NIBG_seq$GI_barcode, -6)

NIBG_seq$age[NIBG_seq$SampleID %in% c("807060", "807094", "807101", "807118", "807125", "807134", "807142", "807154",
                                      "707084", "707085", "707130", "707134", "707197", "607102", "607171", "607198",
                                      "607240", "607304", "607309", "407035", "407036", "407101", "407181", "307086",
                                      "307121", "307159", "307192")] = NA
names(NIBG_seq)[names(NIBG_seq)=="GI_barcode"] = "LocalID"
NIBG_seq = subset(NIBG_seq, select = -Age.x)
NIBG_seq = subset(NIBG_seq, select = -Age.y)
NIBG_seq = subset(NIBG_seq, select = -SampleID)

NIBG_seq$age_withBBC = NIBG_seq$age
NIBG_seq$socio_demographics.lang_speak = NIBG_seq$name_dob_1.mother_tongue
names(NIBG_seq)[names(NIBG_seq)=="history_illness.name_mediation"] = "history_illness.name_medication"

NIBG_seq$name_dob_1.state = "west_bengal"
NIBG_seq$name_dob_1.state[NIBG_seq$name_dob_1.ethnicity=="karn_kayastha"] = "jharkhand"

NIBG_seq$introduction_1.examination_date = ""
NIBG_seq$name_dob.gps.Latitude = ""
NIBG_seq$name_dob.gps.Longitude = ""
NIBG_seq$name_dob.gps.Altitude = ""
NIBG_seq$name_dob.gps.Accuracy = ""
NIBG_seq$name_dob_1.gps_offline = ""
NIBG_seq$name_dob_1.dob = ""
NIBG_seq$name_dob_1.age_on_interview = ""
NIBG_seq$name_dob_1.approx_age = ""
NIBG_seq$name_dob_1.year_of_birth = ""
NIBG_seq$anthropometry.glucose_mg_dl = ""
NIBG_seq$blood_draw.Blood_draw_fasting = ""

NIBG_seq$center = "NIBG"
NIBG_seq$history_illness.medication_currently_status[!is.na(NIBG_seq$history_illness.name_medication)] = "yes"
NIBG_seq$history_illness.medication_currently_status[is.na(NIBG_seq$history_illness.name_medication)] = "no"

NIBG_seq$BMI = as.numeric(NIBG_seq$anthropometry.weight)/((as.numeric(NIBG_seq$anthropometry.height)/100)^2)
NIBG_seq$region = "East"

NIBG_seq$history_illness.history_illness_self = tolower(NIBG_seq$history_illness.history_illness_self)
NIBG_seq$history_illness.history_illness_self = gsub("pregnancy, pregnancy, ", "pregnancy, ", NIBG_seq$history_illness.history_illness_self)
NIBG_seq$history_illness.history_illness_self[NIBG_seq$history_illness.history_illness_self=="neighbour  - tuberculosis"] = NA
NIBG_seq$history_illness.history_illness_self[NIBG_seq$history_illness.history_illness_self=="appendix 2013., others, appendix - 2013"] = "appendix 2013"

idx <- match(test1$LocalID, NIBG_seq$LocalID)

# Loop through the columns present in both test1 and KarnKayastha
columns_to_update <- intersect(names(test1), names(NIBG_seq))

for (col in columns_to_update) {
  # Replace non-NA values from KarnKayastha where LocalID matches
  test1[[col]][!is.na(idx)] <- NIBG_seq[[col]][idx[!is.na(idx)]]
}


# Ethnicity map -----------------------------------------------------------

mapping = read_xlsx("~/Downloads/Population_9772(2).xlsx")
mapping$Population = tolower(mapping$Population)

seqsamples$ethnicity = mapping$Population[match(seqsamples$Population, mapping$`Code NEW`)]


test1$name_dob_1.ethnicity[test1$name_dob_1.ethnicity=="koli (tribe)"] = "kol"
test1$ethnicity_mapping = mapping$`Code NEW`[match(test1$name_dob_1.ethnicity, mapping$Population)]

test1$mainregion = mapping$`Primary Residence`[match(test1$name_dob_1.ethnicity, mapping$Population)]

# Removing discordants ----------------------------------------------------

discordants = read_xlsx("~/Downloads/Confirmed_Discordants.xlsx")

discordant_phenotype = test1[test1$LocalID %in% discordants$LocalID,]

test1 = test1[!(test1$LocalID %in% discordants$LocalID),]
test1 = test1[test1$LocalID != "NIBG/R/107104",]

test1$SeqID = seqsamples$`Random barcode`[match(test1$LocalID, seqsamples$`Local ID`)]
# Thresholds --------------------------------------------------------------

test1$name_dob_1.dob[nchar(test1$name_dob_1.dob)<10] = NA
test1$name_dob_1.gps_offline[grepl("/", test1$name_dob_1.gps_offline)] = NA
test1$name_dob_1.gps_offline[test1$name_dob_1.gps_offline=="Na"] = NA
test1$name_dob_1.gps_offline[test1$name_dob_1.gps_offline=="Nil"] = NA
test1$name_dob_1.gps_offline[test1$name_dob_1.gps_offline=="Sandeep pal"] = NA
test1$name_dob_1.gps_offline[test1$name_dob_1.gps_offline=="SUNIL RAGHAV"] = NA

test1$name_dob_1.approx_age = as.numeric(test1$name_dob_1.approx_age)
test1$anthropometry.sys_bp = as.numeric(test1$anthropometry.sys_bp)
test1$anthropometry.dia_bp = as.numeric(test1$anthropometry.dia_bp)
test1$anthropometry.head_cir = as.numeric(test1$anthropometry.head_cir)
test1$anthropometry.height = as.numeric(test1$anthropometry.height)
test1$anthropometry.weight = as.numeric(test1$anthropometry.weight)
test1$anthropometry.waist_cir = as.numeric(test1$anthropometry.waist_cir)
test1$anthropometry.hip_cir = as.numeric(test1$anthropometry.hip_cir)
test1$anthropometry.body_fat = as.numeric(test1$anthropometry.body_fat)
test1$anthropometry.glucose_mg_dl = as.numeric(test1$anthropometry.glucose_mg_dl)

test1$introduction_1.examination_date[test1$introduction_1.examination_date==""] = NA
test1$name_dob.gps.Latitude[test1$name_dob.gps.Latitude == ""] = NA
test1$name_dob.gps.Longitude[test1$name_dob.gps.Longitude == ""] = NA
test1$name_dob.gps.Altitude[test1$name_dob.gps.Altitude == ""] = NA
test1$name_dob.gps.Accuracy[test1$name_dob.gps.Accuracy == ""] = NA
test1$name_dob_1.gps_offline[test1$name_dob_1.gps_offline == ""] = NA
test1$name_dob_1.dob[test1$name_dob_1.dob == ""] = NA
test1$name_dob_1.age_on_interview[test1$name_dob_1.age_on_interview == ""] = NA
test1$name_dob_1.approx_age[test1$name_dob_1.approx_age == ""] = NA
test1$name_dob_1.year_of_birth[test1$name_dob_1.year_of_birth == ""] = NA
test1$anthropometry.glucose_mg_dl[test1$anthropometry.glucose_mg_dl == ""] = NA
test1$blood_draw.Blood_draw_fasting[test1$blood_draw.Blood_draw_fasting == ""] = NA


test1$RBS[which(test1$RBS == 0)] <- NA
test1$FBS_Fasting_Blood_Glucose[which(test1$FBS_Fasting_Blood_Glucose == 0)] <- NA
test1$FBS_Fasting_Blood_Glucose[which(test1$FBS_Fasting_Blood_Glucose < 20)] <- NA
test1$HbA1C_Glycosylated_Haemoglobin[test1$HbA1C_Glycosylated_Haemoglobin<0] = NA
test1$name_dob_1.age_on_interview[which(test1$name_dob_1.age_on_interview<18)] = NA
#test1$Estimated_Glomerular_filtration_rate[which(test1$Estimated_Glomerular_filtration_rate==0)] = NA
test1$Creatinine[test1$Creatinine==0] = NA
test1$Total_Bilirubin[test1$Total_Bilirubin<0] = NA
test1$Total_Bilirubin[test1$Total_Bilirubin>10] = NA
test1$Direct_Bilirubin[test1$Direct_Bilirubin>100] = NA
test1$Albumin[which(test1$Albumin>70)] = NA
test1$ALT_SGPT[which(as.numeric(test1$ALT_SGPT)>5000)] = NA
test1$Alkaline_Phosphatase[which(as.numeric(test1$Alkaline_Phosphatase)>10000)] = NA
test1$anthropometry.body_fat[which(test1$anthropometry.body_fat==0)] = NA
test1$anthropometry.body_fat[which(test1$anthropometry.body_fat>100)] = NA
test1$anthropometry.dia_bp[which(as.numeric(test1$anthropometry.dia_bp)>500)] = NA
test1$anthropometry.glucose_mg_dl[which((test1$anthropometry.glucose_mg_dl==0))] = NA
test1$anthropometry.head_cir[which(test1$anthropometry.head_cir>100)] = NA
test1$anthropometry.head_cir[which(test1$anthropometry.head_cir==0)] = NA
test1$anthropometry.height[which(as.numeric(test1$anthropometry.height)>500)] = NA
test1$anthropometry.sys_bp[which(as.numeric(test1$anthropometry.sys_bp)>500)] = NA
test1$anthropometry.waist_cir[which(as.numeric(test1$anthropometry.waist_cir)>300)] = NA
test1$anthropometry.weight[which(as.numeric(test1$anthropometry.weight)==0)] = NA
test1$anthropometry.weight[which(as.numeric(test1$anthropometry.weight)>300)] = NA

test1$anthropometry.hip_cir[as.numeric(test1$anthropometry.hip_cir)>400] = NA

test1$Basophils[which(test1$center=='GBRC' & is.na(test1$Basophils))] = 0
test1$Basophils[which(as.numeric(test1$Basophils)>10)] = NA
test1$Cholesterol[which(as.numeric(test1$Cholesterol)>1000)] = NA
test1$Creatinine[which(test1$Creatinine>=10)] = NA
test1$Alkaline_Phosphatase[test1$Alkaline_Phosphatase==0]=NA
test1$Cholesterol[test1$Cholesterol==0]=NA
test1$Eosinophils[which(as.numeric(test1$Eosinophils)>100)] = NA
test1$Eosinophils[which(test1$Eosinophils==0)]= NA
test1$HB_Haemoglobin[which(test1$HB_Haemoglobin>25)]=NA
test1$HB_Haemoglobin[which(test1$HB_Haemoglobin<2)]=NA
test1$HbA1C_Glycosylated_Haemoglobin[which(test1$HbA1C_Glycosylated_Haemoglobin>50)] = NA
test1$HbA1C_Glycosylated_Haemoglobin[which(test1$HbA1C_Glycosylated_Haemoglobin==0)] = NA
test1$HDL[which(test1$HDL>500)] = NA
test1$Platelet_Count[which(test1$Platelet_Count < 10000)] = NA
test1$Platelet_Count[which(test1$Platelet_Count>1000000)]=NA
test1$LDL[which(test1$LDL==0)] = NA
test1$LDL[which(test1$LDL<0)] = NA
test1$Lymphocytes[which(test1$Lymphocytes==0)] = NA
test1$MCH_Mean_Corpuscular_Hb[which(test1$MCH_Mean_Corpuscular_Hb > 5000)] = NA
test1$Monocytes[which(test1$Monocytes==0)] = NA
test1$Neutrophils[which(test1$Neutrophils>100)] = NA
test1$Protein[which(test1$Protein>30)] = NA
test1$RBC_Red_Blood_Cell_Count[test1$RBC_Red_Blood_Cell_Count>20] = NA
test1$RBC_Red_Blood_Cell_Count[test1$RBC_Red_Blood_Cell_Count==0] = NA
test1$RBS[which(test1$RBS==0)] = NA
test1$Total_Bilirubin[which(test1$Total_Bilirubin>100)] = NA
test1$Total_Bilirubin[which(test1$Total_Bilirubin == 0)] = NA
test1$Urea[which(test1$Urea==0)] = NA
test1$WBC_Total_White_Blood_Cell_Count[which(test1$WBC_Total_White_Blood_Cell_Count>25000)]=NA
test1$WBC_Total_White_Blood_Cell_Count[which(test1$WBC_Total_White_Blood_Cell_Count<1000)]=NA
test1$Lymphocytes[test1$Lymphocytes==0] = NA
test1$Lymphocytes[test1$Lymphocytes>100] = NA
test1$MCH_Mean_Corpuscular_Hb[test1$MCH_Mean_Corpuscular_Hb>150] = NA
test1$socio_demographics.n_children[test1$socio_demographics.n_children>30] = NA
test1$socio_demographics.years_edu[test1$socio_demographics.years_edu>20] = NA
test1$anthropometry.hip_cir[test1$anthropometry.hip_cir>500] = NA
test1$ALT_SGPT[test1$ALT_SGPT>500] = NA
test1$LDL[test1$LDL>1000] = NA
test1$age[test1$age<18] = NA
test1$age_withBBC[test1$age_withBBC<18] = NA

test1$HbA1C_Glycosylated_Haemoglobin[test1$HbA1C_Glycosylated_Haemoglobin==0] = NA
test1$Lymphocytes[test1$Lymphocytes==0] = NA
test1$AST_SGOT[test1$AST_SGOT>500] = NA
test1$history_illness.medication_currently_status[test1$history_illness.medication_currently_status=="NA"] = NA
test1$history_illness.medication_currently_status = tolower(test1$history_illness.medication_currently_status)

test1$socio_demographics.marital_status[test1$socio_demographics.marital_status=="currently_currently_married"] = "currently_married"
test1$socio_demographics.marital_status[test1$socio_demographics.marital_status=="Married"] = "currently_married"
test1$socio_demographics.marital_status[test1$socio_demographics.marital_status=="MARRIED"] = "currently_married"
test1$socio_demographics.marital_status[test1$socio_demographics.marital_status=="Unmarried"] = "never_married"
test1$socio_demographics.marital_status[test1$socio_demographics.marital_status=="Un-married"] = "never_married"
test1$socio_demographics.marital_status[test1$socio_demographics.marital_status=="Widowed"] = "widowed"
test1$socio_demographics.marital_status[test1$socio_demographics.marital_status=="Divorced"] = "divorced_separated"
test1$Indirect_Bilirubin[test1$Indirect_Bilirubin<=0] = NA
test1$Indirect_Bilirubin[test1$Indirect_Bilirubin >10] = NA

test1$smoking_tobacco_alcohol.alcohol_status = tolower(test1$smoking_tobacco_alcohol.alcohol_status)
test1$smoking_tobacco_alcohol.smoking_status = tolower(test1$smoking_tobacco_alcohol.smoking_status)
test1$smoking_tobacco_alcohol.chewing_tobacco_status = tolower(test1$smoking_tobacco_alcohol.chewing_tobacco_status)
test1$history_illness.history_illness_self = tolower(test1$history_illness.history_illness_self)
test1$history_illness.history_illness_family = tolower(test1$history_illness.history_illness_family)
test1$history_illness.history_illness_father = tolower(test1$history_illness.history_illness_father)
test1$history_illness.history_illness_mother = tolower(test1$history_illness.history_illness_mother)

test1$BMI[test1$BMI>100] = NA
test1$BMI[test1$BMI<5] = NA

test1$name_dob_1.gender = tolower(test1$name_dob_1.gender)
test1$name_dob_1.gender[test1$name_dob_1.gender=="M"] = "male"

test1$name_dob_1.mother_tongue = tolower(test1$name_dob_1.mother_tongue)
test1$socio_demographics.lang_speak = tolower(test1$socio_demographics.lang_speak)
test1$socio_demographics.lang_write = tolower(test1$socio_demographics.lang_write)
test1$socio_demographics.highest_edu = tolower(test1$socio_demographics.highest_edu)
test1$socio_demographics.medium_edu = tolower(test1$socio_demographics.medium_edu)
test1$socio_demographics.occupation = tolower(test1$socio_demographics.occupation)
test1$history_illness.name_medication = tolower(test1$history_illness.name_medication)
test1$blood_draw.Blood_draw_fasting = tolower(test1$blood_draw.Blood_draw_fasting)

test1$smoking_tobacco_alcohol.alcohol_status[test1$smoking_tobacco_alcohol.alcohol_status=="yes"] = "present"
test1$smoking_tobacco_alcohol.alcohol_status[test1$smoking_tobacco_alcohol.smoking_status=="yes"] = "present"
test1$smoking_tobacco_alcohol.alcohol_status[test1$smoking_tobacco_alcohol.chewing_tobacco_status=="yes"] = "present"

test1$smoking_tobacco_alcohol.alcohol_status[test1$smoking_tobacco_alcohol.alcohol_status=="present"] = "current"
test1$smoking_tobacco_alcohol.alcohol_status[test1$smoking_tobacco_alcohol.smoking_status=="present"] = "current"
test1$smoking_tobacco_alcohol.alcohol_status[test1$smoking_tobacco_alcohol.chewing_tobacco_status=="present"] = "current"


test1$biogeographic_region = mapping$Physiography[match(test1$name_dob_1.ethnicity, mapping$Population)]
test1$population_size = mapping$Demography[match(test1$name_dob_1.ethnicity, mapping$Population)]

test1$biogeographic_region[test1$biogeographic_region=="Brhamaputra Valley"] = "Brahmaputra Valley"
test1$biogeographic_region[test1$biogeographic_region=="Eastern Pleatue"] = "Eastern Plateau"

test1$biogeographic_region = tolower(test1$biogeographic_region)

test1$geneticsex = seqsamples$`Ploidy estimation`[match(test1$SeqID, seqsamples$`Random barcode`)]
test1$geneticsex[test1$geneticsex=="XX"] = "female"
test1$geneticsex[test1$geneticsex=="XY"] = "male"

test1$name_dob_1.gender[test1$LocalID=="NIBG/B/807094"] = "male" #identified and corrected later
test1$mainregion = gsub(" India", "", test1$mainregion)


gwasdata = read_xlsx("~/Downloads/GWAS and WGS updates _to be refered (1).xlsx")
gwasdata_IISR = read_xlsx("~/Downloads/GWAS and WGS updates _to be refered (1).xlsx", sheet = 2)
gwasdata_North = read_xlsx("~/Downloads/GWAS and WGS updates _to be refered (1).xlsx", sheet = 3)
gwasdata_RGCB = read_xlsx("~/Downloads/GWAS and WGS updates _to be refered (1).xlsx", sheet = 4)
gwasdata_NIMH = read_xlsx("~/Downloads/GWAS and WGS updates _to be refered (1).xlsx", sheet = 5)
gwasdata_North2 = read_xlsx("~/Downloads/GWAS and WGS updates _to be refered (1).xlsx", sheet = 6)
gwasdata_IGIB = read_xlsx("~/Downloads/GWAS and WGS updates _to be refered (1).xlsx", sheet = 7)


test1$gwasage = gwasdata$Age[match(test1$SeqID, gwasdata$`Random IDs`)]
test1$gwasage[is.na(test1$gwasage)] = gwasdata_IISR$Age[match(test1$SeqID[is.na(test1$gwasage)], gwasdata_IISR$`Random ID`)]
test1$gwasage[is.na(test1$gwasage)] = gwasdata_North$Age[match(test1$SeqID[is.na(test1$gwasage)], gwasdata_North$`Random IDs`)]
test1$gwasage[is.na(test1$gwasage)] = gwasdata_RGCB$Age[match(test1$SeqID[is.na(test1$gwasage)], gwasdata_RGCB$`Random ID`)]
test1$gwasage[is.na(test1$gwasage)] = gwasdata_NIMH$Age[match(test1$SeqID[is.na(test1$gwasage)], gwasdata_NIMH$`Random ID`)]
test1$gwasage[is.na(test1$gwasage)] = gwasdata_North2$Age[match(test1$SeqID[is.na(test1$gwasage)], gwasdata_North2$`Random IDs`)]
test1$gwasage[is.na(test1$gwasage)] = gwasdata_IGIB$Age[match(test1$SeqID[is.na(test1$gwasage)], gwasdata_IGIB$`Random IDs`)]

check_age = subset(test1, select = c("SeqID", "gwasage", "age"))
check_age = check_age[!is.na(check_age$gwasage),]
check_age$gwasage = as.numeric(check_age$gwasage)
check_age$age = as.numeric(check_age$age)
check_age = check_age[check_age$gwasage != check_age$age,]
check_age = check_age[!is.na(check_age$SeqID),]
check_age = check_age[abs(check_age$gwasage - check_age$age)>1,]
check_age = check_age[abs(check_age$gwasage - check_age$age)>2,]
check_age = check_age[abs(check_age$gwasage - check_age$age)>4,]

check_age$LocalID = samples_9772$LocalID[match(check_age$SeqID, samples_9772$V1)]
check_age$ethnicity = test1$name_dob_1.ethnicity[match(check_age$SeqID, test1$SeqID)]

check_age$BBCAge[check_age$LocalID=="CBRI/B/050035"] = 66
check_age$BBCAge[check_age$LocalID=="CBRI/B/070159"] = 30
check_age$BBCAge[check_age$LocalID=="CBRI/B/075015"] = 65
check_age$BBCAge[check_age$LocalID=="CBRI/B/085055"] = 47
check_age$BBCAge[check_age$LocalID=="CBRI/B/085068"] = 52
check_age$BBCAge[check_age$LocalID=="CBRI/B/085069"] = 44
check_age$BBCAge[check_age$LocalID=="CBRI/B/085146"] = 69

check_age$DOB = test1$name_dob_1.dob[match(check_age$SeqID, test1$SeqID)]

test1$gwasgender = gwasdata$Gender[match(test1$SeqID, gwasdata$`Random IDs`)]
test1$gwasgender[is.na(test1$gwasgender)] = gwasdata_IISR$Genter[match(test1$SeqID[is.na(test1$gwasgender)], gwasdata_IISR$`Random ID`)]


test1$name_dob_1.dob[test1$SeqID=="RY38374147HI"] = NA
test1$name_dob_1.dob[test1$SeqID=="QH49968517WH"] = NA

test1$history_illness.history_illness_self[test1$LocalID=="IGIB/F/011063"] = "tubectomy"

test1$socio_demographics.occupation[test1$LocalID=="IGIB/F/003128"] = "housework"
test1$socio_demographics.occupation[test1$LocalID=="IGIB/F/007152"] = "housework"
#write.table(test1, "~/Downloads/GI_SequencedSamples_9331_Dec2.txt", sep = '\t', row.names = F)
write.table(test1, "~/Downloads/GI_SequencedSamples_9331_Dec2_freeze.txt", sep = '~', row.names = F)

toshare = test1
toshare = subset(toshare, select = -LocalID)
toshare = subset(toshare, select = -name_dob_1.ethnicity)
toshare = subset(toshare, select = -region)
toshare = subset(toshare, select = -geneticsex)

write.table(toshare, "~/Downloads/GI_SequencedSamples_9331_Dec2_freeze_coded.txt", sep = '~', row.names = F)


merge_num_new = dplyr::select_if(toshare, is.numeric)
pdf("~/Downloads/distbycenter_dec2.pdf", width = 11)
variables = colnames(merge_num_new)
for(V in variables){
  if(length(unique(as.numeric(merge_num_new[,V]))) == 1){next;}
  boxplot(as.numeric(merge_num_new[,V]) ~ toshare$center, main = V, ylab = V, xlab = "Centre")
}

dev.off()

# plotting boxplots against ethnicity -------------------------------------
pdf("~/Downloads/distbypopulation_dec2.pdf", width = 11)
variables = colnames(merge_num_new)
for(V in variables){
  if(length(unique(as.numeric(merge_num_new[,V]))) == 1){next;}
  boxplot(as.numeric(merge_num_new[,V]) ~ toshare$ethnicity_mapping, main = V, las = 2, cex.axis = 0.3, ylab = V, xlab = "Population Code")
}

dev.off()

pdf("~/Downloads/missingness_variables_bycenter_Dec2.pdf", width = 10, height = 20)
library(naniar)
gg_miss_fct(x = toshare, fct = center)+scale_fill_gradientn(colors = c("navy","gold", "white"))
dev.off()


