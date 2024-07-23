#load packages
library(dplyr)
library(tidyr)
library(readxl)
library(GGally)
library(ggplot2)
library(ggprism)
#library(ggsci)
library(wesanderson)
library(misty)
library(anytime)
library(lubridate)
library(xlsx)
library(here)
library(tidyverse)
library(rlang)
library(naniar)
library(janitor)
library(parsedate)
options(scipen = 999)
options(max.print = 100000)

# Read in big merged file set --------
ft = read.table("/media/debasrija/New Volume/GenomeIndia/January2024/merged_allsamples_Mar18_2024.txt", 
                sep = "\t", header = TRUE, fill = TRUE, encoding = 'UTF-8')
ft1 = read.table("/media/debasrija/New Volume/GenomeIndia/January2024/merged_allsamples_Mar18_2024.txt", 
                 sep = "\t", header = TRUE, fill = TRUE, encoding = 'latin1') #keep original copy
ft = ft[, -1]
ft$center = gsub("\\/.*","", ft$LocalID)
ft$center[ft$center %in% c("AI72957836GF", "AT96599369CY", "CO02368049ET", "CR98814111XY",
                           "DA19237806ZR", "DV04839813AV", "ER29456934NP", "EU34873313VG",
                           "FD81381443FQ", "FG75912079ZQ", "GJ06061467VI", "IS56252073XV",
                           "IU85779149YX", "JB00665043PS", "KK49874306MX", "NB31406995DF",
                           "OI47060539CY", "OR82172584HH", "PR40044209PS", "PS41041471QI",
                           "PY84122778HR", "TL31698084CL", "UW61566793JE", "VH64473270CW",
                           "VL03952892JB", "WF20055574NE", "YP78626598GN")] = NA

# Remove SKIMS data -------------------------------------------------------

ft <- subset(ft, center != "SKIM")
ft = subset(ft, select = -center.x)
ft = subset(ft, select = -center.y)

# Remove empty columns ----------------------------------------------------

emptycolumns = colSums(is.na(ft)|ft=="")==nrow(ft)
ft = ft[ ,!emptycolumns]

# Remove unnecessary columns ----------------------------------------------

ft = subset(ft, select = -CENTRE_FOR_BRAIN_RESEARCH_IISC_PACKAGE)
ft = subset(ft, select = -NAMES)
ft = subset(ft, select = -Names_._Matching)
ft = subset(ft, select = -Collection_Center)
#ft = subset(ft, select = -NON.HDL_CHOLESTEROL)
#ft = subset(ft, select = -Calcium)
#ft = subset(ft, select = -BUN.Creatinine_Ratio)
ft = ft[,!names(ft) %in% c('checklists.start', 'checklists.end', 'checklists.today',
                           'checklists.deviceid','checklists.subscriberid','checklists.simserial', 
                           'checklists.phonenumber', 'checklists.intro', 
                           'checklists.checklist_before_socio', 'check_n_ckecklist.incomplete_checklist',
                           'consent.consent_genetics','consent.consent_bloodbiochem_antrophometry',
                           'consent.consent_long_term_storage','consent.consent_recontact',
                           'comments_signature.sign_interviewer','meta.instanceID', 'Names_-_Matching',
                           "blood_draw.complications_blood_draw",
                           "blood_draw.n_edta_2ml","blood_draw.n_edta_2ml_dna","blood_draw.n_fluoride_2ml",
                           "blood_draw.n_serum_5ml","Name_of_Examiner", "SL_NO","Serum_Tube_5ml", "Sample_ID",
                           "REMARKS_3","Fasting.Non.Fasting","COMPLETE_HAEMOGRAM","introduction_1.institute_name",
                           "introduction_1.examiner_name","Consent_for_Biochemistry",
                           "Consent_for_DNA","Consent_for_long_term_Storage","Consent_for_Recontact","LIPID_PROFILE__Serum_", 
                           "LIVER_FUNCTION_TEST","introduction_1.local_id_manual", "introduction.local_id_barcode",
                           "EDTA_Tube_2ml", "EDTA_for_DNA_Isolation",'comments_signature.comments', 
                           'CENTRE_FOR_BRAIN_RESEARCH_IISC_PACKAGE','Factors',
                           'Fasting._No_Fasting','LAB', 'COMMUNITY', 'DATE', 'POPULATION','Unique_Barcode_ID','Community',
                           "socio_demographics.n_pregnancy", "socio_demographics.n_abortions", "socio_demographics.n_still_births",
                           "socio_demographics.literacy_status","socio_demographics.work_conditions", "name_dob.place_birth")]


ft = ft[,!names(ft) %in% c('Alcohol_Status','City.Village','DIFFERENTIAL_COUNT','Ethnicity',
                           'Fluorde_Tube_2ml', 'Income', 'Marital_Status','history_illness.pedigree','Medication',
                           'Medium_of_Education','Mother_Tongue','Qualification', 'Same_Caste', 'Smoking_Status',
                           'State','State.UT','Tabacco_Status','Population','Collection_Center',"socio_demographics.n_family_member",
                           "socio_demographics.n_family_member_18")]

ft = subset(ft, select = -CENTER)
ft = subset(ft, select = -name_dob_1.caste)
ft = subset(ft, select = -LIPID_PROFILE_Serum)
ft = subset(ft, select = -...12)
ft = subset(ft, select = -...5)
ft = subset(ft, select = -Others)
#ft = subset(ft, select = -Laungages_to_Speak)
ft = subset(ft, select = -PARASITE)
ft = subset(ft, select = -Height)
ft = subset(ft, select = -Weight)
ft = subset(ft, select = -Gender)
ft = subset(ft, select = -Differential_Cell_Count)
ft = subset(ft, select = -DC14)
ft = subset(ft, select = -X.1)


# removing ILSB blood counts (will be replaced) ---------------------------

ft = subset(ft, select = -BASO...18)
ft = subset(ft, select = -BASO...23)
ft = subset(ft, select = -EO...17)
ft = subset(ft, select = -EO...22)
ft = subset(ft, select = -LYMPH...15)
ft = subset(ft, select = -LYMPH...20)
ft = subset(ft, select = -MONO...16)
ft = subset(ft, select = -MONO...21)
ft = subset(ft, select = -NEUT...14)
ft = subset(ft, select = -NEUT...19)
ft = subset(ft, select = -NRBC)
ft = subset(ft, select = -NRBC.)
ft = subset(ft, select = -NRBC..1)


# removing more unnecessary columns ---------------------------------------

ft = ft[,!names(ft) %in% c("Height", "Weight", "MalariaParasiteexaminationonthinSmear", "MalariaParasiteexaminationonthickSmear",
                           "No._of_Children", "Years_of_Education", "Occupation", "Medical_History", "Illness_Father",
                           "Illness_Mother", "Illness_Other", "Name_of_Medication", "Head_Cm", "Height_Cm", "Weight_Kg",
                           "Waist_Cm", "Hip_Cm", "DOB", "Barcode", "BP_Systolic", "BP_Diastolic","Languages_to_speak",
                           "Laungages_to_write")]

# converting to numeric ---------------------------------------------------

ft$FBS_Fasting_Blood_Glucose = as.numeric(ft$FBS_Fasting_Blood_Glucose)
ft$HbA1C_Glycosylated_Haemoglobin = as.numeric(ft$HbA1C_Glycosylated_Haemoglobin)
#ft$HbA1C_Glycosylated_Haemoglobin[ft$HbA1C_Glycosylated_Haemoglobin == 0] = NA
#merged_df_ft1 = read.table("Added_addnlt_Apr19.txt", sep = "\t", header = T, fill= T, encoding = 'latin1')
ft$Urea = as.numeric(ft$Urea)
ft$Creatinine = as.numeric(ft$Creatinine)
ft$Total_Bilirubin = as.numeric(ft$Total_Bilirubin)
ft$ALT_SGPT = as.numeric(ft$ALT_SGPT)
ft$AST_SGOT = as.numeric(ft$AST_SGOT)
ft$Alkaline_Phosphatase = as.numeric(ft$Alkaline_Phosphatase)
ft$Cholesterol = as.numeric(ft$Cholesterol)
ft$Triglycerides = as.numeric(ft$Triglycerides)
ft$HDL = as.numeric(ft$HDL)
ft$LDL = as.numeric(ft$LDL)
ft$T3_Total = as.numeric(ft$T3_Total)
ft$T4_Total = as.numeric(ft$T4_Total)
ft$TSH = as.numeric(ft$TSH)
ft$Homocysteine_Levels = as.numeric(ft$Homocysteine_Levels)
ft$Vitamin_B12 = as.numeric(ft$Vitamin_B12)
ft$Folic_Acid = as.numeric(ft$Folic_Acid)
ft$Fasting_Insulin_Level = as.numeric(ft$Fasting_Insulin_Level)
ft$C.Peptide = as.numeric(ft$C.Peptide)
ft$HB_Haemoglobin = as.numeric(ft$HB_Haemoglobin)
ft$RBC_Red_Blood_Cell_Count = as.numeric(ft$RBC_Red_Blood_Cell_Count) #this is probably in millions
ft$MCH_Mean_Corpuscular_Hb = as.numeric(ft$MCH_Mean_Corpuscular_Hb)
ft$MCHC_Mean_Corpuscular_Hb_Concn = as.numeric(ft$MCHC_Mean_Corpuscular_Hb_Concn)
ft$MCV_Mean_Corpuscular_Volume = as.numeric(ft$MCV_Mean_Corpuscular_Volume)
ft$RDW_Red_Cell_Distribution_Width = as.numeric(ft$RDW_Red_Cell_Distribution_Width)
ft$WBC_Total_White_Blood_Cell_Count = as.numeric(ft$WBC_Total_White_Blood_Cell_Count)
ft$Basophils = as.numeric(ft$Basophils)
ft$Eosinophils = as.numeric(ft$Eosinophils)
ft$Lymphocytes = as.numeric(ft$Lymphocytes)
ft$Monocytes = as.numeric(ft$Monocytes)
ft$Neutrophils = as.numeric(ft$Neutrophils)
ft$Platelet_Count = as.numeric(ft$Platelet_Count)
ft$A_G_Ratio_Albumin_Globulin = as.numeric(ft$A_G_Ratio_Albumin_Globulin)
ft$Absolute_Neutrophil_Count = as.numeric(ft$Absolute_Neutrophil_Count)
ft$Absolute_Basophil_Count = as.numeric(ft$Absolute_Basophil_Count)
ft$Absolute_Eosinophil_Count = as.numeric(ft$Absolute_Eosinophil_Count)
ft$Absolute_Lymphocyte_Count = as.numeric(ft$Absolute_Lymphocyte_Count)
ft$Absolute_Monocyte_Count = as.numeric(ft$Absolute_Monocyte_Count)
ft$CHOL_HDL_Ratio = as.numeric(ft$CHOL_HDL_Ratio)
ft$Direct_Bilirubin = as.numeric(ft$Direct_Bilirubin)
ft$ESR = as.numeric(ft$ESR)
ft$Estimated_Average_Glucose = as.numeric(ft$Estimated_Average_Glucose)
ft$Gamma_GT_GGTP = as.numeric(ft$Gamma_GT_GGTP)
ft$Globulin = as.numeric(ft$Globulin)
ft$Indirect_Bilirubin = as.numeric(ft$Indirect_Bilirubin)
ft$LDL_HDL_Ratio = as.numeric(ft$LDL_HDL_Ratio)
ft$Hematocrit = as.numeric(ft$Hematocrit)
ft$Albumin = as.numeric(ft$Albumin)
ft$Protein = as.numeric(ft$Protein)
ft$VLDL = as.numeric(ft$VLDL)
ft$RDW_SD = as.numeric(ft$RDW_SD)
ft$CRP = as.numeric(ft$CRP)
ft$MPV_Mean_Platelet_Volume = as.numeric(ft$MPV_Mean_Platelet_Volume)
ft$Immature_granulocyte_perc = as.numeric(ft$Immature_granulocyte_perc)
#ft$IG_0.0.3 = as.numeric(ft$IG_0.0.3)
ft$PDW = as.numeric(ft$PDW)
ft$PLCR = as.numeric(ft$PLCR)
ft$PCT = as.numeric(ft$PCT)
ft$RBS = as.numeric(ft$RBS)
ft$GLYCOSYLATED_Hb_IFCC = as.numeric(ft$GLYCOSYLATED_Hb_IFCC)
ft$BUN = as.numeric(ft$BUN)
ft$BUN_Sr_creatinine = as.numeric(ft$BUN_Sr_creatinine)
ft$Uric_Acid = as.numeric(ft$Uric_Acid)
ft$Estimated_Glomerular_filtration_rate = as.numeric(ft$Estimated_Glomerular_filtration_rate)
ft$NonHDL_Cholesterol = as.numeric(ft$NonHDL_Cholesterol)
ft$Vitamin_D_25_Hydroxy = as.numeric(ft$Vitamin_D_25_Hydroxy)
ft$Free_T3 = as.numeric(ft$Free_T3)
ft$Free_T4 = as.numeric(ft$Free_T4)
ft$Hs.CRP_High_Sensitivity_CRP = as.numeric(ft$Hs.CRP_High_Sensitivity_CRP)
ft$Transferrin = as.numeric(ft$Transferrin)
ft$Polymorphs = as.numeric(ft$Polymorphs)
#ft$PCB = as.numeric(ft$PCB)
ft$Sodium = as.numeric(ft$Sodium)
ft$Potassium = as.numeric(ft$Potassium)
#ft$Iron = as.numeric(ft$Iron)
ft$SerumIronStudy_TIBC_UIBC = as.numeric(ft$SerumIronStudy_TIBC_UIBC)
ft$Transferrin_Saturation = as.numeric(ft$Transferrin_Saturation)
ft$Immature_granulocytes = as.numeric(ft$Immature_granulocytes)
ft$LDH_1 = as.numeric(ft$LDH_1)
ft$Total_Calcium = as.numeric(ft$Total_Calcium)
ft$Serum_Iron = as.numeric(ft$Serum_Iron)
ft$MENTZ1 = as.numeric(ft$MENTZ1)
ft$NLR_4 = as.numeric(ft$NLR_4)
ft$PO4_mg.dl = as.numeric(ft$PO4_mg.dl)
ft$Cl._mEq.L = as.numeric(ft$Cl._mEq.L)
ft$SGOT_SGPT = as.numeric(ft$SGOT_SGPT)
ft$CHOL.LDL_Ratio = as.numeric(ft$CHOL.LDL_Ratio)
ft$APB. = as.numeric(ft$APB.)
ft$APOA = as.numeric(ft$APOA)
ft$APOB = as.numeric(ft$APOB)
#ft$LDL. = as.numeric(ft$LDL.)
ft$LPA = as.numeric(ft$LPA)
#ft$NRBC = as.numeric(ft$NRBC)
#ft$NRBC_percent = as.numeric(ft$NRBC_percent)
#ft$MID = as.numeric(ft$MID)
#ft$MID_Percent = as.numeric(ft$MID_Percent)
#ft$Granulocyte_count = as.numeric(ft$Granulocyte_count)
#ft$Granulocytes = as.numeric(ft$Granulocytes)
#ft$PWD. = as.numeric(ft$PWD.)
#ft$P.LCC_10.9.l = as.numeric(ft$P.LCC_10.9.l)
#ft$LDL_Direct = as.numeric(ft$LDL_Direct)
ft$anthropometry.head_cir = as.numeric(ft$anthropometry.head_cir)
ft$anthropometry.height = as.numeric(ft$anthropometry.height)
ft$anthropometry.hip_cir = as.numeric(ft$anthropometry.hip_cir)
ft$anthropometry.sys_bp = as.numeric(ft$anthropometry.sys_bp)
ft$anthropometry.dia_bp = as.numeric(ft$anthropometry.dia_bp)
ft$anthropometry.body_fat = as.numeric(ft$anthropometry.body_fat)
ft$anthropometry.wasit_cir = as.numeric(ft$anthropometry.wasit_cir)
ft$anthropometry.weight = as.numeric(ft$anthropometry.weight)
ft$anthropometry.glucose_mg_dl = as.numeric(ft$anthropometry.glucose_mg_dl)

# Inserting empty values --------------------------------------------------

ft$PCV_Packed_Cell_Volume = as.numeric(ft$PCV_Packed_Cell_Volume)
ft$Hematocrit = ifelse(is.na(ft$Hematocrit), 
                       ft$PCV_Packed_Cell_Volume, ft$Hematocrit)
ft = subset(ft, select = -PCV_Packed_Cell_Volume)

ft$HCT = as.numeric(ft$HCT)
ft$Hematocrit = ifelse(is.na(ft$Hematocrit), ft$HCT, ft$Hematocrit)
ft = subset(ft, select = -HCT)

ft$PCV_. = as.numeric(ft$PCV_.)
ft$Hematocrit = ifelse(is.na(ft$Hematocrit), ft$PCV_., ft$Hematocrit)
ft = subset(ft, select = -PCV_.)

ft$PCV_PackedCellVolume = as.numeric(ft$PCV_PackedCellVolume)
ft$Hematocrit = ifelse(is.na(ft$Hematocrit), ft$PCV_PackedCellVolume, ft$Hematocrit)
ft = subset(ft, select = -PCV_PackedCellVolume)

ft$Neutrophils_Lymphocyte_Ratio = as.numeric(ft$Neutrophils_Lymphocyte_Ratio)
ft$NLR_4 = ifelse(is.na(ft$NLR_4), ft$Neutrophils_Lymphocyte_Ratio, ft$NLR_4)
ft = subset(ft, select = -Neutrophils_Lymphocyte_Ratio)

ft$A.GR = as.numeric(ft$A.GR)
ft$A.GRatio = as.numeric(ft$A.GRatio)
ft$A_G_Ratio = as.numeric(ft$A_G_Ratio)
ft$A_G_Ratio_Albumin_Globulin = ifelse(is.na(ft$A_G_Ratio_Albumin_Globulin),
                                       ft$A.GR, ft$A_G_Ratio_Albumin_Globulin)
ft$A_G_Ratio_Albumin_Globulin = ifelse(is.na(ft$A_G_Ratio_Albumin_Globulin),
                                       ft$A.GRatio, ft$A_G_Ratio_Albumin_Globulin)
ft$A_G_Ratio_Albumin_Globulin = ifelse(is.na(ft$A_G_Ratio_Albumin_Globulin),
                                       ft$A_G_Ratio, ft$A_G_Ratio_Albumin_Globulin)

ft = subset(ft, select = -A.GR)
ft = subset(ft, select = -A.GRatio)
ft = subset(ft, select = -A_G_Ratio)

ft$ABG = as.numeric(ft$ABG)
ft$Estimated_Average_Glucose <- ifelse(is.na(ft$Estimated_Average_Glucose),
                                       ft$ABG,
                                       ft$Estimated_Average_Glucose)
ft = subset(ft, select = -ABG)

ft$TOTAL_LYMPHOCYTE_COUNT_1500.4000_cells.cu_mm = as.numeric(ft$TOTAL_LYMPHOCYTE_COUNT_1500.4000_cells.cu_mm)
ft$Absolute_Lymphocyte_Count = ifelse(is.na(ft$Absolute_Lymphocyte_Count),
                                      ft$TOTAL_LYMPHOCYTE_COUNT_1500.4000_cells.cu_mm,ft$Absolute_Lymphocyte_Count)

ft = subset(ft, select = -TOTAL_LYMPHOCYTE_COUNT_1500.4000_cells.cu_mm)

ft$TOTAL_LYMPHOCYTE_COUNT_cu_mm = as.numeric(ft$TOTAL_LYMPHOCYTE_COUNT_cu_mm)
ft$Absolute_Lymphocyte_Count = ifelse(is.na(ft$Absolute_Lymphocyte_Count),
                                      ft$TOTAL_LYMPHOCYTE_COUNT_cu_mm,ft$Absolute_Lymphocyte_Count)

ft = subset(ft, select = -TOTAL_LYMPHOCYTE_COUNT_cu_mm)

ft$AbsoluteLymphocyteCount = as.numeric(ft$AbsoluteLymphocyteCount)
ft$Absolute_Lymphocyte_Count = ifelse(is.na(ft$Absolute_Lymphocyte_Count),
                                      ft$AbsoluteLymphocyteCount,ft$Absolute_Lymphocyte_Count)

ft = subset(ft, select = -AbsoluteLymphocyteCount)

ft$AbsoluteBasophilCount = as.numeric(ft$AbsoluteBasophilCount)
ft$Absolute_Basophil_Count = ifelse(is.na(ft$Absolute_Basophil_Count), ft$AbsoluteBasophilCount, ft$Absolute_Basophil_Count)
ft = subset(ft, select = -AbsoluteBasophilCount)

ft$AbsoluteEosinophilCount = as.numeric(ft$AbsoluteEosinophilCount)
ft$Absolute_Eosinophil_Count = ifelse(is.na(ft$Absolute_Eosinophil_Count), ft$AbsoluteEosinophilCount, ft$Absolute_Eosinophil_Count)
ft = subset(ft, select = -AbsoluteEosinophilCount)

ft$AbsoluteNeutrophilCount = as.numeric(ft$AbsoluteNeutrophilsCount)
ft$Absolute_Neutrophil_Count = ifelse(is.na(ft$Absolute_Neutrophil_Count), ft$AbsoluteNeutrophilCount, ft$Absolute_Neutrophil_Count)
ft = subset(ft, select = -AbsoluteNeutrophilCount)

ft = subset(ft, select = -AbsoluteNeutrophilsCount)#no values could be added

ft$AbsoluteMonocyteCount = as.numeric(ft$AbsoluteMonocyteCount)
ft$Absolute_Monocyte_Count = ifelse(is.na(ft$Absolute_Monocyte_Count), ft$AbsoluteMonocyteCount, ft$Absolute_Monocyte_Count)
ft = subset(ft, select = -AbsoluteMonocyteCount)

ft$Estimated_average_glucose_eAG = as.numeric(ft$Estimated_average_glucose_eAG)
ft$Estimated_Average_Glucose = ifelse(is.na(ft$Estimated_Average_Glucose), ft$Estimated_average_glucose_eAG,
                                      ft$Estimated_Average_Glucose)
ft = subset(ft, select = -Estimated_average_glucose_eAG)

ft$EstimatedAverageGlucose_eAG = as.numeric(ft$EstimatedAverageGlucose_eAG)
ft$Estimated_Average_Glucose = ifelse(is.na(ft$Estimated_Average_Glucose), ft$EstimatedAverageGlucose_eAG,
                                      ft$Estimated_Average_Glucose)
ft = subset(ft, select = -EstimatedAverageGlucose_eAG)

ft$MEAN_BLOOD_GLUCOSE_mg.dl = as.numeric(ft$MEAN_BLOOD_GLUCOSE_mg.dl)
ft$Estimated_Average_Glucose = ifelse(is.na(ft$Estimated_Average_Glucose), ft$MEAN_BLOOD_GLUCOSE_mg.dl,
                                      ft$Estimated_Average_Glucose)
ft = subset(ft, select = -MEAN_BLOOD_GLUCOSE_mg.dl)


ft$Vitamin_D = as.numeric(ft$Vitamin_D)
ft$Vitamin_D_25_Hydroxy = ifelse(is.na(ft$Vitamin_D_25_Hydroxy), ft$Vitamin_D, ft$Vitamin_D_25_Hydroxy)
ft = subset(ft, select = -Vitamin_D)

ft$TLC = as.numeric(ft$TLC)
ft$WBC_Total_White_Blood_Cell_Count = ifelse(is.na(ft$WBC_Total_White_Blood_Cell_Count), 
                                             ft$TLC, ft$WBC_Total_White_Blood_Cell_Count)
ft = subset(ft, select = -TLC)

ft$TOTAL_LEUCOCYTE_COUNT_cu_mm = as.numeric(ft$TOTAL_LEUCOCYTE_COUNT_cu_mm)
ft$WBC_Total_White_Blood_Cell_Count = ifelse(is.na(ft$WBC_Total_White_Blood_Cell_Count), 
                                             ft$TOTAL_LEUCOCYTE_COUNT_cu_mm, 
                                             ft$WBC_Total_White_Blood_Cell_Count)
ft = subset(ft, select = -TOTAL_LEUCOCYTE_COUNT_cu_mm)

ft$WBC_Total_White_Blood_Cell_Count...23 = as.numeric(ft$WBC_Total_White_Blood_Cell_Count...23)
ft$WBC_Total_White_Blood_Cell_Count = ifelse(is.na(ft$WBC_Total_White_Blood_Cell_Count), 
                                             ft$WBC_Total_White_Blood_Cell_Count...23, 
                                             ft$WBC_Total_White_Blood_Cell_Count)
ft = subset(ft, select = -WBC_Total_White_Blood_Cell_Count...23)

ft$Total_Leucocytes_Count_in_th.cumm = as.numeric(ft$Total_Leucocytes_Count_in_th.cumm)
ft$WBC_Total_White_Blood_Cell_Count = ifelse(is.na(ft$WBC_Total_White_Blood_Cell_Count), 
                                             ft$Total_Leucocytes_Count_in_th.cumm, 
                                             ft$WBC_Total_White_Blood_Cell_Count)
ft = subset(ft, select = -Total_Leucocytes_Count_in_th.cumm)

ft$TotalLeucocytes_WBC_count = as.numeric(ft$TotalLeucocytes_WBC_count)
ft$WBC_Total_White_Blood_Cell_Count = ifelse(is.na(ft$WBC_Total_White_Blood_Cell_Count), 
                                             ft$TotalLeucocytes_WBC_count, 
                                             ft$WBC_Total_White_Blood_Cell_Count)
ft = subset(ft, select = -TotalLeucocytes_WBC_count)

ft$CALC = as.numeric(ft$CALC)
ft$Total_Calcium = ifelse(is.na(ft$Total_Calcium),
                          ft$CALC, ft$Total_Calcium)
ft = subset(ft, select = -CALC)

ft$Total_Calcium = as.numeric(ft$Total_Calcium)
ft$Calcium = ifelse(is.na(ft$Calcium),
                    ft$Total_Calcium, ft$Calcium)
ft = subset(ft, select = -Total_Calcium)

ft$AlkalinePhosphatase = as.numeric(ft$AlkalinePhosphatase)
ft$Alkaline_Phosphatase = ifelse(is.na(ft$Alkaline_Phosphatase),
                                 ft$AlkalinePhosphatase, ft$Alkaline_Phosphatase)
ft = subset(ft, select = -AlkalinePhosphatase)

ft$SGPT_ALT = as.numeric(ft$SGPT_ALT)
ft$ALT_SGPT = ifelse(is.na(ft$ALT_SGPT), ft$SGPT_ALT, ft$ALT_SGPT)
ft = subset(ft, select = -SGPT_ALT)

ft$SGOT_AST = as.numeric(ft$SGOT_AST)
ft$AST_SGOT = ifelse(is.na(ft$AST_SGOT), ft$SGOT_AST, ft$AST_SGOT)
ft = subset(ft, select = -SGOT_AST)

ft$APB..1 = as.numeric(ft$APB..1)
ft$APB. = ifelse(is.na(ft$APB.),
                 ft$APB..1, ft$APB.)
ft = subset(ft, select = -APB..1)

ft$B.CR = as.numeric(ft$B.CR)
ft$BUN_Sr_creatinine = ifelse(is.na(ft$BUN_Sr_creatinine),
                              ft$B.CR, ft$BUN_Sr_creatinine)
ft = subset(ft, select = -B.CR)

ft$VLDLCholesterol = as.numeric(ft$VLDLCholesterol)
ft$VLDL = ifelse(is.na(ft$VLDL), ft$VLDLCholesterol, ft$VLDL)
ft = subset(ft, select = -VLDLCholesterol)

ft$GammaGT_GGTP = as.numeric(ft$GammaGT_GGTP)
ft$Gamma_GT_GGTP = ifelse(is.na(ft$Gamma_GT_GGTP), ft$GammaGT_GGTP, ft$Gamma_GT_GGTP)
ft = subset(ft, select = -GammaGT_GGTP)

ft$Chloride = as.numeric(ft$Chloride)
ft$Cl._mEq.L = ifelse(is.na(ft$Cl._mEq.L), ft$Chloride, ft$Cl._mEq.L)
ft = subset(ft, select = -Chloride)

ft$Erythrocyte_RBC_Count = as.numeric(ft$Erythrocyte_RBC_Count)
ft$RBC_Red_Blood_Cell_Count = ifelse(is.na(ft$RBC_Red_Blood_Cell_Count), ft$Erythrocyte_RBC_Count, ft$RBC_Red_Blood_Cell_Count)
ft = subset(ft, select = -Erythrocyte_RBC_Count)

ft$Plateletcount = as.numeric(ft$Plateletcount)
ft$Platelet_Count = ifelse(is.na(ft$Platelet_Count), ft$Plateletcount, ft$Platelet_Count)
ft = subset(ft, select = -Plateletcount)

ft$CHOL.HDLRATIO = as.numeric(ft$CHOL.HDLRATIO)
ft$CHOL_HDL_Ratio = ifelse(is.na(ft$CHOL_HDL_Ratio), ft$CHOL.HDLRATIO, ft$CHOL_HDL_Ratio)
ft = subset(ft, select = -CHOL.HDLRATIO)

ft$MCH = as.numeric(ft$MCH)
ft$MCH_Mean_Corpuscular_Hb = ifelse(is.na(ft$MCH_Mean_Corpuscular_Hb), ft$MCH, ft$MCH_Mean_Corpuscular_Hb)
ft = subset(ft, select = -MCH)

ft$MCH_MeanCorpuscularHb = as.numeric(ft$MCH_MeanCorpuscularHb)
ft$MCH_Mean_Corpuscular_Hb = ifelse(is.na(ft$MCH_Mean_Corpuscular_Hb), ft$MCH_MeanCorpuscularHb, ft$MCH_Mean_Corpuscular_Hb)
ft = subset(ft, select = -MCH_MeanCorpuscularHb)

ft$MCHC = as.numeric(ft$MCHC)
ft$MCHC_Mean_Corpuscular_Hb_Concn = ifelse(is.na(ft$MCHC_Mean_Corpuscular_Hb_Concn), ft$MCHC, ft$MCHC_Mean_Corpuscular_Hb_Concn)
ft = subset(ft, select = -MCHC)

ft$MCHC_MeanCorpuscularHbConcn. = as.numeric(ft$MCHC_MeanCorpuscularHbConcn.)
ft$MCHC_Mean_Corpuscular_Hb_Concn = ifelse(is.na(ft$MCHC_Mean_Corpuscular_Hb_Concn), ft$MCHC_MeanCorpuscularHbConcn., ft$MCHC_Mean_Corpuscular_Hb_Concn)
ft = subset(ft, select = -MCHC_MeanCorpuscularHbConcn.)

ft$MCV_MeanCorpuscularVolume = as.numeric(ft$MCV_MeanCorpuscularVolume)
ft$MCV_Mean_Corpuscular_Volume = ifelse(is.na(ft$MCV_Mean_Corpuscular_Volume), ft$MCV_MeanCorpuscularVolume,
                                        ft$MCV_Mean_Corpuscular_Volume)
ft = subset(ft, select = -MCV_MeanCorpuscularVolume)

ft$MPV = as.numeric(ft$MPV)
ft$MPV_Mean_Platelet_Volume = ifelse(is.na(ft$MPV_Mean_Platelet_Volume), ft$MPV, ft$MPV_Mean_Platelet_Volume)
ft = subset(ft, select = -MPV)

ft$MPV_MeanPlateletVolume = as.numeric(ft$MPV_MeanPlateletVolume)
ft$MPV_Mean_Platelet_Volume = ifelse(is.na(ft$MPV_Mean_Platelet_Volume), ft$MPV_MeanPlateletVolume, ft$MPV_Mean_Platelet_Volume)
ft = subset(ft, select = -MPV_MeanPlateletVolume)

ft$NonHDLCholesterol = as.numeric(ft$NonHDLCholesterol)
ft$NonHDL_Cholesterol = ifelse(is.na(ft$NonHDL_Cholesterol), ft$NonHDLCholesterol, ft$NonHDL_Cholesterol)
ft = subset(ft, select = -NonHDLCholesterol)

ft$nonHDL = as.numeric(ft$nonHDL)
ft$NonHDL_Cholesterol = ifelse(is.na(ft$NonHDL_Cholesterol), ft$nonHDL, ft$NonHDL_Cholesterol)
ft = subset(ft, select = -nonHDL)

ft$NHDL = as.numeric(ft$NHDL)
ft$NonHDL_Cholesterol = ifelse(is.na(ft$NonHDL_Cholesterol), ft$NHDL, ft$NonHDL_Cholesterol)
ft = subset(ft, select = -NHDL)


ft$ESR.ErythrocyteSedimentationRate.EDTA = as.numeric(ft$ESR.ErythrocyteSedimentationRate.EDTA)
ft$ESR = ifelse(is.na(ft$ESR),ft$ESR.ErythrocyteSedimentationRate.EDTA, ft$ESR)
ft = subset(ft, select = -ESR.ErythrocyteSedimentationRate.EDTA)

ft$ESR_Erythrocyte_Sedimentation_Rate = as.numeric(ft$ESR_Erythrocyte_Sedimentation_Rate)
ft$ESR = ifelse(is.na(ft$ESR),ft$ESR_Erythrocyte_Sedimentation_Rate, ft$ESR)
ft = subset(ft, select = -ESR_Erythrocyte_Sedimentation_Rate)

ft$TotalProtein = as.numeric(ft$TotalProtein)
ft$Protein = ifelse(is.na(ft$Protein), ft$TotalProtein, ft$Protein)
ft = subset(ft, select = -TotalProtein)

ft$HDLCholesterol = as.numeric(ft$HDLCholesterol)
ft$HDL = ifelse(is.na(ft$HDL), ft$HDLCholesterol, ft$HDL)
ft = subset(ft, select = -HDLCholesterol)

ft$LDLCholesterol = as.numeric(ft$LDLCholesterol)
ft$LDL = ifelse(is.na(ft$LDL), ft$LDLCholesterol, ft$LDL)
ft = subset(ft, select = -LDLCholesterol)

ft$LDL_Direct = as.numeric(ft$LDL_Direct)
ft$LDL = ifelse(is.na(ft$LDL), ft$LDL_Direct, ft$LDL)
ft = subset(ft, select = -LDL_Direct)

ft$LDL. = as.numeric(ft$LDL.)
ft$LDL_HDL_Ratio = ifelse(is.na(ft$LDL_HDL_Ratio),
                          ft$LDL., ft$LDL_HDL_Ratio)
ft = subset(ft, select = -LDL.)

ft$LDL..1 = as.numeric(ft$LDL..1)
ft$LDL_HDL_Ratio = ifelse(is.na(ft$LDL_HDL_Ratio),
                          ft$LDL..1, ft$LDL_HDL_Ratio)
ft = subset(ft, select = -LDL..1)

ft$LDL.HDLRATIO = as.numeric(ft$LDL.HDLRATIO)
ft$LDL_HDL_Ratio = ifelse(is.na(ft$LDL_HDL_Ratio),
                          ft$LDL.HDLRATIO, ft$LDL_HDL_Ratio)
ft = subset(ft, select = -LDL.HDLRATIO)

ft$Haemoglobin_Hb = as.numeric(ft$Haemoglobin_Hb)
ft$HB_Haemoglobin = ifelse(is.na(ft$HB_Haemoglobin), ft$Haemoglobin_Hb, ft$HB_Haemoglobin)
ft = subset(ft, select = -Haemoglobin_Hb)

ft$IG = as.numeric(ft$IG)
ft$IG_0.0.3 = ifelse(is.na(ft$IG_0.0.3),
                     ft$IG, ft$IG_0.0.3)
ft = subset(ft, select = -IG)

ft$IG_0.0.3 = as.numeric(ft$IG_0.0.3)
ft$Immature_granulocyte_perc = ifelse(is.na(ft$Immature_granulocyte_perc),
                                      ft$IG_0.0.3, ft$Immature_granulocyte_perc)
ft = subset(ft, select = -IG_0.0.3)

ft$Immature_granulocytes = ifelse(is.na(ft$Immature_granulocytes), ft$Immature_granulocyte, ft$Immature_granulocytes)
ft = subset(ft, select = -Immature_granulocyte)

ft$UreaSerum = as.numeric(ft$UreaSerum)
ft$Urea = ifelse(is.na(ft$Urea), ft$UreaSerum, ft$Urea)
ft = subset(ft, select = -UreaSerum)

ft$TC.H = as.numeric(ft$TC.H)
ft$CHOL_HDL_Ratio = ifelse(is.na(ft$CHOL_HDL_Ratio),
                           ft$TC.H, ft$CHOL_HDL_Ratio)
ft = subset(ft, select = -TC.H)

ft$CRP...35 = as.numeric(ft$CRP...35)
ft$CRP...84 = as.numeric(ft$CRP...84)
ft$CRP = ifelse(is.na(ft$CRP),
                ft$CRP...35, ft$CRP)
ft = subset(ft, select = -CRP...35)

ft$CRP = ifelse(is.na(ft$CRP),
                ft$CRP...84, ft$CRP)
ft = subset(ft, select = -CRP...84)

ft$CRP_mg.L = as.numeric(ft$CRP_mg.L)
ft$CRP = ifelse(is.na(ft$CRP), ft$CRP_mg.L, ft$CRP)
ft = subset(ft, select = -CRP_mg.L)

ft$GLYCOSYLATED_HAEMOGLOBIN_in_. = as.numeric(ft$GLYCOSYLATED_HAEMOGLOBIN_in_.)
ft$HbA1C_Glycosylated_Haemoglobin = ifelse(is.na(ft$HbA1C_Glycosylated_Haemoglobin),
                                           ft$GLYCOSYLATED_HAEMOGLOBIN_in_., ft$HbA1C_Glycosylated_Haemoglobin)
ft = subset(ft, select = -GLYCOSYLATED_HAEMOGLOBIN_in_.)

ft$HbA1C.GlycatedHaemoglobin = as.numeric(ft$HbA1C.GlycatedHaemoglobin)
ft$HbA1C_Glycosylated_Haemoglobin = ifelse(is.na(ft$HbA1C_Glycosylated_Haemoglobin),
                                           ft$HbA1C.GlycatedHaemoglobin, ft$HbA1C_Glycosylated_Haemoglobin)
ft = subset(ft, select = -HbA1C.GlycatedHaemoglobin)


ft$Bilirubin.Indirect = as.numeric(ft$Bilirubin.Indirect)
ft$Indirect_Bilirubin = ifelse(is.na(ft$Indirect_Bilirubin), ft$Bilirubin.Indirect, ft$Indirect_Bilirubin)
ft = subset(ft, select = -Bilirubin.Indirect)

# ft$NRBC. = as.numeric(ft$NRBC.)
# ft$NRBC = ifelse(is.na(ft$NRBC),ft$NRBC.,ft$NRBC)
# ft = subset(ft, select = -NRBC.)

ft$T3 = as.numeric(ft$T3)
ft$T3_Total = ifelse(is.na(ft$T3_Total),
                     ft$T3, ft$T3_Total)
ft = subset(ft, select = -T3)

ft$T4 = as.numeric(ft$T4)
ft$T4_Total = ifelse(is.na(ft$T4_Total),
                     ft$T4, ft$T4_Total)
ft = subset(ft, select = -T4)

ft$HBA = as.numeric(ft$HBA)
ft$HbA1C_Glycosylated_Haemoglobin = ifelse(is.na(ft$HbA1C_Glycosylated_Haemoglobin),
                                           ft$HBA, ft$HbA1C_Glycosylated_Haemoglobin)
ft = subset(ft, select = -HBA)

ft$PCB = as.numeric(ft$PCB)
ft$Hematocrit = ifelse(is.na(ft$Hematocrit), ft$PCB, ft$Hematocrit)
ft = subset(ft, select = -PCB)

ft$GLUCOSE._RANDOM_R_._PLASMA = as.numeric(ft$GLUCOSE._RANDOM_R_._PLASMA)
ft$RBS = ifelse(is.na(ft$RBS), ft$GLUCOSE._RANDOM_R_._PLASMA, ft$RBS)
ft = subset(ft, select = -GLUCOSE._RANDOM_R_._PLASMA)

ft$Monocytes...15 = as.numeric(ft$Monocytes...15)
ft$Monocytes = ifelse(is.na(ft$Monocytes), ft$Monocytes...15, ft$Monocytes)
ft = subset(ft, select = -Monocytes...15)

ft$Lymphocytes...14 = as.numeric(ft$Lymphocytes...14)
ft$Lymphocytes = ifelse(is.na(ft$Lymphocytes), ft$Lymphocytes...14, ft$Lymphocytes)
ft = subset(ft, select = -Lymphocytes...14)

ft$Neutrophils...16 = as.numeric(ft$Neutrophils...16)
ft$Neutrophils = ifelse(is.na(ft$Neutrophils), ft$Neutrophils...16, ft$Neutrophils)
ft = subset(ft, select = -Neutrophils...16)

ft$Glucosefasting = as.numeric(ft$Glucosefasting)
ft$FBS_Fasting_Blood_Glucose = ifelse(is.na(ft$FBS_Fasting_Blood_Glucose), ft$Glucosefasting, ft$FBS_Fasting_Blood_Glucose)
ft = subset(ft, select = -Glucosefasting)

ft$Iron = as.numeric(ft$Iron)
ft$Serum_Iron <- ifelse(is.na(ft$Serum_Iron),ft$Iron,ft$Serum_Iron)
ft = subset(ft, select = -Iron)

ft$RDCV = as.numeric(ft$RDCV)
ft$RDW_Red_Cell_Distribution_Width = ifelse(is.na(ft$RDW_Red_Cell_Distribution_Width), ft$RDCV,
                                            ft$RDW_Red_Cell_Distribution_Width)
ft = subset(ft, select = -RDCV)

ft$RDW_RedCellDistributionWidth = as.numeric(ft$RDW_RedCellDistributionWidth)
ft$RDW_Red_Cell_Distribution_Width = ifelse(is.na(ft$RDW_Red_Cell_Distribution_Width), ft$RDW_RedCellDistributionWidth,
                                            ft$RDW_Red_Cell_Distribution_Width)
ft = subset(ft, select = -RDW_RedCellDistributionWidth)

ft$RDW = as.numeric(ft$RDW)
ft$RDW_Red_Cell_Distribution_Width = ifelse(is.na(ft$RDW_Red_Cell_Distribution_Width), 
                                            ft$RDW, ft$RDW_Red_Cell_Distribution_Width)
ft = subset(ft, select = -RDW)

ft$X.TSA = as.numeric(ft$X.TSA)
ft$Transferrin_Saturation = ifelse(is.na(ft$Transferrin_Saturation), ft$X.TSA,
                                   ft$Transferrin_Saturation)
ft = subset(ft, select = -X.TSA)

ft$Transferrin = as.numeric(ft$Transferrin)
ft$Transferrin_Saturation = ifelse(is.na(ft$Transferrin_Saturation), ft$Transferrin, ft$Transferrin_Saturation)
ft = subset(ft, select = -Transferrin)

# renaming some columns ---------------------------------------------------


#colnames(ft)[colnames(ft) == "NRBC."] ="NRBC_percent"
colnames(ft)[colnames(ft) == "anthropometry.wasit_cir"] ="anthropometry.waist_cir"
colnames(ft)[colnames(ft) == "Calcium"] = "Total_Calcium"
colnames(ft)[colnames(ft) == "TOTAL_GRANULOCYTE_COUNT_cu_mm"] = "Granulocyte_count"
#colnames(ft)[colnames(ft) == "Immature_granulocyte"] = "Immature_granulocytes"


# ethnicity cleaning ------------------------------------------------------

ft$name_dob_1.ethnicity=tolower(ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="")] = NA
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="na")] = NA
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhoksa (u.k)")]="bhoksa(uk)"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhoksa(u.k)")]="bhoksa(uk)"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhoksa (uk)")]="bhoksa(uk)"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhoksa ( u.k)")]="bhoksa(uk)"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhoksa( u.k)")]="bhoksa(uk)"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhoksa(uk)")]="bhoksa"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="lingayats")]="lingayath"
#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="lingayat")]="lingayath"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vidiki")]="vaidiki_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vidiki brahmin")]="vaidiki_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vidiki  brahmin")]="vaidiki_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vidiki brahmin (mulukanad)")]="vaidiki_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vaidiki bramhin")]="vaidiki_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vaidiki brahmin")]="vaidiki_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vaidiki bhramin")]="vaidiki_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vaidiki")]="vaidiki_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="santal")]="santhal"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="santha")]="santhal"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="santali")]="santhal"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="santhali")]="santhal"
#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="padiyachi")]="padayachi"
#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="nadoda rajput")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="hindu nadoda rajput")]="rajput"
#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="rabha/patirabha")]="rabha"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="  ")]=" "
ft$name_dob_1.ethnicity = gsub("\\s+", " ", ft$name_dob_1.ethnicity)
#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bharadwaj")]="bhardwaj"
condition = grepl("ravidas", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity[condition] <- sub(" .*", "", ft$name_dob_1.ethnicity[condition])
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="ravidas(hr)")]="ravidas"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="ravidas(up)")]="ravidas"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="ravidas/dhusia")]="ravidas"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="ravidasbnaden")]="ravidas"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kennat")]="kannets"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kanat")]="kannets"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kannat")]="kannets"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kannet")]="kannets"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kunnet")]="kannets"
#ft$name_dob_1.ethnicity = gsub("kennet", "kannet", ft$name_dob_1.ethnicity)
#condition1 = grepl("kannet", ft$name_dob_1.ethnicity)
#ft$name_dob_1.ethnicity[condition1] <- sub(" .*", "", ft$name_dob_1.ethnicity[condition1])
#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="iyengar father tamil iyengar mother mandyam iyengar")]="iyangar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kani kubj brahmin")]="kanyakubj_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kani kunj")]="kanyakubj_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kannauj")]="kanyakubj_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kanyakubj")]="kanyakubj_brahmin"

ft$name_dob_1.ethnicity <- sub("agarwal.*", "agarwal", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity <- sub("agrawal.*", "agrawal", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity <- sub("audichya sahastra.*", "audichya sahastra", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="aydichya sahastra")]="audichya sahastra"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="audichya sahastra")]="audichya_sahastra"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="desastha")]="deshastha_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kudmi mahato")]="kudmi_mahato"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kurmi mahato")]="kudmi_mahato"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="aagstya")]="saryuparin_brahmin"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="khasi/jaintia")]="khasi"

ft$name_dob_1.ethnicity <- sub("yadav/.*", "yadav", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity <- sub("paliwal.*", "paliwal_brahmin", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity <- sub("rajput.*", "rajput", ft$name_dob_1.ethnicity)

#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="yadav/")]="yadav"
#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="yadav/yatra")]="yadav"
# condition6 = grepl("saryupairin", ft$name_dob_1.ethnicity)
# ft$name_dob_1.ethnicity[condition6] <- sub(" .*", "", ft$name_dob_1.ethnicity[condition6])
# condition7 = grepl("jat", ft$name_dob_1.ethnicity)
# ft$name_dob_1.ethnicity[condition7] <- sub(" .*", "", ft$name_dob_1.ethnicity[condition7])
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="jaat")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="jat")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="jata")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="panchaar(jat)")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhadyasar(jat)")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="saran(jat)")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="gwala(jat)")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="chotiya(jat)")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="danga(jat)")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bison horn maria")]="bison_horn_maria"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="agarwal")]="aggarwal"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="agrawal")]="aggarwal"

#ft$name_dob_1.ethnicity = sub("^mizo/(.*)", "mizo", ft$name_dob_1.ethnicity)
#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="scheduled tribe/mizo/pawih/lai/khenglawt")]="mizo"
#ft$name_dob_1.ethnicity = sub("^maithil brahmin/(.*)", "maithil brahmin", ft$name_dob_1.ethnicity)
#fft$name_dob_1.ethnicity = sub("^maithil brahmin /(.*)", "maithil brahmin", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="maithil brahmin")]="maithili_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="mathil brahamin")]="maithili_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="maithili")]="maithili_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="maithili brahmin")]="maithili_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="maithili brahmin r")]="maithili_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="rahri brahmin")]="rahri_brahmin"

#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="namibiar")]="nambiar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kol")]="koli (tribe)"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kolii")]="kolis"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="koli")]="kolis"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="namboodiri bhramin")]="namboodari"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="namboodri brahmin")]="namboodari"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="nambudiris")]="namboodari"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="nambudri")]="namboodari"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="nambo")]="namboodari"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="gujar")]="gujjar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="gujjar (hr)")]="gujjar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="gujjar (up)")]="gujjar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="gurjur")]="gujjar"


ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bakmiki")]="balmiki"

#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="valmiki/balmiki")]="balmiki"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="mal")]="mala"

ft$name_dob_1.ethnicity = sub("sb", "saryuparin_brahmin", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity <- sub("saryuparin_brahmin.*", "saryuparin_brahmin", ft$name_dob_1.ethnicity)

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="nabarangpur")]="gond"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vakaliga")]="vakkaliga"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="ansari")]="ansari_sunni"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="adi karnataka")]="adikarnataka"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="iyyengar")]="iyengar"
ft$name_dob_1.ethnicity = sub(".*iyyengars", "iyengar", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity = sub(".*iyyengar", "iyengar", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="iyangar")]="iyengar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="champawat rajput")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="champawat")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="dabhi rajput")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="hindu rajput")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vaghela rajput")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vanar rajput")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="jadeja rajput")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sodawat")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="solanki")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="abu road")]="bhil meena"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bachic")]="thakur"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="baghel")]="thakur"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="baigani")]="baiga"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhambu")]="jat"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bheel")]="bhil"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhii meena")]="bhil_meena"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhil merna")]="bhil_meena"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhil meena")]="bhil_meena"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bheel meena")]="bhil_meena"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhillmeena")]="bhil_meena"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="meena (rajasthan)")]="bhil_meena"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="meena (r.j)")]="bhil_meena"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bodh (spiti)")]="spiti"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="chauhan")]="thakur"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="chou")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="chudawat")]="rajput"
#ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="danta")]="saryuparin brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="dhola")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="garg")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="chik barai")]="chik_baraik"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="chik baraik")]="chik_baraik"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="dang")]="dangi"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="halu")]="halakki"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="s.p.b")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="saryupari")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="saryuparian")]="saryuparin_brahmin"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="katheria")]="thakur"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kathelia")]="thakur"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="dongri bhil")]="dongri_bhil"
ft$name_dob_1.ethnicity <- sub("khatri.*", "khatri_pb", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="laguni mishra")]="saryuparin brahmin"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="ghatela")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="godara")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="jat")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="gotham")]="saryuparin_brahmin"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="hakki pikki")]="hakkipikki"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="hakki pikiki")]="hakkipikki"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="garhakota")]="aggarwal"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="jaipur")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="jongksha")]="khasi"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="hindu vankar")]="vankar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="hindukoli")]="kolis"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kallar calto")]="kallar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sidq")]="siddi"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="jamnagar")]="siddi"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="nad")]="nadar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="mund")]="munda"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="khadav")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kondal")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="koshti")]="lingayath"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kotinya")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kurmi gangwar")]="thakur"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kushwaha")]="thakur"


ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kadva patel")]="patidar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kadwa patel")]="patidar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kadva")]="patidar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="leva patel")]="patidar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="levva patel")]="patidar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="patel")]="patidar"
ft$name_dob_1.ethnicity <- sub("patidar.*", "patidar", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="rajesh")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="talpda koli")]="kolis"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kanwar")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kalwaniya")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashik")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kaswan")]="jats"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kheaadla")]="balmiki"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sengar")]="thakur"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shangrah")]="kannets"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="prayagraj")]="thakur"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kuruma")]="kuruman"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="maratha")]="marathas"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="marathi")]="marathas"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="makwana vankar")]="vankar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vankar hindu")]="vankar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="niyogi brahmin")]="vaidiki_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="rathore")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="rawat")]="rajput"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sahariya")]="saharia"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="tamil" & ft$LocalID=="CCMB/C/050026")]="nadar"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="tamil" & ft$LocalID=="CCMB/C/060080")]="parayan"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="tomar")]="thakur"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="tiya")]="thiya"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vashist")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vastasya")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vastaya")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vastsa")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="vats")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="manipuri")]="meitei"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kokanasth brahmin")]="konkonastha_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kokanastha")]="konkonastha_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="koknastha brahmin")]="konkonastha_brahmin"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="prayan")]="parayan"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="paniya")]="paniyan"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="raj banshi")]="rajbangshi"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="rajbanshi")]="rajbangshi"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="odia brahmin")]="oriya_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="odia bramhin")]="oriya_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="oriya brahmin")]="oriya_brahmin"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="garawali bharthwaj")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="garhwali")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="garhwali barthwaj")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="garhwali bharthwaj")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="garhwali brahmin")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="garhwali brahmin/sonak")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="baradwaj")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="barthwaj")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bharatwaj")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bharthwaj")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bharthwj")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhargav/")]="kanyakubj_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bharist")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bhrmin")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="garg gotra saryuparian")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bramin" & ft$LocalID=="CBRI/B/060193")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bramin" & ft$LocalID=="CBRI/B/085310")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bramin" & ft$LocalID=="CBRI/B/085253")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="brahmins")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bramhin" & ft$LocalID=="CBRI/B/035186")]="vaidiki_brahmin"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="bramhin")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="karn kayastha")]="karn_kayastha"

ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashyap" & ft$LocalID=="CBRI/B/060191")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashyap" & ft$LocalID=="CBRI/B/085205")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashyap" & ft$LocalID=="CBRI/B/085217")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashyap" & ft$LocalID=="CBRI/B/085225")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashyap" & ft$LocalID=="CBRI/B/085205")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashyap" & ft$LocalID=="CBRI/B/085289")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kasyap" & ft$LocalID=="CBRI/B/075134")]="kayastha"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kasyap" & ft$LocalID=="CBRI/B/075136")]="kayastha"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kasyap" & ft$LocalID=="CBRI/B/075140")]="kayastha"


ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashyap")]="kayastha"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kasyap")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kasyap gotra")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kasyap kayastha")]="kayastha"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kayastha kasyap")]="kayastha"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="khasyap")]="kayastha"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashyp")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="kashyap gothara")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sandhayala")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sandheyalga")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sandilya")]="garhwali_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sandiyala")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sandiyala gothara")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sandyala")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="saravana")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sarvana")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sarvaran")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sarvaran gotra")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sarveprave")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sarvaran gotra")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="saryaveparve")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="saryaveparvein")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="saryuparin brahmin")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="saryuparin")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="savaran")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shandaya")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shandayal")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shandayal gotra")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shandil")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shandilya")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shandilya gotra")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shaneliya")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shring")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="shyandilya")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sravana")]="saryuparin_brahmin"
ft$name_dob_1.ethnicity[which(ft$name_dob_1.ethnicity=="sravuveparave bramin")]="saryuparin_brahmin"

# Adding regions ----------------------------------------------------------

ft$name_dob_1.state = tolower(ft$name_dob_1.state)
ft$name_dob_1.state[which(ft$name_dob_1.state=='others')] = NA
#ft$name_dob_1.state[which(ft$name_dob_1.state=='NA')] = NA
ft$name_dob_1.state = ifelse(is.na(ft$name_dob_1.state),ft$name_dob_1.other_state, ft$name_dob_1.state)

ft = subset(ft, select = -name_dob_1.other_state)
ft$name_dob_1.state = tolower(ft$name_dob_1.state)

ft$name_dob_1.state[ft$name_dob_1.state=="bangalore"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="bangalore, karnataka"] = "karnataka"
# ft$name_dob_1.state[ft$name_dob_1.state=="chennai"] = "tamil_nadu"
# ft$name_dob_1.state[ft$name_dob_1.state=="cheruanchary, kerala"] = "kerala"
# ft$name_dob_1.state[ft$name_dob_1.state=="kadirur, kerala"] = "kerala"
# ft$name_dob_1.state[ft$name_dob_1.state=="kalambur"] = "tamil_nadu"
# ft$name_dob_1.state[ft$name_dob_1.state=="kanchipuram"] = "tamil_nadu"
# ft$name_dob_1.state[ft$name_dob_1.state=="karvatenagaram, chittur dustrict, andra pradesh"] = "andra_pradesh"
# ft$name_dob_1.state[ft$name_dob_1.state=="kottayam, kerala"] = "kerala"
ft$name_dob_1.state[ft$name_dob_1.state=="melkote, mandya district, , karnataka"] = "karnataka"
#ft$name_dob_1.state[ft$name_dob_1.state=="muzaffarpur , bihar"] = "bihar"
# ft$name_dob_1.state[ft$name_dob_1.state=="ochira ,kerala"] = "kerala"
# ft$name_dob_1.state[ft$name_dob_1.state=="ochira, kerala"] = "kerala"
ft$name_dob_1.state[ft$name_dob_1.state=="telengana"] = "telangana"
#ft$name_dob_1.state[ft$name_dob_1.state=="tellichrry, kerala"] = "kerala"
ft$name_dob_1.state[ft$name_dob_1.state=="yeliyur, karnataka"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="dalasanur"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="gdm"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="grp"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="gummareddipura"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="gummareddypura"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="gummaresdypura"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="gundamamatha"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="k d halli"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="kadudevandahalli"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="kd halli"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="kdh"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="kdhalli"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="khd"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="kuppahalli"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="kuppahalli (v)"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="valageranahalli"] = "karnataka"
ft$name_dob_1.state[ft$name_dob_1.state=="vgh"] = "karnataka"

ft$name_dob_1.state[ft$LocalID=="CBRI/B/035016"] = "karnataka"
ft$name_dob_1.state[ft$LocalID=="CBRI/B/045084"] = "himachal_pradesh"
ft$name_dob_1.state[ft$LocalID=="CBRI/B/075044"] = "uttar_pradesh"
ft$name_dob_1.state[ft$LocalID=="CBRI/B/055167"] = "rajasthan"
ft$name_dob_1.state[ft$LocalID=="AIIM/J/021071"] = "rajasthan"
ft$name_dob_1.state[ft$LocalID=="CCMB/C/030294"] = "andra_pradesh"
ft$name_dob_1.state[ft$LocalID=="CCMB/C/030270"] = "andra_pradesh"
ft$name_dob_1.state[ft$LocalID=="CCMB/C/090111"] = "andra_pradesh"
ft$name_dob_1.state[ft$LocalID=="RGCB/L/005262"] = "kerala"
ft$name_dob_1.state[ft$LocalID=="MZUA/I/015039"] = "assam"
ft$name_dob_1.state[ft$LocalID=="ILSB/H/000081"] = "orissa"
ft$name_dob_1.state[ft$LocalID=="ILSB/H/000238"] = "orissa"
ft$name_dob_1.state[ft$LocalID=="ILSB/H/000351"] = "orissa"
ft$name_dob_1.state[ft$LocalID=="GBRC/D/030088"] = "gujarat"

ft$region = ifelse(ft$name_dob_1.state %in% c('andra_pradesh', 'karnataka', 'kerala','tamil_nadu', 'telangana'), "South", 
                   ifelse(ft$name_dob_1.state %in% c("arunachal_pradesh","assam", "manipur", "meghalaya", "mizoram"), "North-East", 
                          ifelse(ft$name_dob_1.state %in% c("bihar", "jharkhand", "orissa", "west_bengal", "sikkim"), "East", 
                                 ifelse(ft$name_dob_1.state %in% c("haryana", "himachal_pradesh", "jammu_kashmir", "punjab", "rajasthan", "Delhi", "delhi", "New Delhi", "new delhi"), "North", 
                                        ifelse(ft$name_dob_1.state %in% c("gujarat", "maharashtra"), "West",
                                               ifelse(ft$name_dob_1.state %in% c("madya_pradesh","chhattisgarh", "uttaranchal","uttar_pradesh"), "Central","Unknown"))))))

# Smoking status cleaning, marital status -------------------------------------------------

ft$smoking_tobacco_alcohol.alcohol_status = tolower(ft$smoking_tobacco_alcohol.alcohol_status)
ft$smoking_tobacco_alcohol.chewing_tobacco_status = tolower(ft$smoking_tobacco_alcohol.chewing_tobacco_status)
ft$smoking_tobacco_alcohol.smoking_status = tolower(ft$smoking_tobacco_alcohol.smoking_status)
#ft$smoking_tobacco_alcohol.alcohol_status[which(ft$smoking_tobacco_alcohol.alcohol_status=="")]=NA
ft$smoking_tobacco_alcohol.alcohol_status[which(ft$smoking_tobacco_alcohol.alcohol_status=="yes")]="current"
ft$smoking_tobacco_alcohol.alcohol_status[which(ft$smoking_tobacco_alcohol.alcohol_status=="0")]="never"
ft$smoking_tobacco_alcohol.alcohol_status[which(ft$smoking_tobacco_alcohol.alcohol_status=="no")]="never"
#ft$smoking_tobacco_alcohol.chewing_tobacco_status[which(ft$smoking_tobacco_alcohol.chewing_tobacco_status=="")]=NA
ft$smoking_tobacco_alcohol.chewing_tobacco_status[which(ft$smoking_tobacco_alcohol.chewing_tobacco_status=="yes")]="current"
ft$smoking_tobacco_alcohol.chewing_tobacco_status[which(ft$smoking_tobacco_alcohol.chewing_tobacco_status=="0")]="never"
ft$smoking_tobacco_alcohol.chewing_tobacco_status[which(ft$smoking_tobacco_alcohol.chewing_tobacco_status=="currently_abstinent")]="past"
ft$smoking_tobacco_alcohol.chewing_tobacco_status[which(ft$smoking_tobacco_alcohol.chewing_tobacco_status=="currently_using")]="current"
#ft$smoking_tobacco_alcohol.smoking_status[which(ft$smoking_tobacco_alcohol.smoking_status=="")]=NA
#ft$smoking_tobacco_alcohol.smoking_status[which(ft$smoking_tobacco_alcohol.smoking_status=="0")]="never"
#ft$smoking_tobacco_alcohol.smoking_status[which(ft$smoking_tobacco_alcohol.smoking_status=="currently_abstinent")]="past"
#ft$smoking_tobacco_alcohol.smoking_status[which(ft$smoking_tobacco_alcohol.smoking_status=="currently_using")]="current"
ft$smoking_tobacco_alcohol.smoking_status[which(ft$smoking_tobacco_alcohol.smoking_status=="yes")]="current"
#ft$socio_demographics.marital_status[which(ft$socio_demographics.marital_status=="")]=NA
ft$socio_demographics.marital_status[which(ft$socio_demographics.marital_status=="currently_currently_married")]='currently_married'
ft$socio_demographics.marital_status[which(ft$socio_demographics.marital_status=='never_currently_currently_married')] = NA
ft$socio_demographics.marital_status[which(ft$socio_demographics.marital_status=='Un-currently_married')] = "currently_married"
ft$socio_demographics.marital_status[which(ft$socio_demographics.marital_status=='0')] = "never_married"
ft$socio_demographics.marital_status[which(ft$socio_demographics.marital_status=='cohabiting')] = "never_married"


# History of illness cleaning ---------------------------------------------

ft$history_illness.history_illness_self=tolower(ft$history_illness.history_illness_self)
ft$history_illness.history_illness_family[which(ft$history_illness.history_illness_family=="")] = NA
ft$history_illness.medication_currently_status = tolower(ft$history_illness.medication_currently_status)
ft$history_illness.medication_currently_status[which(ft$history_illness.medication_currently_status=='')] = NA
ft$history_illness.history_illness_self = trimws(ft$history_illness.history_illness_self)
ft$history_illness.history_illness_self = gsub("^,+", "", ft$history_illness.history_illness_self)
ft$history_illness.history_illness_self = gsub(",+$", "", ft$history_illness.history_illness_self)
ft$history_illness.history_illness_self = gsub("\\s+", " ", ft$history_illness.history_illness_self)
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self==" ")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self==",,,")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="n")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="n a")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="na")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="na ki")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="na,,,,,,,,,")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="na(")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="ni")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nik")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nik8")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nil")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nill")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nio")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="no")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="na")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="none")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="none,,,")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="none(")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self==",na")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nq")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nw")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="n.a")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="n/a")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="n0")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="n6")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="na by")]=NA
ft$history_illness.history_illness_self <- sub("na .*", "na", ft$history_illness.history_illness_self)
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="na9")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="naa")]=NA
ft$history_illness.history_illness_self <- sub("nil .*", "na", ft$history_illness.history_illness_self)
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="never problem")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nil.")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nilh")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nip")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="niu")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="njl")]=NA
ft$history_illness.history_illness_self <- sub("no .*", "na", ft$history_illness.history_illness_self)
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="no?")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="none8")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nono")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="noo")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="now")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nau")]=NA
ft$history_illness.history_illness_self[which(ft$history_illness.history_illness_self=="nra")]=NA


ft$history_illness.name_medication[ft$history_illness.name_medication=="Na"] = NA
ft$history_illness.name_medication[ft$history_illness.name_medication=="No"] = NA
ft$history_illness.name_medication[ft$history_illness.name_medication=="Na"] = NA


# Age calculation - age and age_withBBC ------------------------------------
ft$name_dob_1.dob[ft$name_dob_1.dob == ""] = NA
ft$introduction_1.examination_date[ft$introduction_1.examination_date == ""] = NA
#ft$name_dob_1.dob = gsub(",", "", ft$name_dob_1.dob)
ft$name_dob_1.dob = gsub("/", "-", ft$name_dob_1.dob)
ft$name_dob_1.dob[as.numeric(ft$name_dob_1.dob)<0] = NA
#ft$introduction_1.examination_date = gsub(",", "", ft$introduction_1.examination_date)
ft$introduction_1.examination_date = gsub("/", "-", ft$introduction_1.examination_date)

library(anytime)
library(lubridate)

ft$name_dob_1.dob <- sapply(ft$name_dob_1.dob, function(date) {
  if (nchar(date) == 10 && grepl("^\\d{2}-\\d{2}-\\d{4}$", date)) {
    # If it's in "dd-mm-yyyy" or "mm-dd-yyyy" format, try both and check validity
    parsed_date_dmy <- dmy(date)
    parsed_date_mdy <- mdy(date)
    
    if (!is.na(parsed_date_dmy) && !is.na(parsed_date_mdy)) {
      # Both are valid, you need to choose a strategy here
      # For example, prefer one format over the other
      return(as.character(parsed_date_dmy, format = "%Y-%m-%d"))
    } else if (!is.na(parsed_date_dmy)) {
      return(as.character(parsed_date_dmy, format = "%Y-%m-%d"))
    } else if (!is.na(parsed_date_mdy)) {
      return(as.character(parsed_date_mdy, format = "%Y-%m-%d"))
    } else {
      return(date)  # Date couldn't be parsed
    }
  } else {
    return(date)
  }
})

ft$name_dob_1.dob <- sapply(ft$name_dob_1.dob, function(date) {
  if (nchar(date) == 9 && grepl("^\\d{1}-\\d{2}-\\d{4}$", date)) {  # Check if string has 5 characters and all characters are numeric
    date <- mdy(date)
    return(as.character(date, format = "%Y-%m-%d"))
  } else if (nchar(date)== 9 && grepl("^[0-9]{9}$", date)) {
    return(as.Date(as.POSIXct(as.numeric(date), origin = "1970-01-01", tz = "UTC")))
  } else {
    return(date)
  }
})

ft$name_dob_1.dob <- sapply(ft$name_dob_1.dob, function(date) {
  if (nchar(date) == 8 && grepl("^[0-9]{8}$", date)) {  # Check if string has 5 characters and all characters are numeric
    return(as.Date(as.POSIXct(as.numeric(date), origin = "1970-01-01", tz = "UTC")))
  } else {
    return(date)
  }
})

ft$name_dob_1.dob <- sapply(ft$name_dob_1.dob, function(date) {
  if (nchar(date) == 5 && grepl("^[0-9]+$", date)) {  # Check if string has 5 characters and all characters are numeric
    formatted_date <- as.Date(as.numeric(date), origin = "1899-12-30")
    return(format(formatted_date, "%Y-%m-%d"))
  } else {
    return(date)
  }
})

ft$name_dob_1.dob <- sapply(ft$name_dob_1.dob, function(date) {
  if (nchar(date) == 4 && grepl("^[0-9]+$", date)) {  # Check if string has 5 characters and all characters are numeric
    formatted_date <- as.Date(as.numeric(date), origin = "1899-12-30")
    return(format(formatted_date, "%Y-%m-%d"))
  } else {
    return(date)
  }
})

ft$name_dob_1.dob <- sapply(ft$name_dob_1.dob, function(date) {
  if (nchar(date) == 3 && grepl("^[0-9]+$", date)) {  # Check if string has 5 characters and all characters are numeric
    formatted_date <- as.Date(as.numeric(date), origin = "1899-12-30")
    return(format(formatted_date, "%Y-%m-%d"))
  } else {
    return(date)
  }
})

# parse_and_convert_date <- function(date_str) {
#   if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str)) {
#     # If it's in "yyyy-mm-dd" format, just return it
#     return(date_str)
#   } else if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date_str)) {
#     # If it's in "dd-mm-yyyy" format, convert it to "yyyy-mm-dd"
#     parsed_date <- dmy(date_str)
#     return(as.character(parsed_date, format = "%Y-%m-%d"))
#   } else if (grepl("^\\d{1}-\\d{2}-\\d{4}$", date_str)) {
#     # If it's in "dd-mm-yyyy" format, convert it to "yyyy-mm-dd"
#     parsed_date <- mdy(date_str)
#     return(as.character(parsed_date, format = "%Y-%m-%d"))
#   } else if (grepl("^\\d{2}-[A-Za-z]{3}-\\d{4}$", date_str)) {
#     # If it's in "dd-bb-yyyy" format, convert it to "yyyy-mm-dd"
#     parsed_date <- dmy(date_str)
#     return(as.character(parsed_date, format = "%Y-%m-%d"))
#   } else if (grepl("^\\d{2}-[A-Za-z]{3}-\\d{2}$", date_str)) {
#     # If it's in "dd-bb-yy" format, convert it to "yyyy-mm-dd"
#     parsed_date <- dmy(date_str)
#     return(as.character(parsed_date, format = "%Y-%m-%d"))
#   } else if (grepl("^[0-9]{5}$", date_str)) {
#     # If it's an Excel date number
#     return(as.Date(as.numeric(date_str), origin = "1899-12-30"))
#   } else if (grepl("^[0-9]{10}$", date_str)) {
#     # If it's a Unix timestamp (with optional milliseconds)
#     return(as.Date(as.POSIXct(as.numeric(date_str), origin = "1970-01-01", tz = "UTC")) )
#   } else if (grepl("^[0-9]{9}$", date_str)) {
#     return(as.Date(as.POSIXct(as.numeric(date_str), origin = "1970-01-01", tz = "UTC")))
#   } else if (grepl("^[0-9]{8}$", date_str)) {
#     return(as.Date(as.POSIXct(as.numeric(date_str), origin = "1970-01-01", tz = "UTC")))
#   } else {
#     return(date_str)  # Date couldn't be parsed
#   }
# }
# 
# # Standardize the date format
# ft <- ft %>%
#   mutate(name_dob_1.dob = sapply(name_dob_1.dob, parse_and_convert_date))

# library(parsedate)
# ft$name_dob_1.dob = as.Date(parse_date(ft$name_dob_1.dob)) 

parse_and_convert_date1 <- function(date_str) {
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", date_str)) {
    # If it's in "yyyy-mm-dd" format, just return it
    return(date_str)
  } else if (grepl("^\\d{2}-\\d{2}-\\d{4}$", date_str)) {
    # If it's in "dd-mm-yyyy" or "mm-dd-yyyy" format, try both and check validity
    parsed_date_dmy <- dmy(date_str)
    parsed_date_mdy <- mdy(date_str)
    
    if (!is.na(parsed_date_dmy) && !is.na(parsed_date_mdy)) {
      # Both are valid, you need to choose a strategy here
      # For example, prefer one format over the other
      return(as.character(parsed_date_dmy, format = "%Y-%m-%d"))
    } else if (!is.na(parsed_date_dmy)) {
      return(as.character(parsed_date_dmy, format = "%Y-%m-%d"))
    } else if (!is.na(parsed_date_mdy)) {
      return(as.character(parsed_date_mdy, format = "%Y-%m-%d"))
    } else {
      return(date_str)  # Date couldn't be parsed
    }
  } else if (grepl("^\\d{1}-\\d{2}-\\d{4}$", date_str)) {
    # If it's in "m-dd-yyyy" format, convert it to "yyyy-mm-dd"
    parsed_date <- mdy(date_str)
    return(as.character(parsed_date, format = "%Y-%m-%d"))
  } else if (grepl("^\\d{2}-[A-Za-z]{3}-\\d{4}$", date_str)) {
    # If it's in "dd-bbb-yyyy" format, convert it to "yyyy-mm-dd"
    parsed_date <- dmy(date_str)
    return(as.character(parsed_date, format = "%Y-%m-%d"))
  } else if (grepl("^\\d{2}-[A-Za-z]{3}-\\d{2}$", date_str)) {
    # If it's in "dd-bbb-yy" format, convert it to "yyyy-mm-dd"
    parsed_date <- dmy(date_str)
    return(as.character(parsed_date, format = "%Y-%m-%d"))
  } else if (grepl("^[0-9]{10}$", date_str)) {
    # If it's a Unix timestamp (with optional milliseconds)
    as.Date(as.POSIXct(as.numeric(date_str), origin = "1970-01-01", tz = "UTC")) 
  } else {
    return(date_str)  # Date couldn't be parsed
  }
}

# Standardize the date format
ft <- ft %>%
  mutate(introduction_1.examination_date = sapply(introduction_1.examination_date, parse_and_convert_date1))

ft$introduction_1.examination_date <- sapply(ft$introduction_1.examination_date, function(date) {
  if (nchar(date) == 5 && grepl("^[0-9]+$", date)) {  # Check if string has 5 characters and all characters are numeric
    formatted_date <- as.Date(as.numeric(date), origin = "1899-12-30")
    return(format(formatted_date, "%Y-%m-%d"))
  } else {
    return(date)
  }
})



ft$age =trunc((ft$name_dob_1.dob %--% ft$introduction_1.examination_date) / years(1))
ft$age[ft$age<18] = NA
#sum(is.na(ft$age))
#ft$age_on_interview = floor(as.numeric(ft$name_dob_1.age_on_interview))
#ft$name_dob_1.dob[ft$name_dob_1.dob == ""] = NA
# plot(ft$name_dob_1.approx_age ~ ft$age_on_interview, ylim = c(0, 100), col = as.factor(ft$name_dob_1.gender))
# abline(0, 1)
# plot(ft$calc_age ~ ft$name_dob_1.approx_age, xlim = c(0, 100))
# abline(0, 1)
#library(dplyr)
# agevec = subset(ft, select = c("LocalID", "Age", "name_dob_1.dob", "age", "name_dob_1.gender", "name_dob_1.year_of_birth", "name_dob_1.age_on_interview", "name_dob_1.approx_age", "introduction_1.examination_date"))
# #agevec$age_on_interview[agevec$age_on_interview == ""] = NA
# agevec$Age[agevec$Age == ""] = NA
# agevec$Age[agevec$Age == "F"] = NA
# agevec$Age[agevec$Age == "M"] = NA
# agevec$Age[agevec$Age == "NULL"] = NA
# 
# agevec$Age = gsub("/M", "", agevec$Age)
# agevec$Age = gsub("/ M", "", agevec$Age)
# agevec$Age = gsub("/F", "", agevec$Age)
# agevec$Age = gsub("Y", "", agevec$Age)
# agevec$Age = trimws(agevec$Age)
# 
# agevec$Age = abs(as.numeric(agevec$Age))


#agevec$name_dob_1.dob[agevec$name_dob_1.dob == ""] = NA
#agevec$age[agevec$calc_age == ""] = NA
#agevec$name_dob_1.year_of_birth[agevec$name_dob_1.year_of_birth == ""] = NA
#agevec$name_dob_1.age_on_interview[agevec$name_dob_1.age_on_interview == ""] = NA
#agevec$name_dob_1.approx_age[agevec$name_dob_1.approx_age == ""] = NA
#agevec$introduction_1.examination_date[agevec$introduction_1.examination_date == ""] = NA
#agevec$age[agevec$age < 18] = NA
# agevec$name_dob_1.age_on_interview[agevec$name_dob_1.age_on_interview <18] = NA
# agevec$name_dob_1.approx_age[agevec$name_dob_1.approx_age<18] = NA
# #agevec$age_on_interview[agevec$age_on_interview <18] = NA
# agevec$name_dob_1.approx_age[agevec$name_dob_1.approx_age > 250] = NA
# agevec = agevec[!duplicated(agevec$LocalID),]
# #AGE MISSING VALUE IMPUTATION - CALC AGE AND AGE ON INTERVIEW
# colSums(is.na(agevec))
# agevec$calc_age = ifelse(is.na(agevec$calc_age), agevec$name_dob_1.age_on_interview, agevec$calc_age)
# agevec$calc_age <- ifelse(is.na(agevec$calc_age), agevec$name_dob_1.approx_age, agevec$calc_age)
# agevec$calc_age[agevec$calc_age < 18] = NA
# colSums(is.na(agevec))

# ft$Age = as.numeric(ft$Age)
# ft$Age[ft$Age<18] = NA
# ft$calc_age = as.numeric(ft$calc_age)
# ft$name_dob_1.age_on_interview = as.numeric(ft$name_dob_1.age_on_interview)
# ft$name_dob_1.approx_age = as.numeric(ft$name_dob_1.approx_age)

# plt1 = ggpairs(ft, columns = c("Age","calc_age","name_dob_1.age_on_interview","name_dob_1.approx_age"), 
#                aes(color = name_dob_1.gender, alpha = 0.5),cardinality_threshold = NULL) + geom_abline(intercept = 0, slope = 1)
# plt1
# 
#putting into original dataset
ft$Age[ft$Age == ""] = NA
ft$Age[ft$Age == "F"] = NA
ft$Age[ft$Age == "M"] = NA
ft$Age[ft$Age == "NULL"] = NA

ft$Age = gsub("/M", "", ft$Age)
ft$Age = gsub("/ M", "", ft$Age)
ft$Age = gsub("/F", "", ft$Age)
ft$Age = gsub("Y", "", ft$Age)
ft$Age = trimws(ft$Age)

ft$Age = (as.numeric(ft$Age))


#ft$calc_age = agevec[match(ft$LocalID, agevec$LocalID), ]$calc_age
ft$name_dob_1.age_on_interview[ft$name_dob_1.age_on_interview<18] = NA
ft$age = ifelse(is.na(ft$age), ft$name_dob_1.age_on_interview, ft$age)
ft$name_dob_1.approx_age[ft$name_dob_1.approx_age<18] = NA
ft$name_dob_1.approx_age[ft$name_dob_1.approx_age>250] = NA
ft$age = ifelse(is.na(ft$age), ft$name_dob_1.approx_age, ft$age)
#after inspecting this column found some ages in here
ft$name_dob_1.year_of_birth[ft$name_dob_1.year_of_birth<18] = NA

ft <- ft %>% 
  mutate(age = ifelse(is.na(age) & name_dob_1.year_of_birth < 100, name_dob_1.year_of_birth, age))

#NIBG has reported that their BBC age is the ODK age
ft$age <- ifelse(is.na(ft$age) & ft$center == "NIBG", ft$Age, ft$age)
#ft$calc_age[ft$calc_age<18] = NA

ft$age_withBBC = ft$age
ft$age_withBBC = ifelse(is.na(ft$age_withBBC), ft$Age, ft$age_withBBC)
ft$age_withBBC[ft$age_withBBC<18] = NA

ft = ft[-which(ft$LocalID=="CBRI/B/040009"),] # PD sample

ft$name_dob_1.dob = as.character(ft$name_dob_1.dob)
ft$introduction_1.examination_date = as.character(ft$introduction_1.examination_date)
# Adding SKIMS data -------------------------------------------------------

skims_df = read.table("SKIMS_final.csv", sep = ",", header = T, fill = T)
skims_df[skims_df==""] = NA
skims_df = skims_df[-which(is.na(skims_df$Barcode)),]

#Renaming SKIMS data columns

names(skims_df)[names(skims_df) == "Albumin..g.dl."] <- "Albumin"
names(skims_df)[names(skims_df) == "ALP..IU.L."] <- "Alkaline_Phosphatase"
names(skims_df)[names(skims_df) == "Barcode"] <- "LocalID"
names(skims_df)[names(skims_df) == "BSF..mg.dl."] <- "FBS_Fasting_Blood_Glucose"
names(skims_df)[names(skims_df) == "BSR.mg.dl."] <- "RBS"
names(skims_df)[names(skims_df) == "Cholestrol..mg.dl."] <- "Cholesterol"
names(skims_df)[names(skims_df) == "Creatinine..mg.dl."] <- "Creatinine"
names(skims_df)[names(skims_df) == "GRA..10.9.l."] <- "Granulocyte_count"
names(skims_df)[names(skims_df) == "GRA."] <- "Granulocytes"
names(skims_df)[names(skims_df) == "HbA1C.."] <- "HbA1C_Glycosylated_Haemoglobin"
names(skims_df)[names(skims_df) == "HDL..mg.dl."] <- "HDL"
names(skims_df)[names(skims_df) == "HGB..g.dI."] <- "HB_Haemoglobin"
names(skims_df)[names(skims_df) == "LDL..mg.dl."] <- "LDL"
names(skims_df)[names(skims_df) == "LYM..10.9.l."] <- "Absolute_Lymphocyte_Count"
names(skims_df)[names(skims_df) == "LYM."] <- "Lymphocytes"
names(skims_df)[names(skims_df) == "MCH..pg."] <- "MCH_Mean_Corpuscular_Hb"
names(skims_df)[names(skims_df) == "MID."] <- "MID_Percent"
names(skims_df)[names(skims_df) == "MID..10.9.l."] <- "MID"
names(skims_df)[names(skims_df) == "OT.AST..IU.L."] <- "AST_SGOT"
names(skims_df)[names(skims_df) == "PT.ALT..IU.L."] <- "ALT_SGPT"
names(skims_df)[names(skims_df) == "Total.Bilrubin..mg.dl."] <- "Total_Bilirubin"
names(skims_df)[names(skims_df) == "Total.Protein..mg.dl."] <- "Protein"
names(skims_df)[names(skims_df) == "Triglyceride..mg.dl."] <- "Triglycerides"
names(skims_df)[names(skims_df) == "UREA..mg.dl."] <- "Urea"
names(skims_df)[names(skims_df) == "U..ACID..mg.dl."] <- "Uric_Acid"
names(skims_df)[names(skims_df) == "WBC..10.9.l."] <- "WBC_Total_White_Blood_Cell_Count"


# SKIM adding ODK ---------------------------------------------------------


odk_main = read.table("GenomeIndia_Socio_demographics_V1_results(1).csv", header = T, sep = ",", fill = T, encoding = 'latin1')
odk_main$localid = odk_main$introduction_1.local_id_manual
odk_main$localid[odk_main$localid==""] = NA
odk_main$localid = ifelse(is.na(odk_main$localid), odk_main$introduction.local_id_barcode, odk_main$localid)
odk_main$center = gsub("\\/.*","", odk_main$localid)
names(odk_main)[names(odk_main)=="anthropometry.wasit_cir"] = "anthropometry.waist_cir"

common_cols <- intersect(names(odk_main), names(ft))

# Find columns to add to skims_df
cols_to_add <- setdiff(common_cols, names(skims_df))

# Merge skims_df with odk_main based on LocalID
skims_df <- merge(skims_df, odk_main, by.x = "LocalID", by.y = "localid", all.x = TRUE)

# Add columns from odk_main to skims_df based on matching LocalID
for (col in cols_to_add) {
  if (!col %in% names(skims_df)) {
    skims_df[[col]] <- skims_df[[paste0(col, ".y")]]
    skims_df[[paste0(col, ".y")]] <- NULL  # Remove the .y suffixed column
  }
}
skims_df$center = "SKIM"
skims_df = subset(skims_df, select = -Gender)
#age resolution
skims_df$name_dob_1.dob[skims_df$name_dob_1.dob == ""] = NA
skims_df$introduction_1.examination_date[skims_df$introduction_1.examination_date == ""] = NA
#skims_df$name_dob_1.dob = gsub(",", "", skims_df$name_dob_1.dob)
#$introduction_1.examination_date = gsub(",", "", skims_df$introduction_1.examination_date)

# SKIM age resolution -----------------------------------------------------


library(anytime)
library(lubridate)

#skims_df$dob1 = skims_df$name_dob_1.dob
skims_df$name_dob_1.dob = as.Date(skims_df$name_dob_1.dob, format = '%Y-%m-%d')

skims_df$introduction_1.examination_date = as.Date(skims_df$introduction_1.examination_date, format = '%Y-%m-%d')



skims_df$age =trunc((skims_df$name_dob_1.dob %--% skims_df$introduction_1.examination_date) / years(1))
sum(is.na(skims_df$age))

#ft$calc_age = agevec[match(ft$LocalID, agevec$LocalID), ]$calc_age
skims_df$name_dob_1.age_on_interview[skims_df$name_dob_1.age_on_interview<18] = NA
skims_df$age = ifelse(is.na(skims_df$age), skims_df$name_dob_1.age_on_interview, skims_df$age)
#ft$name_dob_1.approx_age[ft$name_dob_1.approx_age<18] = NA
#ft$name_dob_1.approx_age[ft$name_dob_1.approx_age>250] = NA
skims_df$age = ifelse(is.na(skims_df$age), skims_df$name_dob_1.approx_age, skims_df$age)
#after inspecting this column found some ages in here
#ft$name_dob_1.year_of_birth[ft$name_dob_1.year_of_birth<18] = NA

skims_df <- skims_df %>% 
  mutate(age = ifelse(is.na(age) & name_dob_1.year_of_birth < 100, name_dob_1.year_of_birth, age))

skims_df$age[skims_df$age<18] = NA
skims_df$age_withBBC = skims_df$age
skims_df$age_withBBC = ifelse(is.na(skims_df$age_withBBC), skims_df$Age, skims_df$age_withBBC)
skims_df$age_withBBC[skims_df$age_withBBC<18] = NA

# SKIM ethnicity cleaning -------------------------------------------------


skims_df$name_dob_1.ethnicity = tolower(skims_df$name_dob_1.ethnicity)
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="")] = NA
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="na")] = NA

skims_df$name_dob_1.ethnicity = sub(".*/(kashmiri/[^/]+)", "\\1", skims_df$name_dob_1.ethnicity)
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="kashmir/muslim")]="kashmiri_muslim"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="kashmiri muslim")]="kashmiri_muslim"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="kahmiri/muslim")]="kashmiri_muslim"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="muslim")]="kashmiri_muslim"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="kashmiri/muslim")]="kashmiri_muslim"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="syed/muslim/kashmiri")]="kashmiri_muslim"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="bhat/ kashmiri/muslim")]="kashmiri_muslim"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="syed kashmiri muslim")]="kashmiri_muslim"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="syed kashmiri  muslim")]="kashmiri_muslim"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="bhat/ kashmiri/pandit")]="kashmiri_pandit"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="kashmiri pandit")]="kashmiri_pandit"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="kashmiri/pandit")]="kashmiri_pandit"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="gujjjar/bakarwal")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="mir/gujjar/muslim")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="chichi/gujjar/muslim")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="gujjar/muslim")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="gujjar/ muslim")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="gujar/bakarwal")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="gujar/bakarwak")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="gujjar/bakarwak")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="gujjar/bakarwal")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="bakarwal")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="gujar")]="gujjar_and_bakkarwal"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="dogri")]="dogra"
skims_df$name_dob_1.ethnicity[which(skims_df$name_dob_1.ethnicity=="kashmiri")]="kashmiri_pandit"


skims_df$region = "North"
skims_df$center = 'SKIM'


# adding-subtracting SKIMS columns ----------------------------------------
#I Want to preserve these values, but the original DF does not have this
ft$Granulocytes = NA
ft$MID = NA
ft$MID_Percent = NA

skims_df <- skims_df[, !colnames(skims_df) %in% setdiff(colnames(skims_df), colnames(ft))]

new_cols = c(setdiff(colnames(ft), colnames(skims_df)))
library(dplyr)
SKIM_joined = skims_df %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

SKIM_joined<-SKIM_joined[names(ft)]

SKIM_joined$nonmissing = rowSums(is.na(SKIM_joined))

SKIM_joined = SKIM_joined[!SKIM_joined$LocalID %in% setdiff(SKIM_joined$LocalID, odk_main$localid),]
SKIM_joined = subset(SKIM_joined, select = -nonmissing)

SKIM_joined$name_dob_1.dob = as.character(SKIM_joined$name_dob_1.dob)
SKIM_joined$introduction_1.examination_date = as.character(SKIM_joined$introduction_1.examination_date)

ft = rbind(ft, SKIM_joined)



# ILSB blood counts replacement -------------------------------------------

library(xlsx)
library(here)
ILSB = read_excel("ILSB_CBC.xlsx", col_types = 'text')

names(ILSB)[names(ILSB) == "Barcode"] <- "LocalID"
names(ILSB)[names(ILSB) == "RBC"] <- "RBC_Red_Blood_Cell_Count"
names(ILSB)[names(ILSB) == "TLC"] <- "WBC_Total_White_Blood_Cell_Count"
names(ILSB)[names(ILSB) == "PLT"] <- "Platelet_Count"
names(ILSB)[names(ILSB) == "PCV"] <- "Hematocrit"
names(ILSB)[names(ILSB) == "HGB"] <- "HB_Haemoglobin"
names(ILSB)[names(ILSB) == "MCV"] <- "MCV_Mean_Corpuscular_Volume"
names(ILSB)[names(ILSB) == "MCHC"] <- "MCHC_Mean_Corpuscular_Hb_Concn"
names(ILSB)[names(ILSB) == "MCH"] <- "MCH_Mean_Corpuscular_Hb"
names(ILSB)[names(ILSB) == "RDW"] <- "RDW_Red_Cell_Distribution_Width"
names(ILSB)[names(ILSB) == "MPV"] <- "MPV_Mean_Platelet_Volume"
names(ILSB)[names(ILSB) == "NEUT"] <- "Absolute_Neutrophil_Count"
names(ILSB)[names(ILSB) == "LYMPH"] <- "Absolute_Lymphocyte_Count"
names(ILSB)[names(ILSB) == "MONO"] <- "Absolute_Monocyte_Count"
names(ILSB)[names(ILSB) == "EO"] <- "Absolute_Eosinophil_Count"
names(ILSB)[names(ILSB) == "BASO"] <- "Absolute_Basophil_Count"
names(ILSB)[names(ILSB) == "NEUT_per"] <- "Neutrophils"
names(ILSB)[names(ILSB) == "LYMPH_per"] <- "Lymphocytes"
names(ILSB)[names(ILSB) == "MONO_per"] <- "Monocytes"
names(ILSB)[names(ILSB) == "EO_per"] <- "Eosinophils"
names(ILSB)[names(ILSB) == "BASO_per"] <- "Basophils"


ILSB$LocalID = gsub("ILS/", "ILSB/", ILSB$LocalID)

ft$Absolute_Basophil_Count[ft$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Basophil_Count[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$Absolute_Neutrophil_Count[ft$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Neutrophil_Count[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$Absolute_Eosinophil_Count[ft$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Eosinophil_Count[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$Absolute_Monocyte_Count[ft$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Monocyte_Count[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$Absolute_Lymphocyte_Count[ft$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Lymphocyte_Count[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]

ft$Monocytes[ft$LocalID %in% ILSB$LocalID] <- ILSB$Monocytes[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$Basophils[ft$LocalID %in% ILSB$LocalID] <- ILSB$Basophils[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$Eosinophils[ft$LocalID %in% ILSB$LocalID] <- ILSB$Eosinophils[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$Neutrophils[ft$LocalID %in% ILSB$LocalID] <- ILSB$Neutrophils[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$Lymphocytes[ft$LocalID %in% ILSB$LocalID] <- ILSB$Lymphocytes[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]

ft$Platelet_Count[ft$LocalID %in% ILSB$LocalID] <- ILSB$Platelet_Count[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]

ft$RBC_Red_Blood_Cell_Count[ft$LocalID %in% ILSB$LocalID] <- ILSB$RBC_Red_Blood_Cell_Count[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$WBC_Total_White_Blood_Cell_Count[ft$LocalID %in% ILSB$LocalID] <- ILSB$WBC_Total_White_Blood_Cell_Count[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$Hematocrit[ft$LocalID %in% ILSB$LocalID] <- ILSB$Hematocrit[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$HB_Haemoglobin[ft$LocalID %in% ILSB$LocalID] <- ILSB$HB_Haemoglobin[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$MCV_Mean_Corpuscular_Volume[ft$LocalID %in% ILSB$LocalID] <- ILSB$MCV_Mean_Corpuscular_Volume[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$MCHC_Mean_Corpuscular_Hb_Concn[ft$LocalID %in% ILSB$LocalID] <- ILSB$MCHC_Mean_Corpuscular_Hb_Concn[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$MCH_Mean_Corpuscular_Hb[ft$LocalID %in% ILSB$LocalID] <- ILSB$MCH_Mean_Corpuscular_Hb[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$RDW_Red_Cell_Distribution_Width[ft$LocalID %in% ILSB$LocalID] <- ILSB$RDW_Red_Cell_Distribution_Width[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
ft$MPV_Mean_Platelet_Volume[ft$LocalID %in% ILSB$LocalID] <- ILSB$MPV_Mean_Platelet_Volume[match(ft$LocalID[ft$LocalID %in% ILSB$LocalID], ILSB$LocalID)]


# Have to check if I can add more samples to ft with ILSB -----------------

# Adding IGIB data --------------------------------------------------------

library(xlsx)
library(here)
IGIB = read_excel("IGIB4_December2023.xlsx", col_types = 'text')

names(IGIB)[names(IGIB) == "Local ID"] <- "LocalID"
names(IGIB)[names(IGIB) == "TOTAL WBC COUNT (Automated) in th/cumm"] <- "WBC_Total_White_Blood_Cell_Count"
names(IGIB)[names(IGIB) == "fwbc"] <- "Platelet_Count"
names(IGIB)[names(IGIB) == "ABSOLUTE MONOCYTE COUNT  in 10^9/L"] <- "Absolute_Monocyte_Count"

IGIB$WBC_Total_White_Blood_Cell_Count = as.numeric(IGIB$WBC_Total_White_Blood_Cell_Count)
ft$WBC_Total_White_Blood_Cell_Count[ft$LocalID %in% IGIB$LocalID] <- IGIB$WBC_Total_White_Blood_Cell_Count[match(ft$LocalID[ft$LocalID %in% IGIB$LocalID], IGIB$LocalID)]
ft$Platelet_Count[ft$LocalID %in% IGIB$LocalID] <- IGIB$Platelet_Count[match(ft$LocalID[ft$LocalID %in% IGIB$LocalID], IGIB$LocalID)]
ft$Platelet_Count = as.numeric(ft$Platelet_Count)
ft$Absolute_Monocyte_Count[ft$LocalID %in% IGIB$LocalID] <- IGIB$Absolute_Monocyte_Count[match(ft$LocalID[ft$LocalID %in% IGIB$LocalID], IGIB$LocalID)]

# Madia data --------------------------------------------------------------
library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
# ILSB_names_list = read_xlsx("BALASORE MADIA TRIBE NAME DETAILS WITH ILS AND GS IDS (1).xlsx")
# names(ILSB_names_list)[names(ILSB_names_list) == "ILS BARCODE IDs"] <- "LocalID"

ILSB_BBC_data = read_xlsx("madia_cbc.xlsx")
ILSB_BBC_data = subset(ILSB_BBC_data, select = -RegDate)
ILSB_BBC_data = subset(ILSB_BBC_data, select = -PName)
ILSB_BBC_data = subset(ILSB_BBC_data, select = -Mobile)
ILSB_BBC_data = subset(ILSB_BBC_data, select = -`Lab No`)
ILSB_BBC_data = subset(ILSB_BBC_data, select = -TestName)

ILSB_BBC_data = ILSB_BBC_data %>% drop_na(LocalID)
ILSB_BBC_data = ILSB_BBC_data[-which(ILSB_BBC_data$Reading == "HEAD"),]

ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="A/G Ratio"] = "A_G_Ratio_Albumin_Globulin"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Absolute Basophil Count"] = "Absolute_Basophil_Count"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Absolute Eosinophil Count"] = "Absolute_Eosinophil_Count"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Absolute Lymphocyte Count"] = "Absolute_Lymphocyte_Count"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Absolute Monocyte Count"] = "Absolute_Monocyte_Count"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Absolute Neutrophil Count"] = "Absolute_Neutrophil_Count"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Alanine Transaminase (SGPT)"] = "ALT_SGPT"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Albumin"] = "Albumin"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Alkaline Phosphatase"] = "Alkaline_Phosphatase"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Aspartate Transaminase (SGOT)"] = "AST_SGOT"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Basophils"] = "Basophils"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Bilirubin-Direct"] = "Direct_Bilirubin"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Bilirubin-Indirect"] = "Indirect_Bilirubin"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Bilirubin-Total"] = "Total_Bilirubin"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Blood Urea Nitrogen"] = "BUN"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="BUN/Creatinine Ratio"] = "BUN_Sr_creatinine"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="C-Reactive Protein  (Quantitative)"] = "Hs.CRP_High_Sensitivity_CRP"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Chloride"] = "Cl._mEq.L"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Cholesterol - HDL"] = "HDL"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Cholesterol - LDL"] = "LDL"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Cholesterol - Total"] = "Cholesterol"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Cholesterol : HDL Cholesterol"] = "CHOL_HDL_Ratio"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Cholesterol- VLDL"] = "VLDL"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Creatinine"] = "Creatinine"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Eosinophils"] = "Eosinophils"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Estimated average glucose (eAG)"] = "Estimated_Average_Glucose"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Gamma Glutamyltransferase (GGT)"] = "Gamma_GT_GGTP"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Globulin"] = "Globulin"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Glucose- Random"] = "RBS"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Glucose - Fasting"] = "FBS_Fasting_Blood_Glucose"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Glycosylated Hemoglobin (HbA1c)"] = "HbA1C_Glycosylated_Haemoglobin"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="HCT"] = "Hematocrit"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Hemoglobin"] = "HB_Haemoglobin"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="LDL : HDL Cholesterol"] = "LDL_HDL_Ratio"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Lymphocytes"] = "Lymphocytes"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="MCH"] = "MCH_Mean_Corpuscular_Hb"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="MCHC"] = "MCHC_Mean_Corpuscular_Hb_Concn"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="MCV"] = "MCV_Mean_Corpuscular_Volume"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Monocytes"] = "Monocytes"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="MPV"] = "MPV_Mean_Platelet_Volume"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Neutrophils"] = "Neutrophils"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Non HDL Cholesterol"] = "NonHDL_Cholesterol"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="PDW"] = "PDW"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Platelet Count"] = "Platelet_Count"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Potassium"] = "Potassium"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Protein, Total"] = "Protein"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="RBC"] = "RBC_Red_Blood_Cell_Count"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="RDW-CV"] = "RDW_Red_Cell_Distribution_Width"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="SGOT/SGPT"] = "SGOT_SGPT"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Sodium"] = "Sodium"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="T3, Total"] = "T3_Total"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="T4, Total"] = "T4_Total"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Thyroid Stimulating Hormone - Ultra Sensitive"] = "TSH"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Total Leucocyte Count"] = "WBC_Total_White_Blood_Cell_Count"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Triglycerides"] = "Triglycerides"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Urea"] = "Urea"
ILSB_BBC_data$LabObservationName[ILSB_BBC_data$LabObservationName=="Uric Acid"] = "Uric_Acid"


ILSB_BBC_data_copy = ILSB_BBC_data
ILSB_BBC_data_copy <- ILSB_BBC_data_copy %>% 
  pivot_wider(names_from = LabObservationName, values_from = Reading)

ILSB_BBC_data_copy = subset(ILSB_BBC_data_copy, select = -Gender)
# ILSB_BBC_data_copy$WBC_Total_White_Blood_Cell_Count = as.numeric(ILSB_BBC_data_copy$WBC_Total_White_Blood_Cell_Count) * 1000
# ILSB_BBC_data_copy$Platelet_Count = as.numeric(ILSB_BBC_data_copy$Platelet_Count) * 1000
# ILSB_BBC_data_copy$Absolute_Basophil_Count = as.numeric(ILSB_BBC_data_copy$Absolute_Basophil_Count) * 1000
# ILSB_BBC_data_copy$Absolute_Eosinophil_Count = as.numeric(ILSB_BBC_data_copy$Absolute_Eosinophil_Count) * 1000
# ILSB_BBC_data_copy$Absolute_Neutrophil_Count = as.numeric(ILSB_BBC_data_copy$Absolute_Neutrophil_Count) * 1000
# ILSB_BBC_data_copy$Absolute_Monocyte_Count = as.numeric(ILSB_BBC_data_copy$Absolute_Monocyte_Count) * 1000
# ILSB_BBC_data_copy$Absolute_Lymphocyte_Count = as.numeric(ILSB_BBC_data_copy$Absolute_Lymphocyte_Count) * 1000

# odk_main = read.table("GenomeIndia_Socio_demographics_V1_results(1).csv", header = T, sep = ",", fill = T, encoding = 'UTF-8')
# odk_main$localid = odk_main$introduction_1.local_id_manual
# odk_main$localid[odk_main$localid==""] = NA
# odk_main$localid = ifelse(is.na(odk_main$localid), odk_main$introduction.local_id_barcode, odk_main$localid)

ILSB_ODK = odk_main[odk_main$localid %in% ILSB_BBC_data_copy$LocalID,]
#odk_main = subset(odk_main, select = -localid)

ILSB_joined <- merge(ILSB_BBC_data_copy, ILSB_ODK, by.x = "LocalID", 
                     by.y = "localid", all.x = TRUE, all.y = TRUE)

ILSB_joined$age = ILSB_joined$name_dob_1.approx_age
ILSB_joined$age_withBBC = ILSB_joined$age
#ILSB_joined = subset(ILSB_joined, select = -Age)
#ILSB_joined = subset(ILSB_joined, select = -Gender)
#ILSB_joined = ILSB_joined[-which(ILSB_joined$LocalID=="ILSB/H/001101"),]
#ILSB_joined = ILSB_joined[-which(ILSB_joined$LocalID=="ILSB/H/001186"),]
ILSB_joined$center = "ILSB"
ILSB_joined$region = "East"
ILSB_joined$name_dob_1.ethnicity = "madia"

#names(ILSB_joined)[names(ILSB_joined) == "anthropometry.wasit_cir"] <- "anthropometry.waist_cir"

ILSB_joined <- ILSB_joined[, !colnames(ILSB_joined) %in% setdiff(colnames(ILSB_joined), colnames(ft))]

new_cols = c(setdiff(colnames(ft), colnames(ILSB_joined)))
library(dplyr)
ILSB_joined = ILSB_joined %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

ILSB_joined<-ILSB_joined[names(ft)]
ILSB_joined$introduction_1.examination_date = as.Date(ILSB_joined$introduction_1.examination_date, format = '%Y-%m-%d')
ILSB_joined$name_dob_1.dob = NA
#names(ILSB_joined)[names(ILSB_joined) == "Immature_granulocytes.1"] = "Immature_granulocytes"
ILSB_joined$introduction_1.examination_date = as.character(ILSB_joined$introduction_1.examination_date)

ft = rbind(ft, ILSB_joined)


# Karn Kayastha data ------------------------------------------------------

library(readxl)
NIBG_odk = read_xlsx("GenomeIndia_Socio_demographics_ODK_Karan_KAYASTHA_BHUBANESWAR.xlsx")
NIBG_odk = subset(NIBG_odk, select = -KEY)
NIBG_odk = subset(NIBG_odk, select = -SubmissionDate)
colnames(NIBG_odk) <- gsub(x = colnames(NIBG_odk), pattern = "\\-", replacement = ":")  
colnames(NIBG_odk) <- gsub(x = colnames(NIBG_odk), pattern = "\\:", replacement = ".")  

names(NIBG_odk)[names(NIBG_odk)=="anthropometry.wasit_cir"] = "anthropometry.waist_cir"
NIBG_odk$localid = NIBG_odk$introduction.local_id_barcode
NIBG_odk$center = gsub("\\/.*","", NIBG_odk$localid)
#odk_main = read.table("GenomeIndia_Socio_demographics_V1_results(1).csv", header = T, sep = ",", fill = T, encoding = 'UTF-8')


if (all(colnames(odk_main) == colnames(NIBG_odk))) {
  print("Column names are equal")
} else {
  print("Column names are not equal")
}

NIBG_BBC = read_xlsx("NIBMG_biochemistry_May 2024 (Karn Kayastha CBR Format).xlsx")

NIBG_BBC = subset(NIBG_BBC, select = -`Sl No.`)
NIBG_BBC = subset(NIBG_BBC, select = -`Sample ID`)
NIBG_BBC = subset(NIBG_BBC, select = -`Collection Center`)
NIBG_BBC = subset(NIBG_BBC, select = -`Factors`)
#NIBG_BBC = subset(NIBG_BBC, select = -`AGE (Years)`)
NIBG_BBC = subset(NIBG_BBC, select = -`Sex`)

names(NIBG_BBC)[names(NIBG_BBC) == "Sample barcode"] <- "LocalID"
names(NIBG_BBC)[names(NIBG_BBC) == "AGE (Years)"] <- "Age"
names(NIBG_BBC)[names(NIBG_BBC) == "HEMOGLOBIN (g/dL)"] <- "HB_Haemoglobin"
names(NIBG_BBC)[names(NIBG_BBC) == "RBC_Count (million/?L)"] <- "RBC_Red_Blood_Cell_Count"
names(NIBG_BBC)[names(NIBG_BBC) == "WBC_Count (thousands/?L)"] <- "WBC_Total_White_Blood_Cell_Count"
names(NIBG_BBC)[names(NIBG_BBC) == "PLATELET_Count (Lakhs/?L)"] <- "Platelet_Count"
names(NIBG_BBC)[names(NIBG_BBC) == "NEUTROPHILS (%)"] <- "Neutrophils"
names(NIBG_BBC)[names(NIBG_BBC) == "ABSOLUTE_NEUTROPHIL_COUNT (thousands/?L)"] <- "Absolute_Neutrophil_Count"
names(NIBG_BBC)[names(NIBG_BBC) == "EOSINOPHILS (%)"] <- "Eosinophils"
names(NIBG_BBC)[names(NIBG_BBC) == "ABSOLUTE_EOSINOPHIL_COUNT (thousands/?L)"] <- "Absolute_Eosinophil_Count"
names(NIBG_BBC)[names(NIBG_BBC) == "LYMPHOCYTES (%)"] <- "Lymphocytes"
names(NIBG_BBC)[names(NIBG_BBC) == "ABSOLUTE_LYMPHOCYTE_COUNT (thousands/?L)"] <- "Absolute_Lymphocyte_Count"
names(NIBG_BBC)[names(NIBG_BBC) == "MONOCYTES (%)"] <- "Monocytes"
names(NIBG_BBC)[names(NIBG_BBC) == "ABSOLUTE_MONOCYTE_COUNT (thousand/?L)"] <- "Absolute_Monocyte_Count"
names(NIBG_BBC)[names(NIBG_BBC) == "BASOPHILS (%)"] <- "Basophils"
names(NIBG_BBC)[names(NIBG_BBC) == "ABSOLUTE_BASOPHIL_COUNT (thousand/?L)"] <- "Absolute_Basophil_Count"
names(NIBG_BBC)[names(NIBG_BBC) == "SGOT (U/L)"] <- "AST_SGOT"
names(NIBG_BBC)[names(NIBG_BBC) == "SGPT (U/L)"] <- "ALT_SGPT"
names(NIBG_BBC)[names(NIBG_BBC) == "ALKALINE_PHOSPHATASE (U/L)"] <- "Alkaline_Phosphatase"
names(NIBG_BBC)[names(NIBG_BBC) == "GLUCOSE_FASTING_PLASMA (mg/dL)"] <- "FBS_Fasting_Blood_Glucose"
names(NIBG_BBC)[names(NIBG_BBC) == "CHOLESTEROL (mg/dL)"] <- "Cholesterol"
names(NIBG_BBC)[names(NIBG_BBC) == "TRIGLYCERIDES (mg/dL)"] <- "Triglycerides"
names(NIBG_BBC)[names(NIBG_BBC) == "HDL_CHOLESTEROL (mg/dL)"] <- "HDL"
names(NIBG_BBC)[names(NIBG_BBC) == "LDL_CHOLESTEROL (mg/dL)"] <- "LDL"
names(NIBG_BBC)[names(NIBG_BBC) == "CREATININE (mg/dL)"] <- "Creatinine"

NIBG_joined <- merge(NIBG_BBC, NIBG_odk, by.x = "LocalID", 
                     by.y = "localid", all.x = TRUE, all.y = TRUE)

NIBG_joined$name_dob_1.ethnicity = "karn_kayastha"
NIBG_joined$name_dob_1.state= "orissa"
NIBG_joined$region = "East"

NIBG_joined$smoking_tobacco_alcohol.chewing_tobacco_status = NA
NIBG_joined$smoking_tobacco_alcohol.alcohol_status[NIBG_joined$smoking_tobacco_alcohol.alcohol_status=="Yes"] = "current"
NIBG_joined$smoking_tobacco_alcohol.alcohol_status[NIBG_joined$smoking_tobacco_alcohol.alcohol_status=="No"] = "never"
NIBG_joined$smoking_tobacco_alcohol.smoking_status[NIBG_joined$smoking_tobacco_alcohol.smoking_status=="Yes"] = "current"
NIBG_joined$smoking_tobacco_alcohol.smoking_status[NIBG_joined$smoking_tobacco_alcohol.smoking_status=="No"] = "never"

NIBG_joined$name_dob_1.gender = tolower(NIBG_joined$name_dob_1.gender)
NIBG_joined$age = NIBG_joined$name_dob_1.approx_age
NIBG_joined$age_withBBC = NIBG_joined$age

NIBG_joined <- NIBG_joined[, !colnames(NIBG_joined) %in% setdiff(colnames(NIBG_joined), colnames(ft))]

new_cols = c(setdiff(colnames(ft), colnames(NIBG_joined)))
library(dplyr)
NIBG_joined = NIBG_joined %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

NIBG_joined<-NIBG_joined[names(ft)]
NIBG_joined$LocalID = gsub("NIBG/O/", "NIBG/T/", NIBG_joined$LocalID) #as clarified
NIBG_joined$name_dob_1.dob = NA #Since it is empty
NIBG_joined$introduction_1.examination_date = as.Date(NIBG_joined$introduction_1.examination_date, format = '%Y-%m-%d')
NIBG_joined$introduction_1.examination_date = as.character(NIBG_joined$introduction_1.examination_date)
#names(NIBG_joined)[names(NIBG_joined) == "Immature_granulocytes.1"] = "Immature_granulocytes"
ft = rbind(ft, NIBG_joined)


# Additional samples ------------------------------------------------------
addnsamples = read.table("additionalsamples_April10.txt", sep = "~", header = T, fill = T)
emptycolumns = colSums(is.na(addnsamples)|addnsamples=="")==nrow(addnsamples)
addnsamples = addnsamples[ ,!emptycolumns]
addnsamples$center = gsub("\\/.*","", addnsamples$LocalID)
addnsamples = addnsamples[-which(addnsamples$center=="SKIM"),]


# Ethnicity cleaning - additional samples ---------------------------------
addnsamples$name_dob_1.ethnicity[which(addnsamples$name_dob_1.ethnicity=="agarwal")]="aggarwal"
addnsamples$name_dob_1.ethnicity[which(addnsamples$name_dob_1.ethnicity=="jat")]="jats"
addnsamples$name_dob_1.ethnicity[which(addnsamples$name_dob_1.ethnicity=="bhil marwadi")]="bhil_meena"
addnsamples$name_dob_1.ethnicity[which(addnsamples$name_dob_1.ethnicity=="bhilmeena")]="bhil_meena"
addnsamples$name_dob_1.ethnicity[which(addnsamples$name_dob_1.ethnicity=="bhil meena")]="bhil_meena"
addnsamples$name_dob_1.ethnicity[which(addnsamples$name_dob_1.ethnicity=="ansari")]="ansari_sunni"
addnsamples$name_dob_1.ethnicity[which(addnsamples$name_dob_1.ethnicity=="paliwal brahmin")]="paliwal_brahmin"
addnsamples$name_dob_1.ethnicity[which(addnsamples$name_dob_1.ethnicity=="bison horn maria")]="bison_horn_maria"

# Region ------------------------------------------------------------------

addnsamples$region = ifelse(addnsamples$name_dob_1.state %in% c('andra_pradesh', 'karnataka', 'kerala','tamil_nadu', 'telangana'), "South", 
                            ifelse(addnsamples$name_dob_1.state %in% c("arunachal_pradesh","assam", "manipur", "meghalaya", "mizoram"), "North-East", 
                                   ifelse(addnsamples$name_dob_1.state %in% c("bihar", "jharkhand", "orissa", "west_bengal", "sikkim"), "East", 
                                          ifelse(addnsamples$name_dob_1.state %in% c("haryana", "himachal_pradesh", "jammu_kashmir", "punjab", "rajasthan", "Delhi", "delhi", "New Delhi", "new delhi"), "North", 
                                                 ifelse(addnsamples$name_dob_1.state %in% c("gujarat", "maharashtra"), "West",
                                                        ifelse(addnsamples$name_dob_1.state %in% c("madya_pradesh","chhattisgarh", "uttaranchal","uttar_pradesh"), "Central","Unknown"))))))

# unnecessary columns removed ---------------------------------------------

addnsamples = subset(addnsamples, select = -Collection_Center)
#addnsamples = subset(addnsamples, select = -NON.HDL_CHOLESTEROL)
#addnsamples = subset(addnsamples, select = -Calcium)
#addnsamples = subset(addnsamples, select = -BUN.Creatinine_Ratio)
addnsamples = addnsamples[,!names(addnsamples) %in% c('checklists.start', 'checklists.end', 'checklists.today',
                                                      'checklists.deviceid','checklists.subscriberid','checklists.simserial', 
                                                      'checklists.phonenumber', 'checklists.intro', 
                                                      'checklists.checklist_before_socio', 'check_n_ckecklist.incomplete_checklist',
                                                      'consent.consent_genetics','consent.consent_bloodbiochem_antrophometry',
                                                      'consent.consent_long_term_storage','consent.consent_recontact',
                                                      'comments_signature.sign_interviewer','meta.instanceID', 'Names_-_Matching',
                                                      "blood_draw.complications_blood_draw",
                                                      "blood_draw.n_edta_2ml","blood_draw.n_edta_2ml_dna","blood_draw.n_fluoride_2ml",
                                                      "blood_draw.n_serum_5ml","Name_of_Examiner", "SL_NO","Serum_Tube_5ml", "Sample_ID",
                                                      "REMARKS_3","Fasting.Non.Fasting","COMPLETE_HAEMOGRAM","introduction_1.institute_name",
                                                      "introduction_1.examiner_name","Consent_for_Biochemistry",
                                                      "Consent_for_DNA","Consent_for_long_term_Storage","Consent_for_Recontact","LIPID_PROFILE__Serum_", "LIVER_FUNCTION_TEST",
                                                      "introduction_1.local_id_manual", "introduction.local_id_barcode",
                                                      "EDTA_Tube_2ml", "EDTA_for_DNA_Isolation",'comments_signature.comments', 'CENTRE_FOR_BRAIN_RESEARCH_IISC_PACKAGE','Factors',
                                                      'Fasting._No_Fasting','LAB', 'COMMUNITY', 'DATE', 'POPULATION','Unique_Barcode_ID','Community')]


addnsamples = addnsamples[,!names(addnsamples) %in% c('Alcohol_Status','City.Village','DIFFERENTIAL_COUNT','Ethnicity',
                                                      'Fluorde_Tube_2ml', 'Income', 'Marital_Status','history_illness.pedigree','Medication',
                                                      'Medium_of_Education','Mother_Tongue','Qualification', 'Same_Caste', 'Smoking_Status',
                                                      'State','State.UT','Tabacco_Status','Population','Collection_Center',"socio_demographics.n_family_member",
                                                      "socio_demographics.n_family_member_18")]
addnsamples = subset(addnsamples, select = -CENTER)
addnsamples = subset(addnsamples, select = -name_dob_1.caste)
#addnsamples = subset(addnsamples, select = -LIPID_PROFILE_Serum)


# Renaming columns --------------------------------------------------------

names(addnsamples)[names(addnsamples) == "Chloride"] = "Cl._mEq.L"
names(addnsamples)[names(addnsamples) == "anthropometry.wasit_cir"] = "anthropometry.waist_cir"

addnsamples$HbA1C_Glycosylated_Haemoglobin = ifelse(is.na(addnsamples$HbA1C_Glycosylated_Haemoglobin),
                                                    addnsamples$GLYCOSYLATED_HAEMOGLOBIN_in_., 
                                                    addnsamples$HbA1C_Glycosylated_Haemoglobin)
addnsamples = subset(addnsamples, select = -GLYCOSYLATED_HAEMOGLOBIN_in_.)


# another round of empty column removal -----------------------------------

emptycolumns = colSums(is.na(addnsamples)|addnsamples=="")==nrow(addnsamples)
addnsamples = addnsamples[ ,!emptycolumns]


# filling in with IGIB and ILSB readins -----------------------------------

addnsamples$WBC_Total_White_Blood_Cell_Count[addnsamples$LocalID %in% IGIB$LocalID] <- IGIB$WBC_Total_White_Blood_Cell_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% IGIB$LocalID], IGIB$LocalID)]
addnsamples$Platelet_Count[addnsamples$LocalID %in% IGIB$LocalID] <- IGIB$Platelet_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% IGIB$LocalID], IGIB$LocalID)]
addnsamples$Absolute_Monocyte_Count[addnsamples$LocalID %in% IGIB$LocalID] = IGIB$Absolute_Monocyte_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% IGIB$LocalID], IGIB$LocalID)]


addnsamples$Absolute_Basophil_Count[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Basophil_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$Absolute_Neutrophil_Count[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Neutrophil_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$Absolute_Eosinophil_Count[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Eosinophil_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$Absolute_Monocyte_Count[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Monocyte_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$Absolute_Lymphocyte_Count[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Lymphocyte_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]

addnsamples$Monocytes[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Monocytes[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$Basophils[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Basophils[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$Eosinophils[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Eosinophils[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$Neutrophils[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Neutrophils[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$Lymphocytes[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Lymphocytes[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]

addnsamples$Platelet_Count[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Platelet_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]

addnsamples$RBC_Red_Blood_Cell_Count[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$RBC_Red_Blood_Cell_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$WBC_Total_White_Blood_Cell_Count[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$WBC_Total_White_Blood_Cell_Count[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$Hematocrit[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$Hematocrit[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$HB_Haemoglobin[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$HB_Haemoglobin[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$MCV_Mean_Corpuscular_Volume[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$MCV_Mean_Corpuscular_Volume[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$MCHC_Mean_Corpuscular_Hb_Concn[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$MCHC_Mean_Corpuscular_Hb_Concn[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$MCH_Mean_Corpuscular_Hb[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$MCH_Mean_Corpuscular_Hb[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$RDW_Red_Cell_Distribution_Width[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$RDW_Red_Cell_Distribution_Width[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
addnsamples$MPV_Mean_Platelet_Volume[addnsamples$LocalID %in% ILSB$LocalID] <- ILSB$MPV_Mean_Platelet_Volume[match(addnsamples$LocalID[addnsamples$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
# Age resolution ----------------------------------------------------------

addnsamples$age = addnsamples$name_dob_1.approx_age
addnsamples$age = ifelse(is.na(addnsamples$age), addnsamples$name_dob_1.age_on_interview,
                              addnsamples$age)
addnsamples$age[addnsamples$age<18] = NA
addnsamples$age_withBBC = addnsamples$age

# date resolution ---------------------------------------------------------

addnsamples$name_dob_1.dob = gsub('/', '-', addnsamples$name_dob_1.dob)
addnsamples$name_dob_1.dob = as.Date(parse_date(addnsamples$name_dob_1.dob))
addnsamples$introduction_1.examination_date = as.Date(parse_date(addnsamples$introduction_1.examination_date))


# New columns for rbind ---------------------------------------------------

addnsamples <- addnsamples[, !colnames(addnsamples) %in% setdiff(colnames(addnsamples), colnames(ft))]

new_cols = c(setdiff(colnames(ft), colnames(addnsamples)))
library(dplyr)
addnsamples = addnsamples %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

addnsamples<-addnsamples[names(ft)]
addnsamples$name_dob_1.dob = as.character(addnsamples$name_dob_1.dob)
addnsamples$introduction_1.examination_date = as.character(addnsamples$introduction_1.examination_date)
#names(addnsamples)[names(addnsamples) == "Immature_granulocytes.1"] = "Immature_granulocytes"

ft = rbind(ft, addnsamples)

# Missingresolved ---------------------------------------------------------
missingresolved = read.table("missingresolved_April4.txt", sep = "~", header = T)

missingresolved$center = gsub("\\/.*","", missingresolved$LocalID)
missingresolved = subset(missingresolved, select = -center.x)
missingresolved = subset(missingresolved, select = -center.y)
missingresolved = missingresolved[-which(missingresolved$center=="SKIM"),]
emptycolumns = colSums(is.na(missingresolved)|missingresolved=="")==nrow(missingresolved)
missingresolved = missingresolved[ ,!emptycolumns]


# Ethnicity cleaning ------------------------------------------------------

missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="female")]='iyengar'
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="agarwal")]='aggarwal'
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="bison horn maria")]='bison_horn_maria'
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="desastha")]="deshastha_brahmin"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="garhwali brahmin")]='garhwali_brahmin'
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="hakki pikiki")]="hakkipikki"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="hakki pikki")]="hakkipikki"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="jat")]='jats'
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="koli")]='kolis'
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="kani kubj brahmin")]="kanyakubj_brahmin"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="kanyakubj")]="kanyakubj_brahmin"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="kokanastha")]="konkonastha_brahmin"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="kurmi mahato")]='kudmi_mahato'
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="lingayats")]='lingayath'
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="odia brahmin")]="oriya_brahmin"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="oriya brahmin")]="oriya_brahmin"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="sandiyala")]="saryuparin_brahmin"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="saryuparin")]="saryuparin_brahmin"
missingresolved$name_dob_1.ethnicity[which(missingresolved$name_dob_1.ethnicity=="vaidiki")]="vaidiki_brahmin"


# region cleaning ---------------------------------------------------------


missingresolved$name_dob_1.state = tolower(missingresolved$name_dob_1.state)
missingresolved$name_dob_1.state[which(missingresolved$name_dob_1.state=='others')] = NA
missingresolved$name_dob_1.other_state = tolower(missingresolved$name_dob_1.other_state)
missingresolved$name_dob_1.other_state[which(missingresolved$name_dob_1.other_state=='none')] = NA
missingresolved$name_dob_1.state = ifelse(is.na(missingresolved$name_dob_1.state),missingresolved$name_dob_1.other_state, missingresolved$name_dob_1.state)

missingresolved = subset(missingresolved, select = -name_dob_1.other_state)
missingresolved$name_dob_1.state[missingresolved$name_dob_1.state=="bangalore, karnataka"] = "karnataka"


missingresolved$region = ifelse(missingresolved$name_dob_1.state %in% c('andra_pradesh', 'karnataka', 'kerala','tamil_nadu', 'telangana'), "South", 
                                ifelse(missingresolved$name_dob_1.state %in% c("arunachal_pradesh","assam", "manipur", "meghalaya", "mizoram"), "North-East", 
                                       ifelse(missingresolved$name_dob_1.state %in% c("bihar", "jharkhand", "orissa", "west_bengal", "sikkim"), "East", 
                                              ifelse(missingresolved$name_dob_1.state %in% c("haryana", "himachal_pradesh", "jammu_kashmir", "punjab", "rajasthan", "Delhi", "delhi", "New Delhi", "new delhi"), "North", 
                                                     ifelse(missingresolved$name_dob_1.state %in% c("gujarat", "maharashtra"), "West",
                                                            ifelse(missingresolved$name_dob_1.state %in% c("madya_pradesh","chhattisgarh", "uttaranchal","uttar_pradesh"), "Central","Unknown"))))))

# removing unnecessary columns --------------------------------------------


#missingresolved = subset(missingresolved, select = -CENTRE_FOR_BRAIN_RESEARCH_IISC_PACKAGE)
missingresolved = subset(missingresolved, select = -NAMES)
missingresolved = subset(missingresolved, select = -NRBC)
#missingresolved = subset(missingresolved, select = -NRBC_percent)
#missingresolved = subset(missingresolved, select = -PWD.)
missingresolved = subset(missingresolved, select = -Gender)
missingresolved = subset(missingresolved, select = -Names_._Matching)
missingresolved = subset(missingresolved, select = -Collection_Center)
#missingresolved = subset(missingresolved, select = -NON.HDL_CHOLESTEROL)
#missingresolved = subset(missingresolved, select = -Calcium)
#missingresolved = subset(missingresolved, select = -BUN.Creatinine_Ratio)
missingresolved = missingresolved[,!names(missingresolved) %in% c('checklists.start', 'checklists.end', 'checklists.today',
                                                                  'checklists.deviceid','checklists.subscriberid','checklists.simserial', 
                                                                  'checklists.phonenumber', 'checklists.intro', 
                                                                  'checklists.checklist_before_socio', 'check_n_ckecklist.incomplete_checklist',
                                                                  'consent.consent_genetics','consent.consent_bloodbiochem_antrophometry',
                                                                  'consent.consent_long_term_storage','consent.consent_recontact',
                                                                  'comments_signature.sign_interviewer','meta.instanceID', 'Names_-_Matching',
                                                                  "blood_draw.complications_blood_draw",
                                                                  "blood_draw.n_edta_2ml","blood_draw.n_edta_2ml_dna","blood_draw.n_fluoride_2ml",
                                                                  "blood_draw.n_serum_5ml","Name_of_Examiner", "SL_NO","Serum_Tube_5ml", "Sample_ID",
                                                                  "REMARKS_3","Fasting.Non.Fasting","COMPLETE_HAEMOGRAM","introduction_1.institute_name",
                                                                  "introduction_1.examiner_name","Consent_for_Biochemistry",
                                                                  "Consent_for_DNA","Consent_for_long_term_Storage","Consent_for_Recontact","LIPID_PROFILE__Serum_", "LIVER_FUNCTION_TEST",
                                                                  "introduction_1.local_id_manual", "introduction.local_id_barcode",
                                                                  "EDTA_Tube_2ml", "EDTA_for_DNA_Isolation",'comments_signature.comments', 'CENTRE_FOR_BRAIN_RESEARCH_IISC_PACKAGE','Factors',
                                                                  'Fasting._No_Fasting','LAB', 'COMMUNITY', 'DATE', 'POPULATION','Unique_Barcode_ID','Community')]


missingresolved = missingresolved[,!names(missingresolved) %in% c('Alcohol_Status','City.Village','DIFFERENTIAL_COUNT','Ethnicity',
                                                                  'Fluorde_Tube_2ml', 'Income', 'Marital_Status','history_illness.pedigree','Medication',
                                                                  'Medium_of_Education','Mother_Tongue','Qualification', 'Same_Caste', 'Smoking_Status',
                                                                  'State','State.UT','Tabacco_Status','Population','Collection_Center',"socio_demographics.n_family_member",
                                                                  "socio_demographics.n_family_member_18")]
#missingresolved = subset(missingresolved, select = -CENTER)
missingresolved = subset(missingresolved, select = -name_dob_1.caste)
#missingresolved = subset(missingresolved, select = -LIPID_PROFILE_Serum)


# Replacing values --------------------------------------------------------
missingresolved$IG_0.0.3 = as.numeric(missingresolved$IG_0.0.3)
missingresolved$Immature_granulocyte_perc = ifelse(is.na(missingresolved$Immature_granulocyte_perc),
                                                   missingresolved$IG_0.0.3, missingresolved$Immature_granulocyte_perc)
missingresolved = subset(missingresolved, select = -IG_0.0.3)

missingresolved$Iron = as.numeric(missingresolved$Iron)
missingresolved$Serum_Iron <- ifelse(is.na(missingresolved$Serum_Iron),missingresolved$Iron,missingresolved$Serum_Iron)
missingresolved = subset(missingresolved, select = -Iron)

missingresolved$Hematocrit = ifelse(is.na(missingresolved$Hematocrit), 
                                    missingresolved$PCV_Packed_Cell_Volume, missingresolved$Hematocrit)
missingresolved = subset(missingresolved, select = -PCV_Packed_Cell_Volume)
missingresolved$Absolute_Lymphocyte_Count = ifelse(is.na(missingresolved$Absolute_Lymphocyte_Count),
                                                   missingresolved$TOTAL_LYMPHOCYTE_COUNT_1500.4000_cells.cu_mm,missingresolved$Absolute_Lymphocyte_Count)
missingresolved = subset(missingresolved, select = -TOTAL_LYMPHOCYTE_COUNT_1500.4000_cells.cu_mm)
missingresolved$A_G_Ratio_Albumin_Globulin= ifelse(is.na(missingresolved$A_G_Ratio_Albumin_Globulin), 
                                                   missingresolved$A.GRatio, missingresolved$A_G_Ratio_Albumin_Globulin)
missingresolved = subset(missingresolved, select = -A.GRatio)
missingresolved$A_G_Ratio_Albumin_Globulin= ifelse(is.na(missingresolved$A_G_Ratio_Albumin_Globulin), 
                                                   missingresolved$A_G_Ratio, missingresolved$A_G_Ratio_Albumin_Globulin)
missingresolved = subset(missingresolved, select = -A_G_Ratio)
missingresolved$RDW_Red_Cell_Distribution_Width = ifelse(is.na(missingresolved$RDW_Red_Cell_Distribution_Width), 
                                                         missingresolved$RDW, missingresolved$RDW_Red_Cell_Distribution_Width)
missingresolved = subset(missingresolved, select = -RDW)
missingresolved$Immature_granulocyte_perc = ifelse(is.na(missingresolved$Immature_granulocyte_perc), 
                                                   missingresolved$Immature_granulocyte, missingresolved$Immature_granulocyte_perc)
missingresolved = subset(missingresolved, select = -Immature_granulocyte)
missingresolved$Cl._mEq.L = ifelse(is.na(missingresolved$Cl._mEq.L), missingresolved$Chloride, missingresolved$Cl._mEq.L)
missingresolved = subset(missingresolved, select = -Chloride)
missingresolved$Estimated_Average_Glucose = ifelse(is.na(missingresolved$Estimated_Average_Glucose), missingresolved$MEAN_BLOOD_GLUCOSE_mg.dl, missingresolved$Estimated_Average_Glucose)
missingresolved = subset(missingresolved, select = -MEAN_BLOOD_GLUCOSE_mg.dl)
missingresolved$MCH_Mean_Corpuscular_Hb = ifelse(is.na(missingresolved$MCH_Mean_Corpuscular_Hb), missingresolved$MCH, missingresolved$MCH_Mean_Corpuscular_Hb)
missingresolved = subset(missingresolved, select = -MCH)
missingresolved$MCH_Mean_Corpuscular_Hb = ifelse(is.na(missingresolved$MCH_Mean_Corpuscular_Hb), missingresolved$MCH_MeanCorpuscularHb, missingresolved$MCH_Mean_Corpuscular_Hb)
missingresolved = subset(missingresolved, select = -MCH_MeanCorpuscularHb)
missingresolved$MCHC_Mean_Corpuscular_Hb_Concn = ifelse(is.na(missingresolved$MCHC_Mean_Corpuscular_Hb_Concn), missingresolved$MCHC, missingresolved$MCHC_Mean_Corpuscular_Hb_Concn)
missingresolved = subset(missingresolved, select = -MCHC)
missingresolved$MCHC_Mean_Corpuscular_Hb_Concn = ifelse(is.na(missingresolved$MCHC_Mean_Corpuscular_Hb_Concn), missingresolved$MCHC_MeanCorpuscularHbConcn., missingresolved$MCHC_Mean_Corpuscular_Hb_Concn)
missingresolved = subset(missingresolved, select = -MCHC_MeanCorpuscularHbConcn.)
missingresolved$MCV_Mean_Corpuscular_Volume = ifelse(is.na(missingresolved$MCV_Mean_Corpuscular_Volume), missingresolved$MCV_MeanCorpuscularVolume, missingresolved$MCV_Mean_Corpuscular_Volume)
missingresolved = subset(missingresolved, select = -MCV_MeanCorpuscularVolume)
missingresolved$MCV_Mean_Corpuscular_Volume = ifelse(is.na(missingresolved$MCV_Mean_Corpuscular_Volume), missingresolved$MCV, missingresolved$MCV_Mean_Corpuscular_Volume)
missingresolved = subset(missingresolved, select = -MCV)
missingresolved$Absolute_Basophil_Count = ifelse(is.na(missingresolved$Absolute_Basophil_Count), 
                                                 missingresolved$AbsoluteBasophilCount, missingresolved$Absolute_Basophil_Count)
missingresolved = subset(missingresolved, select = -AbsoluteBasophilCount)
missingresolved$Absolute_Eosinophil_Count = ifelse(is.na(missingresolved$Absolute_Eosinophil_Count), 
                                                   missingresolved$AbsoluteEosinophilCount, missingresolved$Absolute_Eosinophil_Count)
missingresolved = subset(missingresolved, select = -AbsoluteEosinophilCount)
missingresolved$Absolute_Neutrophil_Count = ifelse(is.na(missingresolved$Absolute_Neutrophil_Count), 
                                                   missingresolved$AbsoluteNeutrophilsCount, missingresolved$Absolute_Neutrophil_Count)
missingresolved = subset(missingresolved, select = -AbsoluteNeutrophilsCount)
missingresolved$Absolute_Lymphocyte_Count = ifelse(is.na(missingresolved$Absolute_Lymphocyte_Count), 
                                                   missingresolved$AbsoluteLymphocyteCount, missingresolved$Absolute_Lymphocyte_Count)
missingresolved = subset(missingresolved, select = -AbsoluteLymphocyteCount)
missingresolved$Absolute_Monocyte_Count = ifelse(is.na(missingresolved$Absolute_Monocyte_Count), 
                                                 missingresolved$AbsoluteMonocyteCount, missingresolved$Absolute_Monocyte_Count)
missingresolved = subset(missingresolved, select = -AbsoluteMonocyteCount)
missingresolved$Alkaline_Phosphatase = ifelse(is.na(missingresolved$Alkaline_Phosphatase), 
                                              missingresolved$AlkalinePhosphatase, missingresolved$Alkaline_Phosphatase)
missingresolved = subset(missingresolved, select = -AlkalinePhosphatase)
missingresolved$Gamma_GT_GGTP = ifelse(is.na(missingresolved$Gamma_GT_GGTP), 
                                       missingresolved$GammaGT_GGTP, missingresolved$Gamma_GT_GGTP)
missingresolved = subset(missingresolved, select = -GammaGT_GGTP)
missingresolved$SGOT_SGPT = ifelse(is.na(missingresolved$SGOT_SGPT), 
                                   missingresolved$SGOT.SGPT, missingresolved$SGOT_SGPT)
missingresolved = subset(missingresolved, select = -SGOT.SGPT)
missingresolved$CHOL_HDL_Ratio = ifelse(is.na(missingresolved$CHOL_HDL_Ratio), 
                                        missingresolved$CHOL.HDLRATIO, missingresolved$CHOL_HDL_Ratio)
missingresolved = subset(missingresolved, select = -CHOL.HDLRATIO)
missingresolved$ESR = ifelse(is.na(missingresolved$ESR),missingresolved$ESR.ErythrocyteSedimentationRate.EDTA, missingresolved$ESR)
missingresolved = subset(missingresolved, select = -ESR.ErythrocyteSedimentationRate.EDTA)
missingresolved$Estimated_Average_Glucose = ifelse(is.na(missingresolved$Estimated_Average_Glucose),
                                                   missingresolved$EstimatedAverageGlucose_eAG, missingresolved$Estimated_Average_Glucose)
missingresolved = subset(missingresolved, select = -EstimatedAverageGlucose_eAG)
missingresolved$HbA1C_Glycosylated_Haemoglobin = ifelse(is.na(missingresolved$HbA1C_Glycosylated_Haemoglobin),
                                                        missingresolved$HbA1C.GlycatedHaemoglobin, missingresolved$HbA1C_Glycosylated_Haemoglobin)
missingresolved = subset(missingresolved, select = -HbA1C.GlycatedHaemoglobin)
# missingresolved$HbA1C_Glycosylated_Haemoglobin = ifelse(is.na(missingresolved$HbA1C_Glycosylated_Haemoglobin),
#                                            missingresolved$GLYCOSYLATED_Hb_IFCC, missingresolved$HbA1C_Glycosylated_Haemoglobin)
missingresolved$HbA1C_Glycosylated_Haemoglobin = ifelse(is.na(missingresolved$HbA1C_Glycosylated_Haemoglobin),
                                                        missingresolved$GLYCOSYLATED_HAEMOGLOBIN_in_., missingresolved$HbA1C_Glycosylated_Haemoglobin)
missingresolved = subset(missingresolved, select = -GLYCOSYLATED_HAEMOGLOBIN_in_.)
missingresolved$HbA1C_Glycosylated_Haemoglobin = ifelse(is.na(missingresolved$HbA1C_Glycosylated_Haemoglobin),
                                                        missingresolved$HbA1c, missingresolved$HbA1C_Glycosylated_Haemoglobin)
missingresolved = subset(missingresolved, select = -HbA1c)

#missingresolved$HB_Haemoglobin = ifelse(is.na(missingresolved$HB_Haemoglobin),missingresolved$Haemoglobin_Hb, missingresolved$HB_Haemoglobin)
missingresolved$HB_Haemoglobin = ifelse(is.na(missingresolved$HB_Haemoglobin),missingresolved$Hb_Hemoglobin, missingresolved$HB_Haemoglobin)
missingresolved = subset(missingresolved, select = -Hb_Hemoglobin)
missingresolved$LDL = ifelse(is.na(missingresolved$LDL),missingresolved$LDLCholesterol, missingresolved$LDL)
missingresolved = subset(missingresolved, select = -LDLCholesterol)
missingresolved$HDL = ifelse(is.na(missingresolved$HDL),missingresolved$HDLCholesterol, missingresolved$HDL)
missingresolved = subset(missingresolved, select = -HDLCholesterol)
missingresolved$VLDL = ifelse(is.na(missingresolved$VLDL),missingresolved$VLDLCholesterol, missingresolved$VLDL)
missingresolved = subset(missingresolved, select = -VLDLCholesterol)
missingresolved$FBS_Fasting_Blood_Glucose = ifelse(is.na(missingresolved$FBS_Fasting_Blood_Glucose),
                                                   missingresolved$Glucosefasting, missingresolved$FBS_Fasting_Blood_Glucose)
missingresolved = subset(missingresolved, select = -Glucosefasting)
missingresolved$Platelet_Count = ifelse(is.na(missingresolved$Platelet_Count),missingresolved$Plateletcount, missingresolved$Platelet_Count)
missingresolved = subset(missingresolved, select = -Plateletcount)
#missingresolved$RBS = ifelse(is.na(missingresolved$RBS),missingresolved$GLUCOSE._RANDOM_R_._PLASMA, missingresolved$RBS)
missingresolved$Urea = ifelse(is.na(missingresolved$Urea),missingresolved$UreaSerum, missingresolved$Urea)
missingresolved = subset(missingresolved, select = -UreaSerum)
#missingresolved$MPV_Mean_Platelet_Volume = ifelse(is.na(missingresolved$MPV_Mean_Platelet_Volume), missingresolved$MPV, missingresolved$MPV_Mean_Platelet_Volume)
missingresolved$MPV_Mean_Platelet_Volume = ifelse(is.na(missingresolved$MPV_Mean_Platelet_Volume), missingresolved$MPV_MeanPlateletVolume, missingresolved$MPV_Mean_Platelet_Volume)
missingresolved = subset(missingresolved, select = -MPV_MeanPlateletVolume)
missingresolved$WBC_Total_White_Blood_Cell_Count = ifelse(is.na(missingresolved$WBC_Total_White_Blood_Cell_Count),
                                                          missingresolved$TotalLeucocytes_WBC_count,missingresolved$WBC_Total_White_Blood_Cell_Count)
missingresolved = subset(missingresolved, select = -TotalLeucocytes_WBC_count)
#missingresolved$NonHDL_Cholesterol = ifelse(is.na(missingresolved$NonHDL_Cholesterol), missingresolved$nonHDL, missingresolved$NonHDL_Cholesterol)
missingresolved$NonHDL_Cholesterol = ifelse(is.na(missingresolved$NonHDL_Cholesterol), missingresolved$NonHDLCholesterol, missingresolved$NonHDL_Cholesterol)
missingresolved = subset(missingresolved, select = -NonHDLCholesterol)
#missingresolved$Hematocrit = ifelse(is.na(missingresolved$Hematocrit), missingresolved$PCV_., missingresolved$Hematocrit)
#missingresolved$Hematocrit = ifelse(is.na(missingresolved$Hematocrit), missingresolved$HCT, missingresolved$Hematocrit)
missingresolved$Hematocrit = ifelse(is.na(missingresolved$Hematocrit), missingresolved$PCV_PackedCellVolume, missingresolved$Hematocrit)
missingresolved = subset(missingresolved, select = -PCV_PackedCellVolume)
missingresolved$Absolute_Basophil_Count = ifelse(is.na(missingresolved$Absolute_Basophil_Count), missingresolved$BASO...18, missingresolved$Absolute_Basophil_Count)
missingresolved = subset(missingresolved, select = -BASO...18)
missingresolved$Absolute_Basophil_Count = ifelse(is.na(missingresolved$Absolute_Basophil_Count), missingresolved$BASO...23, missingresolved$Absolute_Basophil_Count)
missingresolved = subset(missingresolved, select = -BASO...23)
missingresolved$Absolute_Eosinophil_Count = ifelse(is.na(missingresolved$Absolute_Eosinophil_Count), missingresolved$EO...17, missingresolved$Absolute_Eosinophil_Count)
missingresolved = subset(missingresolved, select = -EO...17)
missingresolved$Absolute_Eosinophil_Count = ifelse(is.na(missingresolved$Absolute_Eosinophil_Count), missingresolved$EO...22, missingresolved$Absolute_Eosinophil_Count)
missingresolved = subset(missingresolved, select = -EO...22)
missingresolved$Absolute_Lymphocyte_Count = ifelse(is.na(missingresolved$Absolute_Lymphocyte_Count), missingresolved$LYMPH...20, missingresolved$Absolute_Lymphocyte_Count)
missingresolved = subset(missingresolved, select = -LYMPH...20)
missingresolved$Absolute_Lymphocyte_Count = ifelse(is.na(missingresolved$Absolute_Lymphocyte_Count), missingresolved$LYMPH...15, missingresolved$Absolute_Lymphocyte_Count)
missingresolved = subset(missingresolved, select = -LYMPH...15)
missingresolved$Absolute_Monocyte_Count = ifelse(is.na(missingresolved$Absolute_Monocyte_Count), missingresolved$MONO...16, missingresolved$Absolute_Monocyte_Count)
missingresolved = subset(missingresolved, select = -MONO...16)
missingresolved$Absolute_Monocyte_Count = ifelse(is.na(missingresolved$Absolute_Monocyte_Count), missingresolved$MONO...21, missingresolved$Absolute_Monocyte_Count)
missingresolved = subset(missingresolved, select = -MONO...21)
missingresolved$Absolute_Neutrophil_Count = ifelse(is.na(missingresolved$Absolute_Neutrophil_Count), missingresolved$NEUT...19, missingresolved$Absolute_Neutrophil_Count)
missingresolved = subset(missingresolved, select = -NEUT...19)
missingresolved$Absolute_Neutrophil_Count = ifelse(is.na(missingresolved$Absolute_Neutrophil_Count), missingresolved$NEUT...14, missingresolved$Absolute_Neutrophil_Count)
missingresolved = subset(missingresolved, select = -NEUT...14)
#missingresolved$Indirect_Bilirubin = ifelse(is.na(missingresolved$Indirect_Bilirubin), missingresolved$Bilirubin.Indirect, missingresolved$Indirect_Bilirubin)
#missingresolved$CRP = ifelse(is.na(missingresolved$CRP), missingresolved$CRP_mg.L, missingresolved$CRP)
missingresolved$LDL_HDL_Ratio = ifelse(is.na(missingresolved$LDL_HDL_Ratio), missingresolved$LDL.HDLRATIO, missingresolved$LDL_HDL_Ratio)
missingresolved = subset(missingresolved, select = -LDL.HDLRATIO)
#missingresolved$NLR_4 = ifelse(is.na(missingresolved$NLR_4), missingresolved$Neutrophils_Lymphocyte_Ratio, missingresolved$NLR_4)
missingresolved$T3_Total = ifelse(is.na(missingresolved$T3_Total), missingresolved$T3, missingresolved$T3_Total)
missingresolved = subset(missingresolved, select = -T3)
missingresolved$T4_Total = ifelse(is.na(missingresolved$T4_Total), missingresolved$T4, missingresolved$T4_Total)
missingresolved = subset(missingresolved, select = -T4)
missingresolved$AST_SGOT = ifelse(is.na(missingresolved$AST_SGOT), missingresolved$SGOT_AST, missingresolved$AST_SGOT)
missingresolved = subset(missingresolved, select = -SGOT_AST)
missingresolved$ALT_SGPT = ifelse(is.na(missingresolved$ALT_SGPT), missingresolved$SGPT_ALT, missingresolved$ALT_SGPT)
missingresolved = subset(missingresolved, select = -SGPT_ALT)
missingresolved$Protein = ifelse(is.na(missingresolved$Protein), missingresolved$TotalProtein, missingresolved$Protein)
missingresolved = subset(missingresolved, select = -TotalProtein)
#missingresolved$Total_Calcium = ifelse(is.na(missingresolved$Total_Calcium), missingresolved$Calcium, missingresolved$Total_Calcium)
missingresolved$RDW_Red_Cell_Distribution_Width = ifelse(is.na(missingresolved$RDW_Red_Cell_Distribution_Width), missingresolved$RDW_RedCellDistributionWidth, missingresolved$RDW_Red_Cell_Distribution_Width)
missingresolved = subset(missingresolved, select = -RDW_RedCellDistributionWidth)
missingresolved$RBC_Red_Blood_Cell_Count = ifelse(is.na(missingresolved$RBC_Red_Blood_Cell_Count), missingresolved$Erythrocyte_RBC_Count, missingresolved$RBC_Red_Blood_Cell_Count)
missingresolved = subset(missingresolved, select = -Erythrocyte_RBC_Count)
#missingresolved$Granulocyte_count = ifelse(is.na(missingresolved$Granulocyte_count), missingresolved$TOTAL_GRANULOCYTE_COUNT_cu_mm, missingresolved$Granulocyte_count)
#missingresolved$Estimated_Average_Glucose = ifelse(is.na(missingresolved$Estimated_Average_Glucose), missingresolved$Estimated_average_glucose_eAG, missingresolved$Estimated_Average_Glucose)
#missingresolved$Vitamin_D_25_Hydroxy = ifelse(is.na(missingresolved$Vitamin_D_25_Hydroxy), missingresolved$Vitamin_D, missingresolved$Vitamin_D_25_Hydroxy)
missingresolved$LDL = ifelse(is.na(missingresolved$LDL), missingresolved$LDL_Direct, missingresolved$LDL)
missingresolved = subset(missingresolved, select = -LDL_Direct)
#missingresolved$Hematocrit = ifelse(is.na(missingresolved$Hematocrit), missingresolved$PCB, missingresolved$Hematocrit)
missingresolved$Platelet_Count = ifelse(is.na(missingresolved$Platelet_Count), missingresolved$Platelet_Counts, missingresolved$Platelet_Count)
missingresolved = subset(missingresolved, select = -Platelet_Counts)

missingresolved$FBS_Fasting_Blood_Glucose = ifelse(is.na(missingresolved$FBS_Fasting_Blood_Glucose),
                                                   missingresolved$FBS, missingresolved$FBS_Fasting_Blood_Glucose)
missingresolved = subset(missingresolved, select = -FBS)

missingresolved$Total_Bilirubin = ifelse(is.na(missingresolved$Total_Bilirubin),
                                         missingresolved$Bilirubin, missingresolved$Total_Bilirubin)
missingresolved = subset(missingresolved, select = -Bilirubin)

missingresolved$Indirect_Bilirubin = ifelse(is.na(missingresolved$Indirect_Bilirubin),
                                         missingresolved$Bilirubin.Indirect, missingresolved$Indirect_Bilirubin)
missingresolved = subset(missingresolved, select = -Bilirubin.Indirect)


missingresolved = subset(missingresolved, select = -X.TSA)

#missingresolved = subset(missingresolved, select = -center.x)
#missingresolved = subset(missingresolved, select = -center.y)
colnames(missingresolved)[colnames(missingresolved) == "NRBC."] ="NRBC_percent"
colnames(missingresolved)[colnames(missingresolved) == "anthropometry.wasit_cir"] ="anthropometry.waist_cir"
names(missingresolved)[names(missingresolved) == "Vitamin_D"] <- "Vitamin_D_25_Hydroxy"
names(missingresolved)[names(missingresolved) == "Homocysteine"] <- "Homocysteine_Levels"
names(missingresolved)[names(missingresolved) == "Insulin"] <- "Fasting_Insulin_Level"
names(missingresolved)[names(missingresolved) == "C_Peptide"] <- "C.Peptide"
names(missingresolved)[names(missingresolved) == "Transferrin"] <- "Transferrin_Saturation"
names(missingresolved)[names(missingresolved) == "Neutrophils_Lymphocyte_Ratio"] <- "NLR_4"



missingresolved = subset(missingresolved, select = -...66)
missingresolved = subset(missingresolved, select = -PARASITE)
missingresolved = subset(missingresolved, select = -MalariaParasiteexaminationonthinSmear)
missingresolved = subset(missingresolved, select = -MalariaParasiteexaminationonthickSmear)

names(missingresolved)[names(missingresolved) == "Vitamin_D"] <- "Vitamin_D_25_Hydroxy"
names(missingresolved)[names(missingresolved) == "Homocysteine"] <- "Homocysteine_Levels"
names(missingresolved)[names(missingresolved) == "Insulin"] <- "Fasting_Insulin_Level"
names(missingresolved)[names(missingresolved) == "C_Peptide"] <- "C.Peptide"

missingresolved$WBC_Total_White_Blood_Cell_Count[missingresolved$LocalID %in% IGIB$LocalID] <- IGIB$WBC_Total_White_Blood_Cell_Count[match(missingresolved$LocalID[missingresolved$LocalID %in% IGIB$LocalID], IGIB$LocalID)]
missingresolved$Platelet_Count[missingresolved$LocalID %in% IGIB$LocalID] <- IGIB$Platelet_Count[match(missingresolved$LocalID[missingresolved$LocalID %in% IGIB$LocalID], IGIB$LocalID)]
missingresolved$Absolute_Monocyte_Count[missingresolved$LocalID %in% IGIB$LocalID] = IGIB$Absolute_Monocyte_Count[match(missingresolved$LocalID[missingresolved$LocalID %in% IGIB$LocalID], IGIB$LocalID)]

missingresolved$Absolute_Basophil_Count[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Basophil_Count[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
missingresolved$Absolute_Neutrophil_Count[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Neutrophil_Count[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
missingresolved$Absolute_Eosinophil_Count[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Eosinophil_Count[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
missingresolved$Absolute_Monocyte_Count[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Monocyte_Count[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
missingresolved$Absolute_Lymphocyte_Count[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Absolute_Lymphocyte_Count[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]

missingresolved$Monocytes[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Monocytes[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
missingresolved$Basophils[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Basophils[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
missingresolved$Eosinophils[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Eosinophils[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
missingresolved$Neutrophils[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Neutrophils[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]
missingresolved$Lymphocytes[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Lymphocytes[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]

missingresolved$Platelet_Count[missingresolved$LocalID %in% ILSB$LocalID] <- ILSB$Platelet_Count[match(missingresolved$LocalID[missingresolved$LocalID %in% ILSB$LocalID], ILSB$LocalID)]

# Age resolution ----------------------------------------------------------

missingresolved$age = missingresolved$name_dob_1.approx_age
missingresolved$age = ifelse(is.na(missingresolved$age), missingresolved$name_dob_1.age_on_interview,
                                  missingresolved$age)
missingresolved <- missingresolved %>% 
  mutate(age = ifelse(is.na(age) & name_dob_1.year_of_birth < 100, name_dob_1.year_of_birth, age))
missingresolved$age[missingresolved$age<18] = NA
missingresolved$age_withBBC = missingresolved$age #no missing


# date resolution ---------------------------------------------------------

missingresolved$name_dob_1.dob = gsub('/', '-', missingresolved$name_dob_1.dob)
missingresolved$name_dob_1.dob = as.Date(parse_date(missingresolved$name_dob_1.dob))
missingresolved$introduction_1.examination_date = as.Date(parse_date(missingresolved$introduction_1.examination_date))

# New columns for rbind ---------------------------------------------------


missingresolved <- missingresolved[, !colnames(missingresolved) %in% setdiff(colnames(missingresolved), colnames(ft))]
new_cols = c(setdiff(colnames(ft), colnames(missingresolved)))
library(dplyr)
missingresolved = missingresolved %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

missingresolved<-missingresolved[names(ft)]
#names(missingresolved)[names(missingresolved) == "Immature_granulocytes.1"] = "Immature_granulocytes"
missingresolved$name_dob_1.dob = as.character(missingresolved$name_dob_1.dob)
missingresolved$introduction_1.examination_date = as.character(missingresolved$introduction_1.examination_date)
ft = rbind(ft, missingresolved)

# RGCB missing HbA1c ------------------------------------------------------
RGCB_new = read_xlsx("RGCB-L-025001 to 154.xlsx")
RGCB_new$LocalID = paste0("RGCB/L/0", RGCB_new$`Barcode Number`)
names(RGCB_new)[names(RGCB_new) == "HbA1c \r\n( %)\r\n4.0 - 6.5"] = "HbA1C_Glycosylated_Haemoglobin"
ft$HbA1C_Glycosylated_Haemoglobin[ft$LocalID %in% RGCB_new$LocalID] = 
  RGCB_new$HbA1C_Glycosylated_Haemoglobin[match(ft$LocalID[ft$LocalID %in% RGCB_new$LocalID], RGCB_new$LocalID)]

# New data CCMB -----------------------------------------------------------

CCMB_BBC = read_xlsx("BBC DATA_CCMB.xlsx")
CCMB_ODK = read_xlsx("ODK DATA_CCMB.xlsx")

CCMB_BBC = subset(CCMB_BBC, select = -Name)
CCMB_BBC = subset(CCMB_BBC, select = -Community)
CCMB_BBC = subset(CCMB_BBC, select = -Gender)


# Renaming BBC columns ----------------------------------------------------

names(CCMB_BBC)[names(CCMB_BBC)=="Local ID"] = "LocalID"
names(CCMB_BBC)[names(CCMB_BBC)=="Total Bilirubin"] = "Total_Bilirubin"
names(CCMB_BBC)[names(CCMB_BBC)=="Direct Bilirubin"] = "Direct_Bilirubin"
names(CCMB_BBC)[names(CCMB_BBC)=="Bilirubin Indirect"] = "Indirect_Bilirubin"
names(CCMB_BBC)[names(CCMB_BBC)=="SGPT"] = "ALT_SGPT"
names(CCMB_BBC)[names(CCMB_BBC)=="SGOT"] = "AST_SGOT"
names(CCMB_BBC)[names(CCMB_BBC)=="Alkaline Phosphatase"] = "Alkaline_Phosphatase"
names(CCMB_BBC)[names(CCMB_BBC)=="Total Protein"] = "Protein"
names(CCMB_BBC)[names(CCMB_BBC)=="TGL"] = "Triglycerides"
names(CCMB_BBC)[names(CCMB_BBC)=="Uric Acid"] = "Uric_Acid"
names(CCMB_BBC)[names(CCMB_BBC)=="HbA1C"] = "HbA1C_Glycosylated_Haemoglobin"
names(CCMB_BBC)[names(CCMB_BBC)=="Hemoglobin"] = "HB_Haemoglobin"
names(CCMB_BBC)[names(CCMB_BBC)=="WBC"] = "WBC_Total_White_Blood_Cell_Count"
names(CCMB_BBC)[names(CCMB_BBC)=="RBC"] = "RBC_Red_Blood_Cell_Count"
names(CCMB_BBC)[names(CCMB_BBC)=="Platelet"] = "Platelet_Count"
names(CCMB_BBC)[names(CCMB_BBC)=="MCV"] = "MCV_Mean_Corpuscular_Volume"
names(CCMB_BBC)[names(CCMB_BBC)=="MCH"] = "MCH_Mean_Corpuscular_Hb"
names(CCMB_BBC)[names(CCMB_BBC)=="MCHC"] = "MCHC_Mean_Corpuscular_Hb_Concn"


# Renaming ODK columns ----------------------------------------------------
names(CCMB_ODK)[names(CCMB_ODK)=="LOCAL ID"] = "LocalID"
names(CCMB_ODK)[names(CCMB_ODK)=="CO-ORDINATING CENTRE"] = "center"
names(CCMB_ODK)[names(CCMB_ODK)=="DATE OF EXAMINATION"] = "introduction_1.examination_date"
names(CCMB_ODK)[names(CCMB_ODK)=="STATE"] = "name_dob_1.state"
names(CCMB_ODK)[names(CCMB_ODK)=="VILLAGE"] = "name_dob_1.village"
names(CCMB_ODK)[names(CCMB_ODK)=="DOB"] = "name_dob_1.dob"
names(CCMB_ODK)[names(CCMB_ODK)=="AGE"] = "name_dob_1.approx_age"
names(CCMB_ODK)[names(CCMB_ODK)=="MOTHER TONGUE"] = "name_dob_1.mother_tongue"
names(CCMB_ODK)[names(CCMB_ODK)=="GENDER"] = "name_dob_1.gender"
names(CCMB_ODK)[names(CCMB_ODK)=="CASTE/COMMUNITY"] = "name_dob_1.ethnicity"
names(CCMB_ODK)[names(CCMB_ODK)=="MARITAL STATUS"] = "socio_demographics.marital_status"
names(CCMB_ODK)[names(CCMB_ODK)=="EDUCATION"] = "socio_demographics.highest_edu"
names(CCMB_ODK)[names(CCMB_ODK)=="MEDICATION TAKING CURRENTLY"] = "history_illness.medication_currently_status"
names(CCMB_ODK)[names(CCMB_ODK)=="SYS BP"] = "anthropometry.sys_bp"
names(CCMB_ODK)[names(CCMB_ODK)=="DIASTOLIC BP"] = "anthropometry.dia_bp"
names(CCMB_ODK)[names(CCMB_ODK)=="HEAD CIRCUM."] = "anthropometry.head_cir"
names(CCMB_ODK)[names(CCMB_ODK)=="HEIGHT"] = "anthropometry.height"
names(CCMB_ODK)[names(CCMB_ODK)=="WEIGHT"] = "anthropometry.weight"
names(CCMB_ODK)[names(CCMB_ODK)=="WAIST CIRCUM."] = "anthropometry.waist_cir"
names(CCMB_ODK)[names(CCMB_ODK)=="HIP CIRCUM."] = "anthropometry.hip_cir"
names(CCMB_ODK)[names(CCMB_ODK)=="BLOOD GLUCOSE"] = "anthropometry.glucose_mg_dl"
names(CCMB_ODK)[names(CCMB_ODK)=="BLOOD DRAW UNDER FASTING OR NOT"] = "blood_draw.Blood_draw_fasting"



# Joined dataset ----------------------------------------------------------

CCMB_joined <- merge(CCMB_BBC, CCMB_ODK, by.x = "LocalID", 
                     by.y = "LocalID", all.x = TRUE, all.y = TRUE)

CCMB_joined$age = CCMB_joined$name_dob_1.approx_age
CCMB_joined$age_withBBC = CCMB_joined$age

CCMB_joined$introduction_1.examination_date = as.Date(CCMB_joined$introduction_1.examination_date)
CCMB_joined$name_dob_1.dob = gsub("/","-",CCMB_joined$name_dob_1.dob)
CCMB_joined$name_dob_1.dob[CCMB_joined$name_dob_1.dob=="15-4-1958"] = "1958-04-15"
CCMB_joined$name_dob_1.dob[CCMB_joined$name_dob_1.dob=="27-1-1990"] = "1990-01-27"
CCMB_joined$name_dob_1.dob[CCMB_joined$name_dob_1.dob=="26333"] = "1972-01-03"

CCMB_joined$name_dob_1.dob = as.Date(parse_date(CCMB_joined$name_dob_1.dob))

CCMB_joined$name_dob_1.ethnicity[CCMB_joined$name_dob_1.ethnicity=="THARU"] = "tharu"
CCMB_joined$name_dob_1.ethnicity[CCMB_joined$name_dob_1.ethnicity=="VAIDIKI BRAHMIN"] = "vaidiki_brahmin"

CCMB_joined$name_dob_1.state[CCMB_joined$name_dob_1.state=="TELANGANA"] = "telangana"
CCMB_joined$name_dob_1.state[CCMB_joined$name_dob_1.state=="UTTAR PRADESH"] = "uttar_pradesh"

CCMB_joined$region[CCMB_joined$name_dob_1.state=="telangana"] = "South"
CCMB_joined$region[CCMB_joined$name_dob_1.state=="uttar_pradesh"] = "Central"

CCMB_joined <- CCMB_joined[, !colnames(CCMB_joined) %in% setdiff(colnames(CCMB_joined), colnames(ft))]

new_cols = c(setdiff(colnames(ft), colnames(CCMB_joined)))
library(dplyr)
CCMB_joined = CCMB_joined %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

CCMB_joined<-CCMB_joined[names(ft)]
CCMB_joined$introduction_1.examination_date = as.character(CCMB_joined$introduction_1.examination_date)
CCMB_joined$name_dob_1.dob = as.character(CCMB_joined$name_dob_1.dob)
ft = rbind(ft, CCMB_joined)

# Convert to numeric again ------------------------------------------------

ft$FBS_Fasting_Blood_Glucose = as.numeric(ft$FBS_Fasting_Blood_Glucose)
ft$HbA1C_Glycosylated_Haemoglobin = as.numeric(ft$HbA1C_Glycosylated_Haemoglobin)
#ft$HbA1C_Glycosylated_Haemoglobin[ft$HbA1C_Glycosylated_Haemoglobin == 0] = NA
#merged_df_ft1 = read.table("Added_addnlt_Apr19.txt", sep = "\t", header = T, fill= T, encoding = 'latin1')
ft$Urea = as.numeric(ft$Urea)
ft$Creatinine = as.numeric(ft$Creatinine)
ft$Total_Bilirubin = as.numeric(ft$Total_Bilirubin)
ft$ALT_SGPT = as.numeric(ft$ALT_SGPT)
ft$AST_SGOT = as.numeric(ft$AST_SGOT)
ft$Alkaline_Phosphatase = as.numeric(ft$Alkaline_Phosphatase)
ft$Cholesterol = as.numeric(ft$Cholesterol)
ft$Triglycerides = as.numeric(ft$Triglycerides)
ft$HDL = as.numeric(ft$HDL)
ft$LDL = as.numeric(ft$LDL)
ft$T3_Total = as.numeric(ft$T3_Total)
ft$T4_Total = as.numeric(ft$T4_Total)
ft$TSH = as.numeric(ft$TSH)
ft$Homocysteine_Levels = as.numeric(ft$Homocysteine_Levels)
ft$Vitamin_B12 = as.numeric(ft$Vitamin_B12)
ft$Folic_Acid = as.numeric(ft$Folic_Acid)
ft$Fasting_Insulin_Level = as.numeric(ft$Fasting_Insulin_Level)
ft$C.Peptide = as.numeric(ft$C.Peptide)
ft$HB_Haemoglobin = as.numeric(ft$HB_Haemoglobin)
ft$RBC_Red_Blood_Cell_Count = as.numeric(ft$RBC_Red_Blood_Cell_Count) #this is probably in millions
ft$MCH_Mean_Corpuscular_Hb = as.numeric(ft$MCH_Mean_Corpuscular_Hb)
ft$MCHC_Mean_Corpuscular_Hb_Concn = as.numeric(ft$MCHC_Mean_Corpuscular_Hb_Concn)
ft$MCV_Mean_Corpuscular_Volume = as.numeric(ft$MCV_Mean_Corpuscular_Volume)
ft$RDW_Red_Cell_Distribution_Width = as.numeric(ft$RDW_Red_Cell_Distribution_Width)
ft$WBC_Total_White_Blood_Cell_Count = as.numeric(ft$WBC_Total_White_Blood_Cell_Count)
ft$Basophils = as.numeric(ft$Basophils)
ft$Eosinophils = as.numeric(ft$Eosinophils)
ft$Lymphocytes = as.numeric(ft$Lymphocytes)
ft$Monocytes = as.numeric(ft$Monocytes)
ft$Neutrophils = as.numeric(ft$Neutrophils)
ft$Platelet_Count = as.numeric(ft$Platelet_Count)
ft$A_G_Ratio_Albumin_Globulin = as.numeric(ft$A_G_Ratio_Albumin_Globulin)
ft$Absolute_Neutrophil_Count = as.numeric(ft$Absolute_Neutrophil_Count)
ft$Absolute_Basophil_Count = as.numeric(ft$Absolute_Basophil_Count)
ft$Absolute_Eosinophil_Count = as.numeric(ft$Absolute_Eosinophil_Count)
ft$Absolute_Lymphocyte_Count = as.numeric(ft$Absolute_Lymphocyte_Count)
ft$Absolute_Monocyte_Count = as.numeric(ft$Absolute_Monocyte_Count)
ft$CHOL_HDL_Ratio = as.numeric(ft$CHOL_HDL_Ratio)
ft$Direct_Bilirubin = as.numeric(ft$Direct_Bilirubin)
ft$ESR = as.numeric(ft$ESR)
ft$Estimated_Average_Glucose = as.numeric(ft$Estimated_Average_Glucose)
ft$Gamma_GT_GGTP = as.numeric(ft$Gamma_GT_GGTP)
ft$Globulin = as.numeric(ft$Globulin)
ft$Indirect_Bilirubin = as.numeric(ft$Indirect_Bilirubin)
ft$LDL_HDL_Ratio = as.numeric(ft$LDL_HDL_Ratio)
ft$Hematocrit = as.numeric(ft$Hematocrit)
ft$Albumin = as.numeric(ft$Albumin)
ft$Protein = as.numeric(ft$Protein)
ft$VLDL = as.numeric(ft$VLDL)
ft$RDW_SD = as.numeric(ft$RDW_SD)
ft$CRP = as.numeric(ft$CRP)
ft$MPV_Mean_Platelet_Volume = as.numeric(ft$MPV_Mean_Platelet_Volume)
ft$Immature_granulocyte_perc = as.numeric(ft$Immature_granulocyte_perc)
#ft$IG_0.0.3 = as.numeric(ft$IG_0.0.3)
ft$PDW = as.numeric(ft$PDW)
ft$PLCR = as.numeric(ft$PLCR)
ft$PCT = as.numeric(ft$PCT)
ft$RBS = as.numeric(ft$RBS)
ft$GLYCOSYLATED_Hb_IFCC = as.numeric(ft$GLYCOSYLATED_Hb_IFCC)
ft$BUN = as.numeric(ft$BUN)
ft$BUN_Sr_creatinine = as.numeric(ft$BUN_Sr_creatinine)
ft$Uric_Acid = as.numeric(ft$Uric_Acid)
ft$Estimated_Glomerular_filtration_rate = as.numeric(ft$Estimated_Glomerular_filtration_rate)
ft$NonHDL_Cholesterol = as.numeric(ft$NonHDL_Cholesterol)
ft$Vitamin_D_25_Hydroxy = as.numeric(ft$Vitamin_D_25_Hydroxy)
ft$Free_T3 = as.numeric(ft$Free_T3)
ft$Free_T4 = as.numeric(ft$Free_T4)
ft$Hs.CRP_High_Sensitivity_CRP = as.numeric(ft$Hs.CRP_High_Sensitivity_CRP)
#$Transferrin = as.numeric(ft$Transferrin)
ft$Polymorphs = as.numeric(ft$Polymorphs)
ft$Sodium = as.numeric(ft$Sodium)
ft$Potassium = as.numeric(ft$Potassium)
ft$SerumIronStudy_TIBC_UIBC = as.numeric(ft$SerumIronStudy_TIBC_UIBC)
ft$Transferrin_Saturation = as.numeric(ft$Transferrin_Saturation)
ft$Immature_granulocytes = as.numeric(ft$Immature_granulocytes)
ft$LDH_1 = as.numeric(ft$LDH_1)
ft$Total_Calcium = as.numeric(ft$Total_Calcium)
ft$Serum_Iron = as.numeric(ft$Serum_Iron)
ft$MENTZ1 = as.numeric(ft$MENTZ1)
ft$NLR_4 = as.numeric(ft$NLR_4)
ft$PO4_mg.dl = as.numeric(ft$PO4_mg.dl)
ft$Cl._mEq.L = as.numeric(ft$Cl._mEq.L)
ft$SGOT_SGPT = as.numeric(ft$SGOT_SGPT)
ft$CHOL.LDL_Ratio = as.numeric(ft$CHOL.LDL_Ratio)
ft$APB. = as.numeric(ft$APB.)
ft$APOA = as.numeric(ft$APOA)
ft$APOB = as.numeric(ft$APOB)
ft$LPA = as.numeric(ft$LPA)
ft$anthropometry.head_cir = as.numeric(ft$anthropometry.head_cir)
ft$anthropometry.height = as.numeric(ft$anthropometry.height)
ft$anthropometry.hip_cir = as.numeric(ft$anthropometry.hip_cir)
ft$anthropometry.sys_bp = as.numeric(ft$anthropometry.sys_bp)
ft$anthropometry.dia_bp = as.numeric(ft$anthropometry.dia_bp)
ft$anthropometry.body_fat = as.numeric(ft$anthropometry.body_fat)
ft$anthropometry.waist_cir = as.numeric(ft$anthropometry.waist_cir)
ft$anthropometry.weight = as.numeric(ft$anthropometry.weight)
ft$anthropometry.glucose_mg_dl = as.numeric(ft$anthropometry.glucose_mg_dl)
ft$Granulocytes = as.numeric(ft$Granulocytes)
ft$Granulocyte_count = as.numeric(ft$Granulocyte_count)
ft$MID = as.numeric(ft$MID)
ft$MID_Percent = as.numeric(ft$MID_Percent)


# Hatta correction --------------------------------------------------------

hatta_corrected = read_excel("Hatta_corrected.xlsx")
names(hatta_corrected)[names(hatta_corrected) == "Total Iron Binding Capacity (TIBC)"] <- "SerumIronStudy_TIBC_UIBC"
names(hatta_corrected)[names(hatta_corrected) == "Transferrin Saturation"] <- "Transferrin_Saturation"
names(hatta_corrected)[names(hatta_corrected) == "T3, Total"] <- "T3_Total"
names(hatta_corrected)[names(hatta_corrected) == "T4, Total"] <- "T4_Total"

ft$SerumIronStudy_TIBC_UIBC[ft$LocalID %in% hatta_corrected$LocalID] <- hatta_corrected$SerumIronStudy_TIBC_UIBC[match(ft$LocalID[ft$LocalID %in% hatta_corrected$LocalID], hatta_corrected$LocalID)]
ft$Transferrin_Saturation[ft$LocalID %in% hatta_corrected$LocalID] <- hatta_corrected$Transferrin_Saturation[match(ft$LocalID[ft$LocalID %in% hatta_corrected$LocalID], hatta_corrected$LocalID)]
ft$T3_Total[ft$LocalID %in% hatta_corrected$LocalID] <- hatta_corrected$T3_Total[match(ft$LocalID[ft$LocalID %in% hatta_corrected$LocalID], hatta_corrected$LocalID)]
ft$T4_Total[ft$LocalID %in% hatta_corrected$LocalID] <- hatta_corrected$T4_Total[match(ft$LocalID[ft$LocalID %in% hatta_corrected$LocalID], hatta_corrected$LocalID)]
ft$TSH[ft$LocalID %in% hatta_corrected$LocalID] <- hatta_corrected$TSH[match(ft$LocalID[ft$LocalID %in% hatta_corrected$LocalID], hatta_corrected$LocalID)]



# Value replacement - IGIB -------------------------------------------------------

IGIB_lipid_corrected = read_xlsx("Query_HDL_LDL_40samples.xlsx")
names(IGIB_lipid_corrected)[names(IGIB_lipid_corrected)=="Local ID"] = "LocalID"
names(IGIB_lipid_corrected)[names(IGIB_lipid_corrected)=="HDL CHOLESTEROL :                                     (Enzymatic)"] = "HDL"
names(IGIB_lipid_corrected)[names(IGIB_lipid_corrected)=="LDL CHOLESTEROL :                                    (Direct)"] = "LDL"
names(IGIB_lipid_corrected)[names(IGIB_lipid_corrected)=="LDL/HDL RATIO :"] = "LDL_HDL_Ratio"
ft$HDL[ft$LocalID %in% IGIB_lipid_corrected$LocalID] <- IGIB_lipid_corrected$HDL[match(ft$LocalID[ft$LocalID %in% IGIB_lipid_corrected$LocalID], IGIB_lipid_corrected$LocalID)]
ft$LDL[ft$LocalID %in% IGIB_lipid_corrected$LocalID] <- IGIB_lipid_corrected$LDL[match(ft$LocalID[ft$LocalID %in% IGIB_lipid_corrected$LocalID], IGIB_lipid_corrected$LocalID)]
ft$LDL_HDL_Ratio[ft$LocalID %in% IGIB_lipid_corrected$LocalID] <- IGIB_lipid_corrected$LDL_HDL_Ratio[match(ft$LocalID[ft$LocalID %in% IGIB_lipid_corrected$LocalID], IGIB_lipid_corrected$LocalID)]

IGIB_hipcirc_corrected = read.table("IGIB_abnormal_hipcirc_values_corrected.txt", sep = '\t', header = T)
ft$anthropometry.hip_cir[ft$LocalID %in% IGIB_hipcirc_corrected$LocalID] <- IGIB_hipcirc_corrected$anthropometry.hip_cir[match(ft$LocalID[ft$LocalID %in% IGIB_hipcirc_corrected$LocalID], IGIB_hipcirc_corrected$LocalID)]

IGIB_headcirc = read_xlsx("igib_headcirc_corrected.xlsx")
ft$anthropometry.head_cir[ft$LocalID %in% IGIB_headcirc$LocalID] <- IGIB_headcirc$anthropometry.head_cir[match(ft$LocalID[ft$LocalID %in% IGIB_headcirc$LocalID], IGIB_headcirc$LocalID)]

IGIB_TSH = read_xlsx("IGIB_TSH.xlsx")
ft$TSH[ft$LocalID %in% IGIB_TSH$LocalID] <- IGIB_TSH$TSH[match(ft$LocalID[ft$LocalID %in% IGIB_TSH$LocalID], IGIB_TSH$LocalID)]

IGIB_Chloride = read_xlsx("IGIB_chloride.xlsx")
ft$Cl._mEq.L[ft$LocalID %in% IGIB_Chloride$LocalID] <- IGIB_Chloride$Chloride[match(ft$LocalID[ft$LocalID %in% IGIB_Chloride$LocalID], IGIB_Chloride$LocalID)]

# Value replacement - AIIM ------------------------------------------------

AIIM_new = read_xlsx("AIIMS-J-Master sheet_Biochemcal data.21-05-2023_1447.xlsx")
names(AIIM_new)[names(AIIM_new) == "Local ID"] <- "LocalID"
names(AIIM_new)[names(AIIM_new) == "TOTAL CHOLESTEROL : (Enzymatic)"] <- "Cholesterol"

ft$Cholesterol[ft$LocalID %in% AIIM_new$LocalID] <- AIIM_new$Cholesterol[match(ft$LocalID[ft$LocalID %in% AIIM_new$LocalID], AIIM_new$LocalID)]


AIIM_bili = read_xlsx("bilirubin data_AIIMS.xlsx")
names(AIIM_bili)[names(AIIM_bili) == "Total"] <- "Total_Bilirubin"
names(AIIM_bili)[names(AIIM_bili) == "Direct Bilirubin"] <- "Direct_Bilirubin"
names(AIIM_bili)[names(AIIM_bili) == "Indirect Bilirubin"] <- "Indirect_Bilirubin"

ft$Total_Bilirubin[ft$LocalID %in% AIIM_bili$LocalID] <- AIIM_bili$Total_Bilirubin[match(ft$LocalID[ft$LocalID %in% AIIM_bili$LocalID], AIIM_bili$LocalID)]
ft$Direct_Bilirubin[ft$LocalID %in% AIIM_bili$LocalID] <- AIIM_bili$Direct_Bilirubin[match(ft$LocalID[ft$LocalID %in% AIIM_bili$LocalID], AIIM_bili$LocalID)]
ft$Indirect_Bilirubin[ft$LocalID %in% AIIM_bili$LocalID] <- AIIM_bili$Indirect_Bilirubin[match(ft$LocalID[ft$LocalID %in% AIIM_bili$LocalID], AIIM_bili$LocalID)]

# NIBG_found data ---------------------------------------------------------
NIBG_found = read_xlsx("missing_ODK_data_found_list_nibmg.xlsx")
NIBG_found_BBC = read_xlsx("NIBMG_biochemistry_july2023_N1330.xlsx")

emptycolumns = colSums(is.na(NIBG_found)|NIBG_found=="")==nrow(NIBG_found)
NIBG_found = NIBG_found[ ,!emptycolumns]

NIBG_found_BBC  = subset(NIBG_found_BBC, select = -`Sl No.`)
#NIBG_found_BBC  = subset(NIBG_found_BBC, select = -`Sample barcode`)
NIBG_found_BBC  = subset(NIBG_found_BBC, select = -`Sample ID`)
NIBG_found_BBC  = subset(NIBG_found_BBC, select = -`Collection Center`)
NIBG_found_BBC  = subset(NIBG_found_BBC, select = -Factors)
NIBG_found_BBC  = subset(NIBG_found_BBC, select = -Sex)

names(NIBG_found_BBC)[names(NIBG_found_BBC) == "Sample barcode"] <- "LocalID"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "AGE (Years)"] <- "Age"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "HEMOGLOBIN (g/dL)"] <- "HB_Haemoglobin"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "RBC_Count (million/?L)"] <- "RBC_Red_Blood_Cell_Count"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "WBC_Count (thousands/?L)"] <- "WBC_Total_White_Blood_Cell_Count"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "PLATELET_Count (Lakhs/?L)"] <- "Platelet_Count"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "NEUTROPHILS (%)"] <- "Neutrophils"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "ABSOLUTE_NEUTROPHIL_COUNT (thousands/?L)"] <- "Absolute_Neutrophil_Count"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "EOSINOPHILS (%)"] <- "Eosinophils"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "ABSOLUTE_EOSINOPHIL_COUNT (thousands/?L)"] <- "Absolute_Eosinophil_Count"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "LYMPHOCYTES (%)"] <- "Lymphocytes"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "ABSOLUTE_LYMPHOCYTE_COUNT (thousands/?L)"] <- "Absolute_Lymphocyte_Count"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "MONOCYTES (%)"] <- "Monocytes"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "ABSOLUTE_MONOCYTE_COUNT (thousand/?L)"] <- "Absolute_Monocyte_Count"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "BASOPHILS (%)"] <- "Basophils"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "ABSOLUTE_BASOPHIL_COUNT (thousand/?L)"] <- "Absolute_Basophil_Count"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "SGOT (U/L)"] <- "AST_SGOT"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "SGPT (U/L)"] <- "ALT_SGPT"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "ALKALINE_PHOSPHATASE (U/L)"] <- "Alkaline_Phosphatase"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "GLUCOSE_FASTING_PLASMA (mg/dL)"] <- "FBS_Fasting_Blood_Glucose"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "CHOLESTEROL (mg/dL)"] <- "Cholesterol"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "TRIGLYCERIDES (mg/dL)"] <- "Triglycerides"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "HDL_CHOLESTEROL (mg/dL)"] <- "HDL"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "LDL_CHOLESTEROL (mg/dL)"] <- "LDL"
names(NIBG_found_BBC)[names(NIBG_found_BBC) == "CREATININE (mg/dL)"] <- "Creatinine"


names(NIBG_found)[names(NIBG_found) == "Barcode present in ODK"] <- "LocalID"
NIBG_found  = subset(NIBG_found, select = -KEY)
colnames(NIBG_found) <- gsub(x = colnames(NIBG_found), pattern = "\\-", replacement = ":")  
colnames(NIBG_found) <- gsub(x = colnames(NIBG_found), pattern = "\\:", replacement = ".")  

colnames(NIBG_found)[colnames(NIBG_found)=="anthropometry.wasit_cir"] = "anthropometry.waist_cir"
colnames(NIBG_found)[colnames(NIBG_found)=="LocalID"] = "localid"
colnames(NIBG_found)[colnames(NIBG_found)=="introduction_1.local_id_manual...2"] = "introduction_1.local_id_manual"
NIBG_found = subset(NIBG_found, select = -introduction_1.local_id_manual...14)

NIBG_found$center = "NIBG"


if (all(colnames(odk_main) == colnames(NIBG_found))) {
  print("Column names are equal")
} else {
  print("Column names are not equal")
}


NIBG_found_joined <- merge(NIBG_found_BBC, NIBG_found, by.x = "LocalID", 
                           by.y = "localid", all.x = F, all.y = F)


NIBG_found_joined$region = "East"
NIBG_found_joined$name_dob_1.ethnicity = tolower(NIBG_found_joined$name_dob_1.ethnicity)

NIBG_found_joined$name_dob_1.ethnicity[NIBG_found_joined$name_dob_1.ethnicity=="kudmi mahato"] = "kudmi_mahato"
NIBG_found_joined$name_dob_1.ethnicity[NIBG_found_joined$name_dob_1.ethnicity=="rajbanshi"] = "rajbangshi"
NIBG_found_joined$name_dob_1.ethnicity[NIBG_found_joined$name_dob_1.ethnicity=="rajput"] = "namasudra"
#NIBG_found_joined$name_dob_1.ethnicity[NIBG_found_joined$name_dob_1.ethnicity=="male"] = "kurmi mahato"

NIBG_found_joined = NIBG_found_joined[,!names(NIBG_found_joined) %in% (setdiff(colnames(NIBG_found_joined), colnames(ft)))]

new_cols = c(setdiff(colnames(ft), colnames(NIBG_found_joined)))
library(dplyr)
NIBG_found_joined = NIBG_found_joined %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

NIBG_found_joined = NIBG_found_joined[names(ft)]

NIBG_found_joined$name_dob_1.dob <- sapply(NIBG_found_joined$name_dob_1.dob, function(date) {
  if (grepl("^[A-Za-z]{3} \\d{1,2}, \\d{4}$", date)) {  # Check if string has 5 characters and all characters are numeric
    parsed_date <- mdy(date)
    return(format(parsed_date, "%Y-%m-%d"))
  } else {
    return(date)
  }
})

NIBG_found_joined$introduction_1.examination_date <- sapply(NIBG_found_joined$introduction_1.examination_date, function(date) {
  if (grepl("^[A-Za-z]{3} \\d{1,2}, \\d{4}$", date)) {  # Check if string has 5 characters and all characters are numeric
    parsed_date <- mdy(date)
    return(format(parsed_date, "%Y-%m-%d"))
  } else {
    return(date)
  }
})


NIBG_found_joined$age = NIBG_found_joined$name_dob_1.approx_age
NIBG_found_joined$age = as.numeric(NIBG_found_joined$age)
NIBG_found_joined$age = ifelse(is.na(NIBG_found_joined$age), NIBG_found_joined$name_dob_1.age_on_interview,
                                    NIBG_found_joined$age)
NIBG_found_joined$age = as.numeric(NIBG_found_joined$age)

NIBG_found_joined$age_withBBC = NIBG_found_joined$age
NIBG_found_joined$age_withBBC = ifelse(is.na(NIBG_found_joined$age_withBBC), NIBG_found_joined$Age,
                                 NIBG_found_joined$age_withBBC)

NIBG_found_joined$name_dob_1.dob = as.character(NIBG_found_joined$name_dob_1.dob)
NIBG_found_joined$introduction_1.examination_date = as.character(NIBG_found_joined$introduction_1.examination_date)
ft = rbind(ft, NIBG_found_joined)

# NIBG_missing data -------------------------------------------------------
NIBG_missing_BBC = read_xlsx("NIBG_missing_pheno.xlsx")
NIBG_missing_ODK = read_xlsx("NIBG_missing_ODK.xlsx")

emptycolumns = colSums(is.na(NIBG_missing_BBC)|NIBG_missing_BBC=="")==nrow(NIBG_missing_BBC)
NIBG_missing_BBC = NIBG_missing_BBC[ ,!emptycolumns]

emptycolumns = colSums(is.na(NIBG_missing_ODK)|NIBG_missing_ODK=="")==nrow(NIBG_missing_ODK)
NIBG_missing_ODK = NIBG_missing_ODK[ ,!emptycolumns]


# NIBG_missing - remove unnecessary columns -------------------------------

NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -SeqID)
NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -ethnicity)
NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -gender)
NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -centre)
NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -BioCHEMISTRY)
NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -`Sample barcode`)
NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -`Sample ID`)
NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -`Collection Center`)
NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -Factors)
NIBG_missing_BBC  = subset(NIBG_missing_BBC, select = -Sex)


# Renaming columns BBC ----------------------------------------------------


#names(NIBG_BBC)[names(NIBG_BBC) == "Sample barcode"] <- "LocalID"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "AGE (Years)"] <- "Age"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "HEMOGLOBIN (g/dL)"] <- "HB_Haemoglobin"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "RBC_Count (million/?L)"] <- "RBC_Red_Blood_Cell_Count"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "WBC_Count (thousands/?L)"] <- "WBC_Total_White_Blood_Cell_Count"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "PLATELET_Count (Lakhs/?L)"] <- "Platelet_Count"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "NEUTROPHILS (%)"] <- "Neutrophils"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "ABSOLUTE_NEUTROPHIL_COUNT (thousands/?L)"] <- "Absolute_Neutrophil_Count"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "EOSINOPHILS (%)"] <- "Eosinophils"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "ABSOLUTE_EOSINOPHIL_COUNT (thousands/?L)"] <- "Absolute_Eosinophil_Count"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "LYMPHOCYTES (%)"] <- "Lymphocytes"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "ABSOLUTE_LYMPHOCYTE_COUNT (thousands/?L)"] <- "Absolute_Lymphocyte_Count"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "MONOCYTES (%)"] <- "Monocytes"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "ABSOLUTE_MONOCYTE_COUNT (thousand/?L)"] <- "Absolute_Monocyte_Count"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "BASOPHILS (%)"] <- "Basophils"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "ABSOLUTE_BASOPHIL_COUNT (thousand/?L)"] <- "Absolute_Basophil_Count"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "SGOT (U/L)"] <- "AST_SGOT"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "SGPT (U/L)"] <- "ALT_SGPT"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "ALKALINE_PHOSPHATASE (U/L)"] <- "Alkaline_Phosphatase"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "GLUCOSE_FASTING_PLASMA (mg/dL)"] <- "FBS_Fasting_Blood_Glucose"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "CHOLESTEROL (mg/dL)"] <- "Cholesterol"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "TRIGLYCERIDES (mg/dL)"] <- "Triglycerides"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "HDL_CHOLESTEROL (mg/dL)"] <- "HDL"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "LDL_CHOLESTEROL (mg/dL)"] <- "LDL"
names(NIBG_missing_BBC)[names(NIBG_missing_BBC) == "CREATININE (mg/dL)"] <- "Creatinine"

# Removing 2 iffy local IDs -----------------------------------------------

#NIBG_missing_ODK = NIBG_missing_ODK[-which(NIBG_missing_ODK$LocalID=="NIBG/K/807218"),]
#NIBG_missing_ODK = NIBG_missing_ODK[-which(NIBG_missing_ODK$LocalID=="NIBG/K/807222"),]
# Renaming columns - ODK --------------------------------------------------
NIBG_missing_ODK  = subset(NIBG_missing_ODK, select = -SeqID)
#NIBG_missing_ODK  = subset(NIBG_missing_ODK, select = -LocalID)
NIBG_missing_ODK  = subset(NIBG_missing_ODK, select = -ethnicity)
NIBG_missing_ODK  = subset(NIBG_missing_ODK, select = -gender)
NIBG_missing_ODK  = subset(NIBG_missing_ODK, select = -centre)
NIBG_missing_ODK  = subset(NIBG_missing_ODK, select = -ODK)
NIBG_missing_ODK  = subset(NIBG_missing_ODK, select = -SubmissionDate)
NIBG_missing_ODK  = subset(NIBG_missing_ODK, select = -KEY)

colnames(NIBG_missing_ODK) <- gsub(x = colnames(NIBG_missing_ODK), pattern = "\\-", replacement = ":")  
colnames(NIBG_missing_ODK) <- gsub(x = colnames(NIBG_missing_ODK), pattern = "\\:", replacement = ".")

NIBG_missing_ODK$localid = NIBG_missing_ODK$LocalID
colnames(NIBG_missing_ODK)[colnames(NIBG_missing_ODK)=="anthropometry.wasit_cir"] = "anthropometry.waist_cir"
NIBG_missing_ODK$center = gsub("\\/.*","", NIBG_missing_ODK$localid)

NIBG_missing_ODK = subset(NIBG_missing_ODK, select = -LocalID)

#odk_main = read.table("GenomeIndia_Socio_demographics_V1_results(1).csv", header = T, sep = ",", fill = T, encoding = 'UTF-8')


if (all(colnames(odk_main) == colnames(NIBG_missing_ODK))) {
  print("Column names are equal")
} else {
  print("Column names are not equal")
}


NIBG_missing_joined <- merge(NIBG_missing_BBC, NIBG_missing_ODK, by.x = "LocalID", 
                             by.y = "localid", all.x = F, all.y = F)

NIBG_missing_joined_other <- merge(NIBG_missing_BBC, odk_main, by.x = "LocalID", 
                             by.y = "localid", all.x = F, all.y = F)


NIBG_missing_joined$region = "East"
NIBG_missing_joined_other$region[NIBG_missing_joined_other$name_dob_1.state=="jharkhand"] = "East"
NIBG_missing_joined_other$region[NIBG_missing_joined_other$name_dob_1.state=="orissa"] = "East"
NIBG_missing_joined_other$region[NIBG_missing_joined_other$name_dob_1.state=="assam"] = "North-East"

NIBG_missing_joined$name_dob_1.ethnicity = tolower(NIBG_missing_joined$name_dob_1.ethnicity)

NIBG_missing_joined$name_dob_1.ethnicity[NIBG_missing_joined$name_dob_1.ethnicity=="rahri brahmin"] = "rahri_brahmin"

NIBG_missing_joined_other$name_dob_1.ethnicity = tolower(NIBG_missing_joined_other$name_dob_1.ethnicity)
NIBG_missing_joined_other$name_dob_1.ethnicity[NIBG_missing_joined_other$name_dob_1.ethnicity=="chik baraik"] = "chik_baraik"
NIBG_missing_joined_other$name_dob_1.ethnicity[NIBG_missing_joined_other$name_dob_1.ethnicity=="maithili brahmin"] = "maithili_brahmin"
NIBG_missing_joined_other$name_dob_1.ethnicity[NIBG_missing_joined_other$name_dob_1.ethnicity=="oriya brahmin"] = "oriya_brahmin"
NIBG_missing_joined_other$name_dob_1.ethnicity[NIBG_missing_joined_other$name_dob_1.ethnicity=="st/hajong"] = "hajong"
NIBG_missing_joined_other$name_dob_1.ethnicity[NIBG_missing_joined_other$name_dob_1.ethnicity=="st/rabha/patirabha"] = "rabha"

NIBG_missing_joined = NIBG_missing_joined[,!names(NIBG_missing_joined) %in% (setdiff(colnames(NIBG_missing_joined), colnames(ft)))]
NIBG_missing_joined_other = NIBG_missing_joined_other[,!names(NIBG_missing_joined_other) %in% (setdiff(colnames(NIBG_missing_joined_other), colnames(ft)))]

new_cols = c(setdiff(colnames(ft), colnames(NIBG_missing_joined)))
library(dplyr)
NIBG_missing_joined = NIBG_missing_joined %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

NIBG_missing_joined = NIBG_missing_joined[names(ft)]

new_cols = c(setdiff(colnames(ft), colnames(NIBG_missing_joined_other)))
library(dplyr)
NIBG_missing_joined_other = NIBG_missing_joined_other %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

NIBG_missing_joined_other = NIBG_missing_joined_other[names(ft)]


NIBG_missing_joined$age = NIBG_missing_joined$name_dob_1.approx_age
NIBG_missing_joined_other$age = NIBG_missing_joined_other$name_dob_1.approx_age

NIBG_missing_joined$age = as.numeric(NIBG_missing_joined$age)
NIBG_missing_joined$age = ifelse(is.na(NIBG_missing_joined$age), NIBG_missing_joined$name_dob_1.age_on_interview,
                                      NIBG_missing_joined$age)
NIBG_missing_joined$age = as.numeric(NIBG_missing_joined$age)

NIBG_missing_joined$age_withBBC = NIBG_missing_joined$age
NIBG_missing_joined$age_withBBC = ifelse(is.na(NIBG_missing_joined$age_withBBC), NIBG_missing_joined$Age,
                                   NIBG_missing_joined$age_withBBC)

NIBG_missing_joined$introduction_1.examination_date = gsub("/", "-",NIBG_missing_joined$introduction_1.examination_date)

NIBG_missing_joined$introduction_1.examination_date = format(dmy(NIBG_missing_joined$introduction_1.examination_date), "%Y-%m-%d")
NIBG_missing_joined$introduction_1.examination_date = as.character(NIBG_missing_joined$introduction_1.examination_date)

ft = rbind(ft, NIBG_missing_joined)

NIBG_missing_joined_other$introduction_1.examination_date = as.Date(NIBG_missing_joined_other$introduction_1.examination_date, format = "%Y-%m-%d")
NIBG_missing_joined_other$introduction_1.examination_date = as.character(NIBG_missing_joined_other$introduction_1.examination_date)

NIBG_missing_joined_other$name_dob_1.dob = as.Date(NIBG_missing_joined_other$name_dob_1.dob, format = "%Y-%m-%d")
NIBG_missing_joined_other$name_dob_1.dob = as.character(NIBG_missing_joined_other$name_dob_1.dob)

NIBG_missing_joined_other$age = ifelse(is.na(NIBG_missing_joined_other$age), NIBG_missing_joined_other$name_dob_1.age_on_interview,
                                 NIBG_missing_joined_other$age)
NIBG_missing_joined$age = as.numeric(NIBG_missing_joined$age)

NIBG_missing_joined_other$age = ifelse(is.na(NIBG_missing_joined_other$age), trunc((NIBG_missing_joined_other$name_dob_1.dob %--% NIBG_missing_joined_other$introduction_1.examination_date) / years(1)),
                                       NIBG_missing_joined_other$age)



NIBG_missing_joined_other$age_withBBC = NIBG_missing_joined_other$age
NIBG_missing_joined_other$age_withBBC = ifelse(is.na(NIBG_missing_joined_other$age_withBBC), NIBG_missing_joined_other$Age,
                                         NIBG_missing_joined_other$age_withBBC)
ft = rbind(ft, NIBG_missing_joined_other)


# History of illness in multiple lines ------------------------------------------------

ft$history_illness.name_medication = gsub('\n', ',', ft$history_illness.name_medication)
ft$history_illness.history_illness_self = gsub('\n', ',', ft$history_illness.history_illness_self)
ft$history_illness.history_illness_father = gsub('\n', ',', ft$history_illness.history_illness_father)
ft$history_illness.history_illness_mother = gsub('\n', ',', ft$history_illness.history_illness_mother)
ft$history_illness.history_illness_family = gsub('\n', ',', ft$history_illness.history_illness_family)
library(dplyr)
ft <- ft %>% 
  mutate(across(everything(),  trimws, which = "both"))

ft$history_illness.name_medication = gsub('Ã°Â\u009fÂ\u0092Â\u008a', '', ft$history_illness.name_medication)

# trying to fill in missing ethnicities -----------------------------------

CBRsamples = read_xlsx("GWAS and WGS updates _to be refered (1).xlsx", sheet = 1)
colnames(CBRsamples)[colnames(CBRsamples) == "Local IDs"] = "LocalID"
colnames(CBRsamples)[colnames(CBRsamples) == "Ethnic group"] = "Ethnicity"

ft$name_dob_1.ethnicity[ft$LocalID %in% CBRsamples$LocalID] <- 
  CBRsamples$Ethnicity[match(ft$LocalID[ft$LocalID %in% CBRsamples$LocalID], 
                             CBRsamples$LocalID)]

ft$name_dob_1.ethnicity = tolower(ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity = gsub("(sanscago)", "", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="adikarnataka ()"] = "adikarnataka"
ft$name_dob_1.ethnicity <- sub("vakkaliga.*", "vakkaliga", ft$name_dob_1.ethnicity)
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="hassan iyengar"] = "iyengar"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="hebbar iyengar"] = "iyengar"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="mysore iyengar"] = "iyengar"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="iyengar (from 67 samples)"] = "iyengar"
#ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="k1"] = "K1"
#ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="k2"] = "K2"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="saryuparin brahmin"] = "saryuparin_brahmin"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="vaidiki brahmin"] = "vaidiki_brahmin"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="kashyap gotra"] = "saryuparin_brahmin"

skims_df1 = read.table("SKIMS_final.csv", sep = ",", header = T, fill = T)
skims_df1[skims_df1==""] = NA
skims_df1 = skims_df1[-which(is.na(skims_df1$Barcode)),]

ft$name_dob_1.ethnicity[ft$LocalID %in% skims_df1$Barcode & is.na(ft$name_dob_1.ethnicity)] <- 
  skims_df1$Ethnic[match(ft$LocalID[ft$LocalID %in% skims_df1$Barcode & is.na(ft$name_dob_1.ethnicity)], 
                         skims_df1$Barcode)]
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="KM"] = "kashmiri_muslim"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="KP"] = "kashmiri_pandit"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="Baltis"] = "balti"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="Dogras"] = "dogra"
ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="G&B"] = "gujjar_and_bakkarwal"

ft$name_dob_1.ethnicity[ft$LocalID=="IISR/G/000241"] = "deshastha_brahmin"
ft$name_dob_1.ethnicity[ft$LocalID=="IISR/G/001517"] = "kolis"



# converting numeric ------------------------------------------------------

ft$FBS_Fasting_Blood_Glucose = as.numeric(ft$FBS_Fasting_Blood_Glucose)
ft$HbA1C_Glycosylated_Haemoglobin = as.numeric(ft$HbA1C_Glycosylated_Haemoglobin)
#ft$HbA1C_Glycosylated_Haemoglobin[ft$HbA1C_Glycosylated_Haemoglobin == 0] = NA
#merged_df_ft1 = read.table("Added_addnlt_Apr19.txt", sep = "\t", header = T, fill= T, encoding = 'latin1')
ft$Urea = as.numeric(ft$Urea)
ft$Creatinine = as.numeric(ft$Creatinine)
ft$Total_Bilirubin = as.numeric(ft$Total_Bilirubin)
ft$ALT_SGPT = as.numeric(ft$ALT_SGPT)
ft$AST_SGOT = as.numeric(ft$AST_SGOT)
ft$Alkaline_Phosphatase = as.numeric(ft$Alkaline_Phosphatase)
ft$Cholesterol = as.numeric(ft$Cholesterol)
ft$Triglycerides = as.numeric(ft$Triglycerides)
ft$HDL = as.numeric(ft$HDL)
ft$LDL = as.numeric(ft$LDL)
ft$T3_Total = as.numeric(ft$T3_Total)
ft$T4_Total = as.numeric(ft$T4_Total)
ft$TSH = as.numeric(ft$TSH)
ft$Homocysteine_Levels = as.numeric(ft$Homocysteine_Levels)
ft$Vitamin_B12 = as.numeric(ft$Vitamin_B12)
ft$Folic_Acid = as.numeric(ft$Folic_Acid)
ft$Fasting_Insulin_Level = as.numeric(ft$Fasting_Insulin_Level)
ft$C.Peptide = as.numeric(ft$C.Peptide)
ft$HB_Haemoglobin = as.numeric(ft$HB_Haemoglobin)
ft$RBC_Red_Blood_Cell_Count = as.numeric(ft$RBC_Red_Blood_Cell_Count) #this is probably in millions
ft$MCH_Mean_Corpuscular_Hb = as.numeric(ft$MCH_Mean_Corpuscular_Hb)
ft$MCHC_Mean_Corpuscular_Hb_Concn = as.numeric(ft$MCHC_Mean_Corpuscular_Hb_Concn)
ft$MCV_Mean_Corpuscular_Volume = as.numeric(ft$MCV_Mean_Corpuscular_Volume)
ft$RDW_Red_Cell_Distribution_Width = as.numeric(ft$RDW_Red_Cell_Distribution_Width)
ft$WBC_Total_White_Blood_Cell_Count = as.numeric(ft$WBC_Total_White_Blood_Cell_Count)
ft$Basophils = as.numeric(ft$Basophils)
ft$Eosinophils = as.numeric(ft$Eosinophils)
ft$Lymphocytes = as.numeric(ft$Lymphocytes)
ft$Monocytes = as.numeric(ft$Monocytes)
ft$Neutrophils = as.numeric(ft$Neutrophils)
ft$Platelet_Count = as.numeric(ft$Platelet_Count)
ft$A_G_Ratio_Albumin_Globulin = as.numeric(ft$A_G_Ratio_Albumin_Globulin)
ft$Absolute_Neutrophil_Count = as.numeric(ft$Absolute_Neutrophil_Count)
ft$Absolute_Basophil_Count = as.numeric(ft$Absolute_Basophil_Count)
ft$Absolute_Eosinophil_Count = as.numeric(ft$Absolute_Eosinophil_Count)
ft$Absolute_Lymphocyte_Count = as.numeric(ft$Absolute_Lymphocyte_Count)
ft$Absolute_Monocyte_Count = as.numeric(ft$Absolute_Monocyte_Count)
ft$CHOL_HDL_Ratio = as.numeric(ft$CHOL_HDL_Ratio)
ft$Direct_Bilirubin = as.numeric(ft$Direct_Bilirubin)
ft$ESR = as.numeric(ft$ESR)
ft$Estimated_Average_Glucose = as.numeric(ft$Estimated_Average_Glucose)
ft$Gamma_GT_GGTP = as.numeric(ft$Gamma_GT_GGTP)
ft$Globulin = as.numeric(ft$Globulin)
ft$Indirect_Bilirubin = as.numeric(ft$Indirect_Bilirubin)
ft$LDL_HDL_Ratio = as.numeric(ft$LDL_HDL_Ratio)
ft$Hematocrit = as.numeric(ft$Hematocrit)
ft$Albumin = as.numeric(ft$Albumin)
ft$Protein = as.numeric(ft$Protein)
ft$VLDL = as.numeric(ft$VLDL)
ft$RDW_SD = as.numeric(ft$RDW_SD)
ft$CRP = as.numeric(ft$CRP)
ft$MPV_Mean_Platelet_Volume = as.numeric(ft$MPV_Mean_Platelet_Volume)
ft$Immature_granulocyte_perc = as.numeric(ft$Immature_granulocyte_perc)
#ft$IG_0.0.3 = as.numeric(ft$IG_0.0.3)
ft$PDW = as.numeric(ft$PDW)
ft$PLCR = as.numeric(ft$PLCR)
ft$PCT = as.numeric(ft$PCT)
ft$RBS = as.numeric(ft$RBS)
ft$GLYCOSYLATED_Hb_IFCC = as.numeric(ft$GLYCOSYLATED_Hb_IFCC)
ft$BUN = as.numeric(ft$BUN)
ft$BUN_Sr_creatinine = as.numeric(ft$BUN_Sr_creatinine)
ft$Uric_Acid = as.numeric(ft$Uric_Acid)
ft$Estimated_Glomerular_filtration_rate = as.numeric(ft$Estimated_Glomerular_filtration_rate)
ft$NonHDL_Cholesterol = as.numeric(ft$NonHDL_Cholesterol)
ft$Vitamin_D_25_Hydroxy = as.numeric(ft$Vitamin_D_25_Hydroxy)
ft$Free_T3 = as.numeric(ft$Free_T3)
ft$Free_T4 = as.numeric(ft$Free_T4)
ft$Hs.CRP_High_Sensitivity_CRP = as.numeric(ft$Hs.CRP_High_Sensitivity_CRP)
#ft$Transferrin = as.numeric(ft$Transferrin)
ft$Polymorphs = as.numeric(ft$Polymorphs)
ft$Sodium = as.numeric(ft$Sodium)
ft$Potassium = as.numeric(ft$Potassium)
ft$SerumIronStudy_TIBC_UIBC = as.numeric(ft$SerumIronStudy_TIBC_UIBC)
ft$Transferrin_Saturation = as.numeric(ft$Transferrin_Saturation)
ft$Immature_granulocytes = as.numeric(ft$Immature_granulocytes)
ft$LDH_1 = as.numeric(ft$LDH_1)
ft$Total_Calcium = as.numeric(ft$Total_Calcium)
ft$Serum_Iron = as.numeric(ft$Serum_Iron)
ft$MENTZ1 = as.numeric(ft$MENTZ1)
ft$NLR_4 = as.numeric(ft$NLR_4)
ft$PO4_mg.dl = as.numeric(ft$PO4_mg.dl)
ft$Cl._mEq.L = as.numeric(ft$Cl._mEq.L)
ft$SGOT_SGPT = as.numeric(ft$SGOT_SGPT)
ft$CHOL.LDL_Ratio = as.numeric(ft$CHOL.LDL_Ratio)
ft$APB. = as.numeric(ft$APB.)
ft$APOA = as.numeric(ft$APOA)
ft$APOB = as.numeric(ft$APOB)
ft$LPA = as.numeric(ft$LPA)
ft$anthropometry.head_cir = as.numeric(ft$anthropometry.head_cir)
ft$anthropometry.height = as.numeric(ft$anthropometry.height)
ft$anthropometry.hip_cir = as.numeric(ft$anthropometry.hip_cir)
ft$anthropometry.sys_bp = as.numeric(ft$anthropometry.sys_bp)
ft$anthropometry.dia_bp = as.numeric(ft$anthropometry.dia_bp)
ft$anthropometry.body_fat = as.numeric(ft$anthropometry.body_fat)
ft$anthropometry.waist_cir = as.numeric(ft$anthropometry.waist_cir)
ft$anthropometry.weight = as.numeric(ft$anthropometry.weight)
ft$anthropometry.glucose_mg_dl = as.numeric(ft$anthropometry.glucose_mg_dl)
ft$Granulocytes = as.numeric(ft$Granulocytes)
ft$Granulocyte_count = as.numeric(ft$Granulocyte_count)
ft$MID = as.numeric(ft$MID)
ft$MID_Percent = as.numeric(ft$MID_Percent)
ft$age = as.numeric(ft$age)
ft$age_withBBC = as.numeric(ft$age_withBBC)

# Blood count rescaling ---------------------------------------------------

#RESCALING WBC
boxplot(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) ~ ft$center)
ft$WBC_Total_White_Blood_Cell_Count[which(ft$center %in% c('NIBG','AIIM','MZUA','SKIM','ILSB'))] =
  ft$WBC_Total_White_Blood_Cell_Count[which(ft$center %in% c('NIBG','AIIM','MZUA','SKIM','ILSB'))]*1000

ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "CBRI"), 
                                              which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))] = 
  as.numeric(ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "CBRI"), 
                                                           which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))]) * 1000

ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "CCMB"), 
                                              which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))] = 
  as.numeric(ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "CCMB"), 
                                                           which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))]) * 1000

ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "IGIB"), 
                                              which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))] = 
  as.numeric(ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "IGIB"), 
                                                           which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))]) * 1000
ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "IBSD"), 
                                              which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))] = 
  as.numeric(ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "IBSD"), 
                                                           which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))]) * 1000

ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "RGCB"), 
                                              which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))] = 
  as.numeric(ft$WBC_Total_White_Blood_Cell_Count[intersect(which(ft$center == "RGCB"), 
                                                           which(as.numeric(ft$WBC_Total_White_Blood_Cell_Count) < 500))]) * 1000

# RESCALING NEUTROPHILS
boxplot(as.numeric(ft$Absolute_Neutrophil_Count) ~ ft$center)
ft$Absolute_Neutrophil_Count[which(ft$center %in% c('NIBG','ILSB', 'IBSD', 'MZUA'))] =
  ft$Absolute_Neutrophil_Count[which(ft$center %in% c('NIBG','ILSB', 'IBSD', 'MZUA'))]*1000

ft$Absolute_Neutrophil_Count[intersect(which(ft$center == "IGIB"), 
                                       which(as.numeric(ft$Absolute_Neutrophil_Count) < 500))] = 
  as.numeric(ft$Absolute_Neutrophil_Count[intersect(which(ft$center == "IGIB"), 
                                                    which(as.numeric(ft$Absolute_Neutrophil_Count) < 500))]) * 1000

ft$Absolute_Neutrophil_Count[intersect(which(ft$center == "CBRI"), 
                                       which(as.numeric(ft$Absolute_Neutrophil_Count) < 500))] = 
  as.numeric(ft$Absolute_Neutrophil_Count[intersect(which(ft$center == "CBRI"), 
                                                    which(as.numeric(ft$Absolute_Neutrophil_Count) < 500))]) * 1000

# ft$Absolute_Neutrophil_Count[intersect(which(ft$center == "ILSB"), 
#                                        which(as.numeric(ft$Absolute_Neutrophil_Count) < 500))] = 
#   as.numeric(ft$Absolute_Neutrophil_Count[intersect(which(ft$center == "ILSB"), 
#                                                     which(as.numeric(ft$Absolute_Neutrophil_Count) < 500))]) * 1000

# #RESCALING BASOPHILS (upto 150)
boxplot(as.numeric(ft$Absolute_Basophil_Count) ~ ft$center)

ft$Absolute_Basophil_Count[which(ft$center %in% c('NIBG','ILSB', 'IBSD', 'MZUA'))] = 
  ft$Absolute_Basophil_Count[which(ft$center %in% c('NIBG','ILSB', 'IBSD', 'MZUA'))]*1000
# 
ft$Absolute_Basophil_Count[intersect(which(ft$center == "IGIB"), 
                                     which(as.numeric(ft$Absolute_Basophil_Count) < 1))] = 
  as.numeric(ft$Absolute_Basophil_Count[intersect(which(ft$center == "IGIB"), 
                                                  which(as.numeric(ft$Absolute_Basophil_Count) < 1))]) * 1000

ft$Absolute_Basophil_Count[intersect(which(ft$center == "CBRI"), 
                                     which(as.numeric(ft$Absolute_Basophil_Count) < 1))] = 
  as.numeric(ft$Absolute_Basophil_Count[intersect(which(ft$center == "CBRI"), 
                                                  which(as.numeric(ft$Absolute_Basophil_Count) < 1))]) * 1000
# ft$Absolute_Basophil_Count[intersect(which(ft$center == "CBRI"), 
#             which(as.numeric(ft$Absolute_Basophil_Count) < 10))] = 
#   as.numeric(ft$Absolute_Basophil_Count[intersect(which(ft$center == "CBRI"), 
#               which(as.numeric(ft$Absolute_Basophil_Count) < 10))]) * 1000

#RESCALING EOSINOPHILS (upto 444 cells/mm3) 
boxplot(as.numeric(ft$Absolute_Eosinophil_Count) ~ ft$center)
ft$Absolute_Eosinophil_Count[which(ft$center %in% c('NIBG','ILSB', 'IBSD', 'MZUA'))] =
  ft$Absolute_Eosinophil_Count[which(ft$center %in% c('NIBG','ILSB', 'IBSD', 'MZUA'))]*1000

ft$Absolute_Eosinophil_Count[intersect(which(ft$center == "IGIB"), 
                                       which(as.numeric(ft$Absolute_Eosinophil_Count) < 1))] = 
  as.numeric(ft$Absolute_Eosinophil_Count[intersect(which(ft$center == "IGIB"), 
                                                    which(as.numeric(ft$Absolute_Eosinophil_Count) < 1))]) * 1000

ft$Absolute_Eosinophil_Count[intersect(which(ft$center == "CBRI"), 
                                       which(as.numeric(ft$Absolute_Eosinophil_Count) < 1))] = 
  as.numeric(ft$Absolute_Eosinophil_Count[intersect(which(ft$center == "CBRI"), 
                                                    which(as.numeric(ft$Absolute_Eosinophil_Count) < 1))]) * 1000

#RESCALING LYMPHOCYTES (upto 4600/mm3)
boxplot(as.numeric(ft$Absolute_Lymphocyte_Count) ~ ft$center)
ft$Absolute_Lymphocyte_Count[which(ft$center %in% c('NIBG','ILSB', 'SKIM', 'MZUA'))] =
  ft$Absolute_Lymphocyte_Count[which(ft$center %in% c('NIBG','ILSB', 'SKIM', 'MZUA'))]*1000

ft$Absolute_Lymphocyte_Count[intersect(which(ft$center == "IGIB"), 
                                       which(as.numeric(ft$Absolute_Lymphocyte_Count) < 100))] = 
  as.numeric(ft$Absolute_Lymphocyte_Count[intersect(which(ft$center == "IGIB"), 
                                                    which(as.numeric(ft$Absolute_Lymphocyte_Count) < 100))]) * 1000

ft$Absolute_Lymphocyte_Count[intersect(which(ft$center == "CBRI"), 
                                       which(as.numeric(ft$Absolute_Lymphocyte_Count) < 100))] = 
  as.numeric(ft$Absolute_Lymphocyte_Count[intersect(which(ft$center == "CBRI"), 
                                                    which(as.numeric(ft$Absolute_Lymphocyte_Count) < 100))]) * 1000

ft$Absolute_Lymphocyte_Count[intersect(which(ft$center == "IBSD"), 
                                       which(as.numeric(ft$Absolute_Lymphocyte_Count) < 100))] = 
  as.numeric(ft$Absolute_Lymphocyte_Count[intersect(which(ft$center == "IBSD"), 
                                                    which(as.numeric(ft$Absolute_Lymphocyte_Count) < 100))]) * 1000

#RESCALING MONOCYTES(upto 1380)
boxplot(as.numeric(ft$Absolute_Monocyte_Count) ~ ft$center)
ft$Absolute_Monocyte_Count[ft$LocalID %in% IGIB$LocalID] <- IGIB$Absolute_Monocyte_Count[match(ft$LocalID[ft$LocalID %in% IGIB$LocalID], IGIB$LocalID)]

ft$Absolute_Monocyte_Count = as.numeric(ft$Absolute_Monocyte_Count)
ft$Absolute_Monocyte_Count[which(ft$center %in% c('NIBG','ILSB', 'IBSD', 'MZUA'))] =
  ft$Absolute_Monocyte_Count[which(ft$center %in% c('NIBG','ILSB', 'IBSD', "MZUA"))]*1000

ft$Absolute_Monocyte_Count[intersect(which(ft$center == "IGIB"), 
                                     which(as.numeric(ft$Absolute_Monocyte_Count) < 10))] = 
  as.numeric(ft$Absolute_Monocyte_Count[intersect(which(ft$center == "IGIB"), 
                                                  which(as.numeric(ft$Absolute_Monocyte_Count) < 10))]) * 1000

ft$Absolute_Monocyte_Count[intersect(which(ft$center == "CBRI"), 
                                     which(as.numeric(ft$Absolute_Monocyte_Count) < 10))] = 
  as.numeric(ft$Absolute_Monocyte_Count[intersect(which(ft$center == "CBRI"), 
                                                  which(as.numeric(ft$Absolute_Monocyte_Count) < 10))]) * 1000

#RESCALING PLATELETS (upto 4.5lakh/mm3) 
boxplot(as.numeric(ft$Platelet_Count) ~ ft$center)
# ft$Platelet_Count[which(ft$center %in% c('RGCB'))] = 
#   ft$Platelet_Count[which(ft$center %in% c('RGCB'))]*100000 
ft$Platelet_Count[which(ft$center %in% c('NIBG', 'ILSB', 'MZUA', 'SKIM'))] = 
  ft$Platelet_Count[which(ft$center %in% c('NIBG', 'ILSB', 'MZUA','SKIM'))]*1000

ft$Platelet_Count[intersect(which(ft$center == "AIIM"), 
                            which(as.numeric(ft$Platelet_Count) < 1000))] = 
  as.numeric(ft$Platelet_Count[intersect(which(ft$center == "AIIM"), 
                                         which(as.numeric(ft$Platelet_Count) < 1000))]) * 1000

ft$Platelet_Count[intersect(which(ft$center == "CBRI"), 
                            which(as.numeric(ft$Platelet_Count) < 1000))] = 
  as.numeric(ft$Platelet_Count[intersect(which(ft$center == "CBRI"), 
                                         which(as.numeric(ft$Platelet_Count) < 1000))]) * 1000

ft$Platelet_Count[intersect(which(ft$center == "IGIB"), 
                            which(as.numeric(ft$Platelet_Count) < 1000))] = 
  as.numeric(ft$Platelet_Count[intersect(which(ft$center == "IGIB"), 
                                         which(as.numeric(ft$Platelet_Count) < 1000))]) * 1000

ft$Platelet_Count[intersect(which(ft$center == "IBSD"), 
                            which(as.numeric(ft$Platelet_Count) < 10))] = 
  as.numeric(ft$Platelet_Count[intersect(which(ft$center == "IBSD"), 
                                         which(as.numeric(ft$Platelet_Count) < 10))]) * 100000

ft$Platelet_Count[intersect(which(ft$center == "IBSD"), 
                            which(as.numeric(ft$Platelet_Count) > 10 & as.numeric(ft$Platelet_Count)<1000))] = 
  as.numeric(ft$Platelet_Count[intersect(which(ft$center == "IBSD"), 
                                         which(as.numeric(ft$Platelet_Count) > 10 & as.numeric(ft$Platelet_Count)<1000))]) * 1000

ft$Platelet_Count[intersect(which(ft$center == "CCMB"), 
                            which(as.numeric(ft$Platelet_Count) < 100))] = 
  as.numeric(ft$Platelet_Count[intersect(which(ft$center == "CCMB"), 
                                         which(as.numeric(ft$Platelet_Count) < 100))]) * 100000

ft$Platelet_Count[intersect(which(ft$center == "CCMB"), 
                            which(as.numeric(ft$Platelet_Count) > 100 & as.numeric(ft$Platelet_Count)<1000))] = 
  as.numeric(ft$Platelet_Count[intersect(which(ft$center == "CCMB"), 
                                         which(as.numeric(ft$Platelet_Count) > 100 & as.numeric(ft$Platelet_Count)<1000))]) * 1000

ft$Platelet_Count[intersect(which(ft$center == "RGCB"), 
                            which(as.numeric(ft$Platelet_Count) > 10 & as.numeric(ft$Platelet_Count)<1000))] = 
  as.numeric(ft$Platelet_Count[intersect(which(ft$center == "RGCB"), 
                                         which(as.numeric(ft$Platelet_Count) > 10 & as.numeric(ft$Platelet_Count)<1000))]) * 1000

ft$Platelet_Count[intersect(which(ft$center == "RGCB"), 
                            which(as.numeric(ft$Platelet_Count) <10))] = 
  as.numeric(ft$Platelet_Count[intersect(which(ft$center == "RGCB"), 
                                         which(as.numeric(ft$Platelet_Count) <10))]) * 100000


#T3_TOTAL
boxplot(as.numeric(ft$T3_Total) ~ ft$center)
ft$T3_Total[ft$LocalID %in% ft1$LocalID] <- ft1$T3_Total[match(ft$LocalID[ft$LocalID %in% ft1$LocalID], ft1$LocalID)]
#ft$T3_Total[which(ft$center == "ILSB")] = as.numeric(ft$T3_Total[(which(ft$center == "ILSB"))])*650.97/10
#ft$T3_Total[which(ft$center == "IBSD")] = as.numeric(ft$T3_Total[(which(ft$center == "IBSD"))])*650.97/10
ft$T3_Total[intersect(which(ft$center == "CBRI"), 
                      which(as.numeric(ft$T3_Total) < 10))] = 
  as.numeric(ft$T3_Total[intersect(which(ft$center == "CBRI"), 
                                   which(as.numeric(ft$T3_Total) < 10))]) / 0.01536

ft$T3_Total[intersect(which(ft$center == "IBSD"), 
                      which(as.numeric(ft$T3_Total) < 10))] = 
  as.numeric(ft$T3_Total[intersect(which(ft$center == "IBSD"), 
                                   which(as.numeric(ft$T3_Total) < 10))]) / 0.01536

ft$T3_Total[intersect(which(ft$center == "ILSB"), 
                      which(as.numeric(ft$T3_Total) < 10))] = 
  as.numeric(ft$T3_Total[intersect(which(ft$center == "ILSB"), 
                                   which(as.numeric(ft$T3_Total) < 10))]) / 0.01536



# Outlier resolution ------------------------------------------------------

#ft$RBS[which(ft$center == "CBRI" & ft$RBS == 0)] <- NA

ft$RBS[which(ft$RBS == 0)] <- NA
ft$FBS_Fasting_Blood_Glucose[which(ft$FBS_Fasting_Blood_Glucose == 0)] <- NA
ft$FBS_Fasting_Blood_Glucose[which(ft$FBS_Fasting_Blood_Glucose < 20)] <- NA
ft$HbA1C_Glycosylated_Haemoglobin[ft$HbA1C_Glycosylated_Haemoglobin<0] = NA
ft$name_dob_1.age_on_interview[which(ft$name_dob_1.age_on_interview<18)] = NA
#ft$Estimated_Glomerular_filtration_rate[which(ft$Estimated_Glomerular_filtration_rate==0)] = NA
ft$Creatinine[ft$Creatinine==0] = NA
ft$Total_Bilirubin[ft$Total_Bilirubin<0] = NA
ft$Total_Bilirubin[ft$Total_Bilirubin>10] = NA
ft$A_G_Ratio_Albumin_Globulin[which(ft$A_G_Ratio_Albumin_Globulin==0)]=NA
ft$Absolute_Basophil_Count[which(ft$Absolute_Basophil_Count>10000)]=NA
ft$Absolute_Eosinophil_Count[which(ft$Absolute_Eosinophil_Count==0)]=NA
ft$Absolute_Eosinophil_Count[which(ft$Absolute_Eosinophil_Count>10000)]=NA
ft$Absolute_Lymphocyte_Count[which(ft$Absolute_Lymphocyte_Count==0)]=NA
ft$Absolute_Lymphocyte_Count[which(ft$Absolute_Lymphocyte_Count>10000)]=NA
ft$Absolute_Monocyte_Count[which(ft$Absolute_Monocyte_Count==0)]=NA
ft$Absolute_Monocyte_Count[which(ft$Absolute_Monocyte_Count>4000)]=NA
ft$Absolute_Neutrophil_Count[which(ft$Absolute_Neutrophil_Count==0)] = NA
ft$Absolute_Neutrophil_Count[which(ft$Absolute_Neutrophil_Count>40000)] = NA
ft$Albumin[which(ft$Albumin>100)] = NA
ft$ALT_SGPT[which(as.numeric(ft$ALT_SGPT)>5000)] = NA
ft$Alkaline_Phosphatase[which(as.numeric(ft$Alkaline_Phosphatase)>10000)] = NA
ft$anthropometry.body_fat[which(ft$anthropometry.body_fat==0)] = NA
ft$anthropometry.body_fat[which(ft$anthropometry.body_fat>100)] = NA
ft$anthropometry.dia_bp[which(as.numeric(ft$anthropometry.dia_bp)>500)] = NA
ft$anthropometry.glucose_mg_dl[which((ft$anthropometry.glucose_mg_dl==0))] = NA
ft$anthropometry.head_cir[which(ft$anthropometry.head_cir>100)] = NA
ft$anthropometry.head_cir[which(ft$anthropometry.head_cir==0)] = NA
ft$anthropometry.height[which(as.numeric(ft$anthropometry.height)>500)] = NA
ft$anthropometry.sys_bp[which(as.numeric(ft$anthropometry.sys_bp)>500)] = NA
ft$anthropometry.waist_cir[which(as.numeric(ft$anthropometry.waist_cir)>300)] = NA
ft$anthropometry.weight[which(as.numeric(ft$anthropometry.weight)==0)] = NA
ft$anthropometry.weight[which(as.numeric(ft$anthropometry.weight)>300)] = NA

ft$anthropometry.hip_cir[as.numeric(ft$anthropometry.hip_cir)>400] = NA

ft$Basophils[which(ft$center=='GBRC' & is.na(ft$Basophils))] = 0
ft$Basophils[which(as.numeric(ft$Basophils)>10)] = NA
ft$BUN[which(ft$BUN==0)]=NA
ft$BUN_Sr_creatinine[which(ft$BUN_Sr_creatinine==0)] = NA
ft$CHOL_HDL_Ratio[which(ft$CHOL_HDL_Ratio>20)] = NA
ft$CHOL_HDL_Ratio[which(ft$CHOL_HDL_Ratio==0)] = NA
ft$Cholesterol[which(as.numeric(ft$Cholesterol)>1000)] = NA
ft$Creatinine[which(ft$Creatinine>=10)] = NA
ft$Alkaline_Phosphatase[ft$Alkaline_Phosphatase==0]=NA
ft$Cholesterol[ft$Cholesterol==0]=NA
ft$Eosinophils[which(as.numeric(ft$Eosinophils)>100)] = NA
ft$Eosinophils[which(ft$Eosinophils==0)]= NA
ft$Estimated_Average_Glucose[which(ft$Estimated_Average_Glucose==0)]=NA
ft$Estimated_Average_Glucose[which(ft$Estimated_Average_Glucose>1000)]=NA
ft$Estimated_Glomerular_filtration_rate[which(ft$Estimated_Glomerular_filtration_rate==0)]=NA
ft$Free_T3[which(ft$Free_T3==0.00)]=NA
ft$Free_T4[which(ft$Free_T4==0)]=NA
ft$Gamma_GT_GGTP[which(ft$Gamma_GT_GTP==0)]=NA
ft$Globulin[which(ft$Globulin==0)]=NA
ft$Globulin[which(ft$Globulin>10)]=NA
#ft$GLYCOSYLATED_Hb_IFCC[which(ft$GLYCOSYLATED_Hb_IFCC==0)]=NA
ft$HB_Haemoglobin[which(ft$HB_Haemoglobin>25)]=NA
ft$HB_Haemoglobin[which(ft$HB_Haemoglobin<2)]=NA
ft$HbA1C_Glycosylated_Haemoglobin[which(ft$HbA1C_Glycosylated_Haemoglobin>50)] = NA
ft$HbA1C_Glycosylated_Haemoglobin[which(ft$HbA1C_Glycosylated_Haemoglobin==0)] = NA
ft$HDL[which(ft$HDL>500)] = NA
ft$Hematocrit[which(ft$Hematocrit>100)] = NA
ft$Hematocrit[which(ft$Hematocrit==0)] = NA
ft$Platelet_Count[which(ft$Platelet_Count < 10000)] = NA
ft$Platelet_Count[which(ft$Platelet_Count>1000000)]=NA
ft$LDL[which(ft$LDL==0)] = NA
ft$Lymphocytes[which(ft$Lymphocytes==0)] = NA
ft$LDL_HDL_Ratio[which(ft$LDL_HDL_Ratio == 0)] = NA
ft$LDL_HDL_Ratio[which(ft$LDL_HDL_Ratio > 100)] = NA
ft$MCH_Mean_Corpuscular_Hb[which(ft$MCH_Mean_Corpuscular_Hb > 5000)] = NA
ft$Monocytes[which(ft$Monocytes==0)] = NA
ft$MPV_Mean_Platelet_Volume[which(ft$MPV_Mean_Platelet_Volume==0)] = NA
ft$Neutrophils[which(ft$Neutrophils>100)] = NA
ft$NonHDL_Cholesterol[which(ft$NonHDL_Cholesterol==0)] = NA
ft$PCT[which(ft$PCT==0)]=NA
ft$PDW[which(ft$PDW==0)]=NA

ft$Protein[which(ft$Protein>30)] = NA
ft$RBC_Red_Blood_Cell_Count[ft$RBC_Red_Blood_Cell_Count>20] = NA
ft$RBC_Red_Blood_Cell_Count[ft$RBC_Red_Blood_Cell_Count==0] = NA
ft$RBS[which(ft$RBS==0)] = NA
ft$RDW_Red_Cell_Distribution_Width[which(ft$RDW_Red_Cell_Distribution_Width==0)] = NA
ft$RDW_Red_Cell_Distribution_Width[which(ft$RDW_Red_Cell_Distribution_Width>100)] = NA
ft$RDW_SD[which(ft$RDW_SD==0)] = NA
ft$Sodium[which(ft$Sodium>1000)] = NA
ft$T3_Total[which(ft$T3_Total>10000)] = NA
ft$T4_Total[which(ft$T4_Total>50)] = NA
ft$Total_Bilirubin[which(ft$Total_Bilirubin>100)] = NA
ft$Total_Bilirubin[which(ft$Total_Bilirubin == 0)] = NA
ft$Urea[which(ft$Urea==0)] = NA
ft$Uric_Acid[which(ft$Uric_Acid==0)] = NA
ft$Uric_Acid[which(ft$Uric_Acid>100)] = NA
ft$WBC_Total_White_Blood_Cell_Count[which(ft$WBC_Total_White_Blood_Cell_Count>25000)]=NA
ft$WBC_Total_White_Blood_Cell_Count[which(ft$WBC_Total_White_Blood_Cell_Count<1000)]=NA
ft$Lymphocytes[ft$Lymphocytes==0] = NA
ft$Lymphocytes[ft$Lymphocytes>100] = NA
ft$MCH_Mean_Corpuscular_Hb[ft$MCH_Mean_Corpuscular_Hb>150] = NA
ft$MCHC_Mean_Corpuscular_Hb_Concn[ft$MCHC_Mean_Corpuscular_Hb_Concn==0] = NA
ft$MCHC_Mean_Corpuscular_Hb_Concn[ft$MCHC_Mean_Corpuscular_Hb_Concn>60] = NA
ft$socio_demographics.n_children[ft$socio_demographics.n_children>30] = NA
ft$socio_demographics.years_edu[ft$socio_demographics.years_edu>20] = NA
ft$LDL_HDL_Ratio[ft$LDL_HDL_Ratio>20] = NA
ft$anthropometry.hip_cir[ft$anthropometry.hip_cir>500] = NA
ft$PCT[ft$PCT>10] = NA
ft$ALT_SGPT[ft$ALT_SGPT>500] = NA
ft$LDL[ft$LDL>1000] = NA
ft$age[ft$age<18] = NA
ft$age_withBBC[ft$age_withBBC<18] = NA

ft$HbA1C_Glycosylated_Haemoglobin[ft$HbA1C_Glycosylated_Haemoglobin==0] = NA
ft$Hematocrit[ft$Hematocrit==0] = NA
ft$PDW[ft$PDW==0] = NA
ft$Vitamin_B12[ft$Vitamin_B12==0] = NA
ft$T3_Total[ft$T3_Total==0] = NA
ft$Homocysteine_Levels[ft$Homocysteine_Levels<0] = NA
ft$C.Peptide[ft$C.Peptide<0] = NA
ft$Fasting_Insulin_Level[ft$Fasting_Insulin_Level<0] = NA
ft$Lymphocytes[ft$Lymphocytes==0] = NA
ft$AST_SGOT[ft$AST_SGOT>500] = NA
ft$socio_demographics.marital_status[ft$socio_demographics.marital_status=="currently_currently_married"] = "currently_married"
ft$socio_demographics.marital_status[ft$socio_demographics.marital_status=="Married"] = "currently_married"
ft$socio_demographics.marital_status[ft$socio_demographics.marital_status=="MARRIED"] = "currently_married"
ft$socio_demographics.marital_status[ft$socio_demographics.marital_status=="Unmarried"] = "never_married"
ft$socio_demographics.marital_status[ft$socio_demographics.marital_status=="Widowed"] = "widowed"
ft$socio_demographics.marital_status[ft$socio_demographics.marital_status=="Divorced"] = "divorced_separated"
ft$Indirect_Bilirubin[ft$Indirect_Bilirubin<=0] = NA
ft$Indirect_Bilirubin[ft$Indirect_Bilirubin >10] = NA

ft$name_dob_1.ethnicity[ft$name_dob_1.ethnicity=="sikh"] = "sikhs"
# correcting languages ----------------------------------------------------

ft$name_dob_1.mother_tongue = tolower(ft$name_dob_1.mother_tongue)

# adding BMI --------------------------------------------------------------
ft$BMI = (ft$anthropometry.weight)/((ft$anthropometry.height/100)^2)
ft$BMI[ft$BMI>100] = NA
ft$BMI = round(ft$BMI, digits = 2)


# smoking-alcohol-tobacco -------------------------------------------------

ft$smoking_tobacco_alcohol.alcohol_status = tolower(ft$smoking_tobacco_alcohol.alcohol_status)
ft$smoking_tobacco_alcohol.chewing_tobacco_status = tolower(ft$smoking_tobacco_alcohol.chewing_tobacco_status)
ft$smoking_tobacco_alcohol.smoking_status = tolower(ft$smoking_tobacco_alcohol.smoking_status)
ft$smoking_tobacco_alcohol.alcohol_status[which(ft$smoking_tobacco_alcohol.alcohol_status=="na")]=NA
ft$smoking_tobacco_alcohol.alcohol_status[which(ft$smoking_tobacco_alcohol.alcohol_status=="yes")]="current"
ft$smoking_tobacco_alcohol.chewing_tobacco_status[which(ft$smoking_tobacco_alcohol.chewing_tobacco_status=="yes")]="current"
ft$smoking_tobacco_alcohol.smoking_status[which(ft$smoking_tobacco_alcohol.smoking_status=="yes")]="current"


# removing empty rows -----------------------------------------------------
ft = ft[-which(rowSums(is.na(ft))>115 & ft$center != "CBRI"),]

# check states at this point ----------------------------------------------

#whether all LocalIDs are in the correct state
#Classify the different centres in each state and look up if something looks off

# Sequenced samples -------------------------------------------------------

seq_samples = read.table("IDMappings_8964.csv", sep = ",", header = T, encoding = 'latin1')
seq_samples$LocalID <- sub("^AIIMS/", "AIIM/", seq_samples$LocalID)

seq_samples$centre = gsub("\\/.*","", seq_samples$LocalID)
seq_samples$ethnicity = tolower(seq_samples$ethnicity)
seq_samples$gender = tolower(seq_samples$gender)

merged_seq_samples1 = ft[(ft$LocalID %in% seq_samples$LocalID),]
#merged_seq_samples1$SeqID <- NA
#merged_seq_samples$ethnicity_seqfile = NA
#merged_seq_samples$gender_seqfile = NA


# Duplicate resolution 1 --------------------------------------- --------
newdups = merged_seq_samples1[duplicated(merged_seq_samples1$LocalID)|duplicated(merged_seq_samples1$LocalID, fromLast = T),]
#df[duplicated(df$a)|duplicated(df$a, fromLast=TRUE),]
#write.table(newdups, "New Duplicates.txt", sep = '\t', row.names = F)

merged_seq_samples1 = merged_seq_samples1[!(merged_seq_samples1$LocalID %in% newdups$LocalID),]



# Putting back some resolved dups -----------------------------------------

resolveddups = read_xlsx("New Duplicates.xlsx")
resolveddups = subset(resolveddups, select = -Transferrin)
resolveddups$anthropometry.height = as.numeric(resolveddups$anthropometry.height)
resolveddups$anthropometry.weight = as.numeric(resolveddups$anthropometry.weight)
resolveddups$BMI = (resolveddups$anthropometry.weight)/(((resolveddups$anthropometry.height)/100)^2)
resolveddups$BMI[resolveddups$BMI>100] = NA
resolveddups$BMI = round(resolveddups$BMI, digits = 2)
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="agrawal"] = "aggarwal"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="ansari"] = "ansari_sunni"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="audichya sahastra"] = "audichya_sahastra"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="bhil meena"] = "bhil_meena"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="chik baraik"] = "chik_baraik"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="desastha brahmin"] = "deshastha_brahmin"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="dongri bhil"] = "dongri_bhil"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="garhwali brahmin"] = "garhwali_brahmin"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="gujjar and bakarwal"] = "gujjar_and_bakkarwal"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="iyangar"] = "iyengar"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="jat"] = "jats"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="kannet"] = "kannets"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="kanyakubj brahmin"] = "kanyakubj_brahmin"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="kashmiri/muslim"] = "kashmiri_muslim"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="kashmiri/pandit"] = "kashmiri_pandit"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="khatri"] = "khatri_pb"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="kokanastha brahmin"] = "konkonastha_brahmin"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="koli"] = "kolis"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="kurmi mahato"] = "kudmi_mahato"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="kuruma"] = "kuruman"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="lingayats"] = "lingayath"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="NA"] = NA
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="nambudri brahmin"] = "namboodari"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="oriya brahmin"] = "oriya_brahmin"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="rajbanshi"] = "rajbangshi"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="saryuparin brahmin"] = "saryuparin_brahmin"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="sikh"] = "sikhs"
resolveddups$name_dob_1.ethnicity[resolveddups$name_dob_1.ethnicity=="vaidiki brahmin"] = "vaidiki_brahmin"
resolveddups<-resolveddups[names(merged_seq_samples1)]

resolveddups$FBS_Fasting_Blood_Glucose = as.numeric(resolveddups$FBS_Fasting_Blood_Glucose)
resolveddups$HbA1C_Glycosylated_Haemoglobin = as.numeric(resolveddups$HbA1C_Glycosylated_Haemoglobin)
#resolveddups$HbA1C_Glycosylated_Haemoglobin[resolveddups$HbA1C_Glycosylated_Haemoglobin == 0] = NA
#merged_df_ft1 = read.table("Added_addnlt_Apr19.txt", sep = "\t", header = T, fill= T, encoding = 'latin1')
resolveddups$Urea = as.numeric(resolveddups$Urea)
resolveddups$Creatinine = as.numeric(resolveddups$Creatinine)
resolveddups$Total_Bilirubin = as.numeric(resolveddups$Total_Bilirubin)
resolveddups$ALT_SGPT = as.numeric(resolveddups$ALT_SGPT)
resolveddups$AST_SGOT = as.numeric(resolveddups$AST_SGOT)
resolveddups$Alkaline_Phosphatase = as.numeric(resolveddups$Alkaline_Phosphatase)
resolveddups$Cholesterol = as.numeric(resolveddups$Cholesterol)
resolveddups$Triglycerides = as.numeric(resolveddups$Triglycerides)
resolveddups$HDL = as.numeric(resolveddups$HDL)
resolveddups$LDL = as.numeric(resolveddups$LDL)
resolveddups$T3_Total = as.numeric(resolveddups$T3_Total)
resolveddups$T4_Total = as.numeric(resolveddups$T4_Total)
resolveddups$TSH = as.numeric(resolveddups$TSH)
resolveddups$Homocysteine_Levels = as.numeric(resolveddups$Homocysteine_Levels)
resolveddups$Vitamin_B12 = as.numeric(resolveddups$Vitamin_B12)
resolveddups$Folic_Acid = as.numeric(resolveddups$Folic_Acid)
resolveddups$Fasting_Insulin_Level = as.numeric(resolveddups$Fasting_Insulin_Level)
resolveddups$C.Peptide = as.numeric(resolveddups$C.Peptide)
resolveddups$HB_Haemoglobin = as.numeric(resolveddups$HB_Haemoglobin)
resolveddups$RBC_Red_Blood_Cell_Count = as.numeric(resolveddups$RBC_Red_Blood_Cell_Count) #this is probably in millions
resolveddups$MCH_Mean_Corpuscular_Hb = as.numeric(resolveddups$MCH_Mean_Corpuscular_Hb)
resolveddups$MCHC_Mean_Corpuscular_Hb_Concn = as.numeric(resolveddups$MCHC_Mean_Corpuscular_Hb_Concn)
resolveddups$MCV_Mean_Corpuscular_Volume = as.numeric(resolveddups$MCV_Mean_Corpuscular_Volume)
resolveddups$RDW_Red_Cell_Distribution_Width = as.numeric(resolveddups$RDW_Red_Cell_Distribution_Width)
resolveddups$WBC_Total_White_Blood_Cell_Count = as.numeric(resolveddups$WBC_Total_White_Blood_Cell_Count)
resolveddups$Basophils = as.numeric(resolveddups$Basophils)
resolveddups$Eosinophils = as.numeric(resolveddups$Eosinophils)
resolveddups$Lymphocytes = as.numeric(resolveddups$Lymphocytes)
resolveddups$Monocytes = as.numeric(resolveddups$Monocytes)
resolveddups$Neutrophils = as.numeric(resolveddups$Neutrophils)
resolveddups$Platelet_Count = as.numeric(resolveddups$Platelet_Count)
resolveddups$A_G_Ratio_Albumin_Globulin = as.numeric(resolveddups$A_G_Ratio_Albumin_Globulin)
resolveddups$Absolute_Neutrophil_Count = as.numeric(resolveddups$Absolute_Neutrophil_Count)
resolveddups$Absolute_Basophil_Count = as.numeric(resolveddups$Absolute_Basophil_Count)
resolveddups$Absolute_Eosinophil_Count = as.numeric(resolveddups$Absolute_Eosinophil_Count)
resolveddups$Absolute_Lymphocyte_Count = as.numeric(resolveddups$Absolute_Lymphocyte_Count)
resolveddups$Absolute_Monocyte_Count = as.numeric(resolveddups$Absolute_Monocyte_Count)
resolveddups$CHOL_HDL_Ratio = as.numeric(resolveddups$CHOL_HDL_Ratio)
resolveddups$Direct_Bilirubin = as.numeric(resolveddups$Direct_Bilirubin)
resolveddups$ESR = as.numeric(resolveddups$ESR)
resolveddups$Estimated_Average_Glucose = as.numeric(resolveddups$Estimated_Average_Glucose)
resolveddups$Gamma_GT_GGTP = as.numeric(resolveddups$Gamma_GT_GGTP)
resolveddups$Globulin = as.numeric(resolveddups$Globulin)
resolveddups$Indirect_Bilirubin = as.numeric(resolveddups$Indirect_Bilirubin)
resolveddups$LDL_HDL_Ratio = as.numeric(resolveddups$LDL_HDL_Ratio)
resolveddups$Hematocrit = as.numeric(resolveddups$Hematocrit)
resolveddups$Albumin = as.numeric(resolveddups$Albumin)
resolveddups$Protein = as.numeric(resolveddups$Protein)
resolveddups$VLDL = as.numeric(resolveddups$VLDL)
resolveddups$RDW_SD = as.numeric(resolveddups$RDW_SD)
resolveddups$CRP = as.numeric(resolveddups$CRP)
resolveddups$MPV_Mean_Platelet_Volume = as.numeric(resolveddups$MPV_Mean_Platelet_Volume)
resolveddups$Immature_granulocyte_perc = as.numeric(resolveddups$Immature_granulocyte_perc)
#resolveddups$IG_0.0.3 = as.numeric(resolveddups$IG_0.0.3)
resolveddups$PDW = as.numeric(resolveddups$PDW)
resolveddups$PLCR = as.numeric(resolveddups$PLCR)
resolveddups$PCT = as.numeric(resolveddups$PCT)
resolveddups$RBS = as.numeric(resolveddups$RBS)
resolveddups$GLYCOSYLATED_Hb_IFCC = as.numeric(resolveddups$GLYCOSYLATED_Hb_IFCC)
resolveddups$BUN = as.numeric(resolveddups$BUN)
resolveddups$BUN_Sr_creatinine = as.numeric(resolveddups$BUN_Sr_creatinine)
resolveddups$Uric_Acid = as.numeric(resolveddups$Uric_Acid)
resolveddups$Estimated_Glomerular_filtration_rate = as.numeric(resolveddups$Estimated_Glomerular_filtration_rate)
resolveddups$NonHDL_Cholesterol = as.numeric(resolveddups$NonHDL_Cholesterol)
resolveddups$Vitamin_D_25_Hydroxy = as.numeric(resolveddups$Vitamin_D_25_Hydroxy)
resolveddups$Free_T3 = as.numeric(resolveddups$Free_T3)
resolveddups$Free_T4 = as.numeric(resolveddups$Free_T4)
resolveddups$Hs.CRP_High_Sensitivity_CRP = as.numeric(resolveddups$Hs.CRP_High_Sensitivity_CRP)
#resolveddups$Transferrin = as.numeric(resolveddups$Transferrin)
resolveddups$Polymorphs = as.numeric(resolveddups$Polymorphs)
resolveddups$Sodium = as.numeric(resolveddups$Sodium)
resolveddups$Potassium = as.numeric(resolveddups$Potassium)
resolveddups$SerumIronStudy_TIBC_UIBC = as.numeric(resolveddups$SerumIronStudy_TIBC_UIBC)
resolveddups$Transferrin_Saturation = as.numeric(resolveddups$Transferrin_Saturation)
resolveddups$Immature_granulocytes = as.numeric(resolveddups$Immature_granulocytes)
resolveddups$LDH_1 = as.numeric(resolveddups$LDH_1)
resolveddups$Total_Calcium = as.numeric(resolveddups$Total_Calcium)
resolveddups$Serum_Iron = as.numeric(resolveddups$Serum_Iron)
resolveddups$MENTZ1 = as.numeric(resolveddups$MENTZ1)
resolveddups$NLR_4 = as.numeric(resolveddups$NLR_4)
resolveddups$PO4_mg.dl = as.numeric(resolveddups$PO4_mg.dl)
resolveddups$Cl._mEq.L = as.numeric(resolveddups$Cl._mEq.L)
resolveddups$SGOT_SGPT = as.numeric(resolveddups$SGOT_SGPT)
resolveddups$CHOL.LDL_Ratio = as.numeric(resolveddups$CHOL.LDL_Ratio)
resolveddups$APB. = as.numeric(resolveddups$APB.)
resolveddups$APOA = as.numeric(resolveddups$APOA)
resolveddups$APOB = as.numeric(resolveddups$APOB)
resolveddups$LPA = as.numeric(resolveddups$LPA)
resolveddups$anthropometry.head_cir = as.numeric(resolveddups$anthropometry.head_cir)
resolveddups$anthropometry.height = as.numeric(resolveddups$anthropometry.height)
resolveddups$anthropometry.hip_cir = as.numeric(resolveddups$anthropometry.hip_cir)
resolveddups$anthropometry.sys_bp = as.numeric(resolveddups$anthropometry.sys_bp)
resolveddups$anthropometry.dia_bp = as.numeric(resolveddups$anthropometry.dia_bp)
resolveddups$anthropometry.body_fat = as.numeric(resolveddups$anthropometry.body_fat)
resolveddups$anthropometry.waist_cir = as.numeric(resolveddups$anthropometry.waist_cir)
resolveddups$anthropometry.weight = as.numeric(resolveddups$anthropometry.weight)
resolveddups$anthropometry.glucose_mg_dl = as.numeric(resolveddups$anthropometry.glucose_mg_dl)
resolveddups$Granulocytes = as.numeric(resolveddups$Granulocytes)
resolveddups$Granulocyte_count = as.numeric(resolveddups$Granulocyte_count)
resolveddups$MID = as.numeric(resolveddups$MID)
resolveddups$MID_Percent = as.numeric(resolveddups$MID_Percent)
resolveddups$age = as.numeric(resolveddups$age)
resolveddups$age_withBBC = as.numeric(resolveddups$age_withBBC)
resolveddups = resolveddups[-which(rowSums(is.na(resolveddups))>90),]


merged_seq_samples1 = rbind(merged_seq_samples1, resolveddups)

# How did these samples go missing? ---------------------------------------

merged_seq_samples = read.table("SequencedSamples_May23.txt", sep = "\t", header = T)

missedsamples = merged_seq_samples[merged_seq_samples$LocalID %in% setdiff(merged_seq_samples$LocalID, merged_seq_samples1$LocalID),]
missedsamples = missedsamples[-which(missedsamples$center=="SKIM"),]

missedsamples = missedsamples[,!names(missedsamples) %in% (setdiff(colnames(missedsamples), colnames(merged_seq_samples1)))]

new_cols = c(setdiff(colnames(merged_seq_samples1), colnames(missedsamples)))
library(dplyr)
missedsamples = missedsamples %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

missedsamples<-missedsamples[names(merged_seq_samples1)]
missedsamples$name_dob_1.ethnicity[missedsamples$name_dob_1.ethnicity=="agrawal"] = "aggarwal"
missedsamples$name_dob_1.ethnicity[missedsamples$name_dob_1.ethnicity=="jat"] = "jats"
missedsamples$name_dob_1.ethnicity[missedsamples$name_dob_1.ethnicity=="kannet"] = "kannets"
missedsamples$name_dob_1.ethnicity[missedsamples$name_dob_1.ethnicity=="kokanastha_brahmin"] = "konkonastha_brahmin"

missedsamples$age = missedsamples$name_dob_1.age_on_interview
missedsamples$age = ifelse(is.na(missedsamples$age), missedsamples$name_dob_1.approx_age,
                           missedsamples$age)

missedsamples$age_withBBC = missedsamples$age

merged_seq_samples1 = rbind(merged_seq_samples1, missedsamples)


merged_seq_samples1$SeqID <- NA
merged_seq_samples1$ethnicity_seqfile = NA
merged_seq_samples1$gender_seqfile = NA

merged_seq_samples1$SeqID[merged_seq_samples1$LocalID %in% seq_samples$LocalID] <- seq_samples$SeqID[match(merged_seq_samples1$LocalID[merged_seq_samples1$LocalID %in% seq_samples$LocalID], seq_samples$LocalID)]
merged_seq_samples1$ethnicity_seqfile[merged_seq_samples1$LocalID %in% seq_samples$LocalID] <- seq_samples$ethnicity[match(merged_seq_samples1$LocalID[merged_seq_samples1$LocalID %in% seq_samples$LocalID], seq_samples$LocalID)]
merged_seq_samples1$gender_seqfile[merged_seq_samples1$LocalID %in% seq_samples$LocalID] <- seq_samples$gender[match(merged_seq_samples1$LocalID[merged_seq_samples1$LocalID %in% seq_samples$LocalID], seq_samples$LocalID)]

merged_seq_samples1$gender_seqfile[merged_seq_samples1$gender_seqfile=='xx'] = 'female'
merged_seq_samples1$gender_seqfile[merged_seq_samples1$gender_seqfile=='xy'] = 'male'


eth_neq = merged_seq_samples1[merged_seq_samples1$name_dob_1.ethnicity != merged_seq_samples1$ethnicity_seqfile,]
eth_neq = subset(eth_neq, select = c("name_dob_1.ethnicity", "ethnicity_seqfile", "LocalID", "center"))

eth_neq = eth_neq[-which(is.na(eth_neq$LocalID)),]
eth_neq = eth_neq[-which(eth_neq$ethnicity_seqfile=="kholis"),]
eth_neq = eth_neq[-which(eth_neq$ethnicity_seqfile=="kol"),]

eth_neq$correct = eth_neq$ethnicity_seqfile
eth_neq$correct[eth_neq$correct=="kashmiri_pandit"] = "kashmiri_muslim"
eth_neq$correct[eth_neq$LocalID== "RGCB/L/005207"] = "thiya"
eth_neq$correct[eth_neq$LocalID== "RGCB/L/005212"] = "thiya"
merged_seq_samples1$name_dob_1.ethnicity[merged_seq_samples1$LocalID %in% eth_neq$LocalID] <- eth_neq$correct[match(merged_seq_samples1$LocalID[merged_seq_samples1$LocalID %in% eth_neq$LocalID], eth_neq$LocalID)]


# filling in missing ethnicities in freeze w 10000 samples ethnici --------

merged_seq_samples1$name_dob_1.ethnicity = ifelse(is.na(merged_seq_samples1$name_dob_1.ethnicity),
                                                  merged_seq_samples1$ethnicity_seqfile, 
                                                  merged_seq_samples1$name_dob_1.ethnicity)


# removing BBC missing samples --------------------------------------------

merged_seq_samples1 = merged_seq_samples1[-which(rowSums(is.na(merged_seq_samples1))>115),]
merged_seq_samples1 = merged_seq_samples1[-which(merged_seq_samples1$LocalID=="ILSB/H/000093"),]
merged_seq_samples1 = merged_seq_samples1[-which(merged_seq_samples1$LocalID=="ILSB/H/000472"),]
merged_seq_samples1 = merged_seq_samples1[-which(merged_seq_samples1$LocalID=="SKIM/M/000356"),]

merged_seq_samples1$name_dob_1.gender = tolower(merged_seq_samples1$name_dob_1.gender)

merged_seq_samples1$region[merged_seq_samples1$region=="Northeast"] = "North-East"
merged_seq_samples1$region[merged_seq_samples1$region=="Western"] = "West"

# removing required columns -----------------------------------------------


gender_neq = merged_seq_samples1[tolower(merged_seq_samples1$name_dob_1.gender) != merged_seq_samples1$gender_seqfile,]
gender_neq = subset(gender_neq, select = c("name_dob_1.gender", "gender_seqfile", "LocalID", "center"))

gender_neq = gender_neq[-which(is.na(gender_neq$LocalID)),]

merged_seq_samples1$name_dob_1.gender = merged_seq_samples1$gender_seqfile

merged_seq_samples1 = subset(merged_seq_samples1, select = -ethnicity_seqfile)
merged_seq_samples1 = subset(merged_seq_samples1, select = -gender_seqfile)
#merged_seq_samples1 = subset(merged_seq_samples1, select = -Age)

totrackdown = seq_samples[!(seq_samples$LocalID %in% merged_seq_samples1$LocalID),]



# checking earlier version ------------------------------------------------

totrackdownold = read_excel("to track down May 24.xlsx")


# Ethnicity coding --------------------------------------------------------


# Removing Hakkipikki-Halakki ---------------------------------------------
merged_seq_samples1 <- subset(merged_seq_samples1, name_dob_1.ethnicity != "hakkipikki")
merged_seq_samples1 <- subset(merged_seq_samples1, name_dob_1.ethnicity != "halakki")

# Take top 25 variables ---------------------------------------------------

empty_cols = sort(colSums(is.na(merged_seq_samples1)|merged_seq_samples1==""))
empty_cols = data.frame(Column = names(empty_cols), missing_values = empty_cols)
rownames(empty_cols)=NULL
empty_cols$missing_values_perc = (empty_cols$missing_values/length(merged_seq_samples1$LocalID))*100

empty_cols <- empty_cols %>% mutate(across(c('missing_values_perc'), round, 2))

empty_cols$label = NA
empty_cols$label[empty_cols$Column %in% colnames(odk_main)] = "ODK"
empty_cols$label[empty_cols$Column == "anthropometry.waist_cir"] = "ODK"
empty_cols$label[empty_cols$Column == "region"] = "meta"
empty_cols$label[empty_cols$Column == "center"] = "ODK"
empty_cols$label[empty_cols$Column == "BMI"] = "ODK"
empty_cols$label[empty_cols$Column == "age"] = "ODK"
empty_cols$label[empty_cols$Column == "age_withBBC"] = "ODK"
empty_cols$label[empty_cols$Column == "LocalID"] = "Key"
empty_cols$label[empty_cols$Column == "SeqID"] = "Key"
empty_cols$label[!(empty_cols$label %in% c("Key", "ODK", "meta"))] = "BBC"


merged_seq_samples1_cp = merged_seq_samples1

odk_variables = empty_cols[empty_cols$label %in% c("Key","ODK","meta"),]
odk_variables = subset(odk_variables, select = Column)
odk_variables = as.list(odk_variables)
merged_seq_samples1_01 = merged_seq_samples1_cp[, names(merged_seq_samples1_cp) %in% odk_variables$Column]

BBC_variables = empty_cols[empty_cols$label %in% c("BBC"),]
BBC_variables = BBC_variables[1:25,]
BBC_variables = as.list(subset(BBC_variables, select = Column))
BBC_variables_to_add = c("RBS", "FBS_Fasting_Blood_Glucose")
BBC_variables$Column = append(BBC_variables$Column, BBC_variables_to_add)
merged_seq_samples1_02 = merged_seq_samples1_cp[, names(merged_seq_samples1_cp) %in% BBC_variables$Column]

merged_seq_samples_joined = cbind(merged_seq_samples1_01, merged_seq_samples1_02)


# recoded ethnicities -----------------------------------------------------
b = read.table("populations.txt", header=T,stringsAsFactors = F, sep="\t")
#b = trimws(b)
popn_info = merged_seq_samples_joined[,c('LocalID', 'SeqID', 'name_dob_1.ethnicity', 'name_dob_1.state', 'region')]
b$Population = tolower(b$Population)

popn_info_merged = merge(popn_info, b, by.x=3, by.y=1,sort=F,all.x=T)
popn_info_merged$region[which(popn_info_merged$region == "Unknown")] = 
  popn_info_merged$Region[which(popn_info_merged$region == 'Unknown')]

popn_info_merged$Region = trimws(popn_info_merged$Region)

popn_info_merged = unique(popn_info_merged)

popn_info_merged$Region[popn_info_merged$name_dob_1.state=="rajasthan"] = "North"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="delhi"] = "North"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="new delhi"] = "North"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="haryana"] = "North"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="karnataka"] = "South"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="madya_pradesh"] = "Central"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="uttaranchal"] = "Central"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="uttar_pradesh"] = "Central"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="himachal_pradesh"] = "North"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="gujarat"] = "West"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="maharashtra"] = "West"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="west_bengal"] = "East"
popn_info_merged$Region[popn_info_merged$name_dob_1.state=="jharkhand"] = "East"
popn_info_merged = unique(popn_info_merged)

popn_info_merged$regioncode = "N"
popn_info_merged$regioncode[which(popn_info_merged$region == "South")] = "S"
popn_info_merged$regioncode[which(popn_info_merged$region == "West")] = "W"
popn_info_merged$regioncode[which(popn_info_merged$region == "East")] = "E"
popn_info_merged$regioncode[which(popn_info_merged$region == "North-East")] = "NE"
popn_info_merged$regioncode[which(popn_info_merged$region == "Central")] = "C"

popn_info_merged$ID = paste0(popn_info_merged$regioncode, "-", popn_info_merged$Language.Family, "-", popn_info_merged$Region)

ethnicities = unique(popn_info_merged$name_dob_1.ethnicity)
ethmap = data.frame(ethnicities, 1:length(ethnicities))
rownames(ethmap) = ethmap[,1]

popn_info_merged$Mapping = NA
for(id in unique(popn_info_merged$ID)){
  rowstoedit = which(popn_info_merged$ID == id)
  eth = popn_info_merged[rowstoedit, 'name_dob_1.ethnicity']
  eth2 = ethmap[eth,2]
  nums = c(1:length(rowstoedit))
  newids = paste0(popn_info_merged$ID[rowstoedit],"-",eth2)
  popn_info_merged$Mapping[rowstoedit] = newids
}

write.table(popn_info_merged, file="seqid_mappings.txt", quote=F,row.names=F,sep="\t")

mappingslist = read.table("seqid_mappings.txt", sep = '\t', header = T)
merged_seq_samples_joined$ethnicity_mapping[merged_seq_samples_joined$SeqID %in% mappingslist$SeqID] <- mappingslist$Mapping[match(merged_seq_samples_joined$SeqID[merged_seq_samples_joined$SeqID %in% mappingslist$SeqID], mappingslist$SeqID)]
#colnames(merged_seq_samples_joined_cp1)[colnames(merged_seq_samples_joined_cp1) == "mapping"] = "ethnicity_mapping"


merged_seq_samples_joined$region_list[merged_seq_samples_joined$SeqID %in% mappingslist$SeqID] <- mappingslist$region[match(merged_seq_samples_joined$SeqID[merged_seq_samples_joined$SeqID %in% mappingslist$SeqID], mappingslist$SeqID)]

region_check = merged_seq_samples_joined[merged_seq_samples_joined$region != merged_seq_samples_joined$region_list,]
region_check = subset(merged_seq_samples_joined, select = c("SeqID", "name_dob_1.ethnicity", "name_dob_1.state", "region", "region_list"))
region_check = region_check[region_check$region != region_check$region_list,]
region_check = subset(region_check, region!="Unknown")
#region_check = subset(region_check, region_list!="Northeast")

merged_seq_samples_joined = subset(merged_seq_samples_joined, select = -region)
names(merged_seq_samples_joined)[names(merged_seq_samples_joined) == "region_list"] = "region"

# Export files (remove Age!) ------------------------------------------------------------

finalsamplelist = read.table("GI_QCd_sample_ids.txt")

merged_seq_samples_joined_cp1 = merged_seq_samples_joined[merged_seq_samples_joined$SeqID %in% finalsamplelist$V1,]
merged_seq_samples_joined_cp1 = subset(merged_seq_samples_joined_cp1, select = -LocalID)
merged_seq_samples_joined_cp1 = merged_seq_samples_joined_cp1 %>% dplyr::select("SeqID", everything())
write.table(merged_seq_samples_joined_cp1, "GI_SequencedSamples_9385_Jul23.txt", sep = '\t', row.names = F)
merged_seq_samples_joined_cp1 = subset(merged_seq_samples_joined_cp1, select = -name_dob_1.ethnicity)
write.table(merged_seq_samples_joined_cp1, "GI_SequencedSamples_9385_Jul23_coded.txt", sep = '\t', row.names = F)

# Dist by center and ethnicity -----------------------------------------------
merge_num_new = dplyr::select_if(merged_seq_samples_joined_cp1, is.numeric)
pdf("distbycenter_top25_newlist_coded.pdf", width = 11)
variables = colnames(merge_num_new)
for(V in variables){
  if(length(unique(as.numeric(merge_num_new[,V]))) == 1){next;}
  boxplot(as.numeric(merge_num_new[,V]) ~ merged_seq_samples_joined_cp1$center, main = V, ylab = V, xlab = "Centre")
}

dev.off()

# plotting boxplots against ethnicity -------------------------------------
pdf("distbyethnicity_top25_newlist_coded.pdf", width = 11)
variables = colnames(merge_num_new)
for(V in variables){
  if(length(unique(as.numeric(merge_num_new[,V]))) == 1){next;}
  boxplot(as.numeric(merge_num_new[,V]) ~ merged_seq_samples_joined_cp1$ethnicity_mapping, main = V, las = 2, cex.axis = 0.3, ylab = V, xlab = "Ethnicity code")
}

dev.off()


# scatterplots ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(rlang)

# Start the PDF device
pdf("allscatterplots_top25_Jul23.pdf", width = 10, height = 10)

for (i in 1:(ncol(merge_num_new))) { 
  for (j in 1:(ncol(merge_num_new))) {
    if (i != j) {  # Avoid plotting a variable against itself
      # Check for enough complete observations
      if (sum(complete.cases(merge_num_new[, c(i, j)])) >= 3) {
        # Print the variable names on screen
        cat("Plotting:", names(merge_num_new)[i], "vs", names(merge_num_new)[j], "\n")
        
        # Calculate correlation coefficient
        correlation <- cor.test(merge_num_new[,i], merge_num_new[,j], method = "spearman")$estimate
        
        # Create the plot
        p <- ggplot(merge_num_new, aes_string(x = names(merge_num_new)[i], y = names(merge_num_new)[j])) +
          geom_point(aes(color = factor(merged_seq_samples_joined_cp1$center))) +
          scale_color_manual(values = c("#bfef45", "#800000", "#ffe119", "#f032e6",
                                        "#a9a9a9", "#e6194B", "#c19a6b", "#469990",
                                        "#aaffc3", "#911eb4", "#808000", "#fabed4",
                                        "#000075")) +
          geom_text(aes(label = paste("Correlation:", round(correlation, 2))), x = Inf, y = -Inf, hjust = 1, vjust = -1) +
          labs(title = paste(names(merge_num_new)[i], "vs", names(merge_num_new)[j]))+theme_bw()
        
        # Print the plot
        print(p)
      } else {
        warning("Not enough complete observations to calculate correlation for variables ", 
                names(merge_num_new)[i], " and ", names(merge_num_new)[j])
      }
      
    }
  }
}

# Close the PDF device
dev.off()

# putting values back in ft -----------------------------------------------

merged_seq_samples1_cp_cp = merged_seq_samples1
#merged_seq_samples1_cp_cp = subset(merged_seq_samples1_cp_cp, select = -BMI)
merged_seq_samples1_cp_cp = subset(merged_seq_samples1_cp_cp, select = -SeqID)
#merged_seq_samples1_cp_cp$name_dob_1.gender = merged_seq_samples1_cp_cp$g
new_cols = setdiff(colnames(ft), colnames(merged_seq_samples1_cp_cp))
library(dplyr)
seq_joined = merged_seq_samples1_cp_cp %>% 
  mutate(!!!setNames(rep(NA, length(new_cols)), new_cols))

seq_joined<-seq_joined[names(ft)]


ft_cp = ft
ft_cp = ft_cp[ -which(ft_cp$LocalID %in% seq_samples$LocalID),]

newdups = ft_cp[duplicated(ft_cp$LocalID)|duplicated(ft_cp$LocalID, fromLast = T),]
#df[duplicated(df$a)|duplicated(df$a, fromLast=TRUE),]
#write.table(newdups, "New Duplicates_20000_Jul15.txt", sep = '\t', row.names = F)

ft_cp = ft_cp[!(ft_cp$LocalID %in% newdups$LocalID),]

ft_cp = rbind(ft_cp, seq_joined)
ft_cp = subset(ft_cp, select = -Age)

ft_cp$FBS_Fasting_Blood_Glucose = as.numeric(ft_cp$FBS_Fasting_Blood_Glucose)
ft_cp$HbA1C_Glycosylated_Haemoglobin = as.numeric(ft_cp$HbA1C_Glycosylated_Haemoglobin)
#ft_cp$HbA1C_Glycosylated_Haemoglobin[ft_cp$HbA1C_Glycosylated_Haemoglobin == 0] = NA
#merged_df_ft1 = read.table("Added_addnlt_Apr19.txt", sep = "\t", header = T, fill= T, encoding = 'latin1')
ft_cp$Urea = as.numeric(ft_cp$Urea)
ft_cp$Creatinine = as.numeric(ft_cp$Creatinine)
ft_cp$Total_Bilirubin = as.numeric(ft_cp$Total_Bilirubin)
ft_cp$ALT_SGPT = as.numeric(ft_cp$ALT_SGPT)
ft_cp$AST_SGOT = as.numeric(ft_cp$AST_SGOT)
ft_cp$Alkaline_Phosphatase = as.numeric(ft_cp$Alkaline_Phosphatase)
ft_cp$Cholesterol = as.numeric(ft_cp$Cholesterol)
ft_cp$Triglycerides = as.numeric(ft_cp$Triglycerides)
ft_cp$HDL = as.numeric(ft_cp$HDL)
ft_cp$LDL = as.numeric(ft_cp$LDL)
ft_cp$T3_Total = as.numeric(ft_cp$T3_Total)
ft_cp$T4_Total = as.numeric(ft_cp$T4_Total)
ft_cp$TSH = as.numeric(ft_cp$TSH)
ft_cp$Homocysteine_Levels = as.numeric(ft_cp$Homocysteine_Levels)
ft_cp$Vitamin_B12 = as.numeric(ft_cp$Vitamin_B12)
ft_cp$Folic_Acid = as.numeric(ft_cp$Folic_Acid)
ft_cp$Fasting_Insulin_Level = as.numeric(ft_cp$Fasting_Insulin_Level)
ft_cp$C.Peptide = as.numeric(ft_cp$C.Peptide)
ft_cp$HB_Haemoglobin = as.numeric(ft_cp$HB_Haemoglobin)
ft_cp$RBC_Red_Blood_Cell_Count = as.numeric(ft_cp$RBC_Red_Blood_Cell_Count) #this is probably in millions
ft_cp$MCH_Mean_Corpuscular_Hb = as.numeric(ft_cp$MCH_Mean_Corpuscular_Hb)
ft_cp$MCHC_Mean_Corpuscular_Hb_Concn = as.numeric(ft_cp$MCHC_Mean_Corpuscular_Hb_Concn)
ft_cp$MCV_Mean_Corpuscular_Volume = as.numeric(ft_cp$MCV_Mean_Corpuscular_Volume)
ft_cp$RDW_Red_Cell_Distribution_Width = as.numeric(ft_cp$RDW_Red_Cell_Distribution_Width)
ft_cp$WBC_Total_White_Blood_Cell_Count = as.numeric(ft_cp$WBC_Total_White_Blood_Cell_Count)
ft_cp$Basophils = as.numeric(ft_cp$Basophils)
ft_cp$Eosinophils = as.numeric(ft_cp$Eosinophils)
ft_cp$Lymphocytes = as.numeric(ft_cp$Lymphocytes)
ft_cp$Monocytes = as.numeric(ft_cp$Monocytes)
ft_cp$Neutrophils = as.numeric(ft_cp$Neutrophils)
ft_cp$Platelet_Count = as.numeric(ft_cp$Platelet_Count)
ft_cp$A_G_Ratio_Albumin_Globulin = as.numeric(ft_cp$A_G_Ratio_Albumin_Globulin)
ft_cp$Absolute_Neutrophil_Count = as.numeric(ft_cp$Absolute_Neutrophil_Count)
ft_cp$Absolute_Basophil_Count = as.numeric(ft_cp$Absolute_Basophil_Count)
ft_cp$Absolute_Eosinophil_Count = as.numeric(ft_cp$Absolute_Eosinophil_Count)
ft_cp$Absolute_Lymphocyte_Count = as.numeric(ft_cp$Absolute_Lymphocyte_Count)
ft_cp$Absolute_Monocyte_Count = as.numeric(ft_cp$Absolute_Monocyte_Count)
ft_cp$CHOL_HDL_Ratio = as.numeric(ft_cp$CHOL_HDL_Ratio)
ft_cp$Direct_Bilirubin = as.numeric(ft_cp$Direct_Bilirubin)
ft_cp$ESR = as.numeric(ft_cp$ESR)
ft_cp$Estimated_Average_Glucose = as.numeric(ft_cp$Estimated_Average_Glucose)
ft_cp$Gamma_GT_GGTP = as.numeric(ft_cp$Gamma_GT_GGTP)
ft_cp$Globulin = as.numeric(ft_cp$Globulin)
ft_cp$Indirect_Bilirubin = as.numeric(ft_cp$Indirect_Bilirubin)
ft_cp$LDL_HDL_Ratio = as.numeric(ft_cp$LDL_HDL_Ratio)
ft_cp$Hematocrit = as.numeric(ft_cp$Hematocrit)
ft_cp$Albumin = as.numeric(ft_cp$Albumin)
ft_cp$Protein = as.numeric(ft_cp$Protein)
ft_cp$VLDL = as.numeric(ft_cp$VLDL)
ft_cp$RDW_SD = as.numeric(ft_cp$RDW_SD)
ft_cp$CRP = as.numeric(ft_cp$CRP)
ft_cp$MPV_Mean_Platelet_Volume = as.numeric(ft_cp$MPV_Mean_Platelet_Volume)
ft_cp$Immature_granulocyte_perc = as.numeric(ft_cp$Immature_granulocyte_perc)
#ft_cp$IG_0.0.3 = as.numeric(ft_cp$IG_0.0.3)
ft_cp$PDW = as.numeric(ft_cp$PDW)
ft_cp$PLCR = as.numeric(ft_cp$PLCR)
ft_cp$PCT = as.numeric(ft_cp$PCT)
ft_cp$RBS = as.numeric(ft_cp$RBS)
ft_cp$GLYCOSYLATED_Hb_IFCC = as.numeric(ft_cp$GLYCOSYLATED_Hb_IFCC)
ft_cp$BUN = as.numeric(ft_cp$BUN)
ft_cp$BUN_Sr_creatinine = as.numeric(ft_cp$BUN_Sr_creatinine)
ft_cp$Uric_Acid = as.numeric(ft_cp$Uric_Acid)
ft_cp$Estimated_Glomerular_filtration_rate = as.numeric(ft_cp$Estimated_Glomerular_filtration_rate)
ft_cp$NonHDL_Cholesterol = as.numeric(ft_cp$NonHDL_Cholesterol)
ft_cp$Vitamin_D_25_Hydroxy = as.numeric(ft_cp$Vitamin_D_25_Hydroxy)
ft_cp$Free_T3 = as.numeric(ft_cp$Free_T3)
ft_cp$Free_T4 = as.numeric(ft_cp$Free_T4)
ft_cp$Hs.CRP_High_Sensitivity_CRP = as.numeric(ft_cp$Hs.CRP_High_Sensitivity_CRP)
#ft_cp$Transferrin = as.numeric(ft_cp$Transferrin)
ft_cp$Polymorphs = as.numeric(ft_cp$Polymorphs)
ft_cp$Sodium = as.numeric(ft_cp$Sodium)
ft_cp$Potassium = as.numeric(ft_cp$Potassium)
ft_cp$SerumIronStudy_TIBC_UIBC = as.numeric(ft_cp$SerumIronStudy_TIBC_UIBC)
ft_cp$Transferrin_Saturation = as.numeric(ft_cp$Transferrin_Saturation)
ft_cp$Immature_granulocytes = as.numeric(ft_cp$Immature_granulocytes)
ft_cp$LDH_1 = as.numeric(ft_cp$LDH_1)
ft_cp$Total_Calcium = as.numeric(ft_cp$Total_Calcium)
ft_cp$Serum_Iron = as.numeric(ft_cp$Serum_Iron)
ft_cp$MENTZ1 = as.numeric(ft_cp$MENTZ1)
ft_cp$NLR_4 = as.numeric(ft_cp$NLR_4)
ft_cp$PO4_mg.dl = as.numeric(ft_cp$PO4_mg.dl)
ft_cp$Cl._mEq.L = as.numeric(ft_cp$Cl._mEq.L)
ft_cp$SGOT_SGPT = as.numeric(ft_cp$SGOT_SGPT)
ft_cp$CHOL.LDL_Ratio = as.numeric(ft_cp$CHOL.LDL_Ratio)
ft_cp$APB. = as.numeric(ft_cp$APB.)
ft_cp$APOA = as.numeric(ft_cp$APOA)
ft_cp$APOB = as.numeric(ft_cp$APOB)
ft_cp$LPA = as.numeric(ft_cp$LPA)
ft_cp$anthropometry.head_cir = as.numeric(ft_cp$anthropometry.head_cir)
ft_cp$anthropometry.height = as.numeric(ft_cp$anthropometry.height)
ft_cp$anthropometry.hip_cir = as.numeric(ft_cp$anthropometry.hip_cir)
ft_cp$anthropometry.sys_bp = as.numeric(ft_cp$anthropometry.sys_bp)
ft_cp$anthropometry.dia_bp = as.numeric(ft_cp$anthropometry.dia_bp)
ft_cp$anthropometry.body_fat = as.numeric(ft_cp$anthropometry.body_fat)
ft_cp$anthropometry.waist_cir = as.numeric(ft_cp$anthropometry.waist_cir)
ft_cp$anthropometry.weight = as.numeric(ft_cp$anthropometry.weight)
ft_cp$anthropometry.glucose_mg_dl = as.numeric(ft_cp$anthropometry.glucose_mg_dl)
ft_cp$Granulocytes = as.numeric(ft_cp$Granulocytes)
ft_cp$Granulocyte_count = as.numeric(ft_cp$Granulocyte_count)
ft_cp$MID = as.numeric(ft_cp$MID)
ft_cp$MID_Percent = as.numeric(ft_cp$MID_Percent)
ft_cp$age = as.numeric(ft_cp$age)
ft_cp$age_withBBC = as.numeric(ft_cp$age_withBBC)


ft_cp$smoking_tobacco_alcohol.alcohol_status = tolower(ft_cp$smoking_tobacco_alcohol.alcohol_status)
ft_cp$smoking_tobacco_alcohol.chewing_tobacco_status = tolower(ft_cp$smoking_tobacco_alcohol.chewing_tobacco_status)
ft_cp$smoking_tobacco_alcohol.smoking_status = tolower(ft_cp$smoking_tobacco_alcohol.smoking_status)

ft_cp$name_dob_1.gender = tolower(ft_cp$name_dob_1.gender)
# missingness -------------------------------------------------------------

empty_cols_20k = sort(colSums(is.na(ft_cp)|ft_cp==""))
empty_cols_20k = data.frame(Column = names(empty_cols_20k), missing_values = empty_cols_20k)
rownames(empty_cols_20k)=NULL
empty_cols_20k$missing_values_perc = (empty_cols_20k$missing_values/length(ft_cp$LocalID))*100

empty_cols_20k <- empty_cols_20k %>% mutate(across(c('missing_values_perc'), round, 2))


# adding resolved dups ----------------------------------------------------

resolved_dups = read_xlsx("resolveddups_20000.xlsx")
#resolved_dups = subset(resolved_dups, select = -Age)
resolved_dups = subset(resolved_dups, select = -Transferrin)
resolved_dups$anthropometry.height = as.numeric(resolved_dups$anthropometry.height)
resolved_dups$anthropometry.weight = as.numeric(resolved_dups$anthropometry.weight)
resolved_dups$BMI = resolved_dups$anthropometry.weight/((resolved_dups$anthropometry.height/100)^2)

resolved_dups<-resolved_dups[names(ft_cp)]

ft_cp = rbind(ft_cp, resolved_dups)

ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="dongri bhil"] = "dongri_bhil"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="gujjar and bakarwal"] = "gujjar_and_bakkarwal"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="kashmiri/muslim"] = "kashmiri_muslim"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="kashmiri/pandit"] = "kashmiri_pandit"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="garhwali brahmin"] = "garhwali_brahmin"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="saryuparin brahmin"] = "saryuparin_brahmin"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="oriya brahmin"] = "oriya_brahmin"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="nambudri brahmin"] = "namboodari"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="kuruma"] = "kuruman"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="kurmi mahato"] = "kudmi_mahato"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="bhil meena"] = "bhil_meena"
ft_cp$name_dob_1.ethnicity[ft_cp$name_dob_1.ethnicity=="agrawal"] = "aggarwal"

# Tracking missing samples ------------------------------------------------

list_of_samples = read_xlsx("Genome India BB data after duplication check.xlsx")

missingids = as.data.frame(setdiff(list_of_samples$`Local Id`, ft_cp$LocalID))
missingids$ODKmissingstatus = NA
colnames(missingids)[colnames(missingids) == "setdiff(list_of_samples$`Local Id`, ft_cp$LocalID)"] = "LocalID"

missingids$ODKmissingstatus[missingids$LocalID %in% odk_main$introduction.local_id_barcode] = "Present"
missingids$ODKmissingstatus[missingids$LocalID %in% odk_main$introduction_1.local_id_manual] = "Present"

write.table(missingids, "Missing_samples_20k_ODKmissing.txt", row.names = F)



# remove all samples with only BBC or only ODK ----------------------------

ft$nonmissing = rowSums(!is.na(ft))
ft = subset(ft, select = -nonmissing)


# writing file ------------------------------------------------------------

write.table(ft_cp, "20000freeze_Jul16.txt", sep = '\t', row.names = F)


# checking which samples are not there ------------------------------------

ft_old = read.table("20000_freeze.txt", sep = '\t', header = T)


# Tracking missing samples ------------------------------------------------

list_of_samples = read_xlsx("Genome India BB data after duplication check.xlsx")

missingids = as.data.frame(setdiff(list_of_samples$`Local Id`, ft_cp$LocalID))
missingids$ODKmissingstatus = NA
colnames(missingids)[colnames(missingids) == "setdiff(list_of_samples$`Local Id`, ft_cp$LocalID)"] = "LocalID"

missingids$ODKmissingstatus[missingids$LocalID %in% odk_main$introduction.local_id_barcode] = "Present"
missingids$ODKmissingstatus[missingids$LocalID %in% odk_main$introduction_1.local_id_manual] = "Present"

write.table(missingids, "Missing_samples_20k_ODKmissing_JUl16.txt", row.names = F)

