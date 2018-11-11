library(pacman)

pacman::p_load(data.table, ggplot2, xray, dplyr, healthcareai)

## Volumetry
path_file = "data/labevents.rda"
load(path_file)
dt_labs = labevents
dt_labs = setDT(dt_labs)

names(dt_labs)

head(dt_labs)

xray::anomalies(dt_labs[, c("SUBJECT_ID", "VALUE", "VALUENUM", "FLAG")])

## Labs master

path_file = "data/labs_master.csv"
dt_labs_m = fread(file = path_file)

# merge with masters
dt_labs$ITEMID
dt_labs_m$ITEMID

dt_labs_merge = merge(dt_labs, dt_labs_m, by = "ITEMID", all.x = TRUE)
names(dt_labs_merge)
xray::anomalies(dt_labs_merge[, c("SUBJECT_ID", "VALUE", "VALUENUM", "FLAG", "LABEL")])


dt_group = dt_labs_merge[, .(count = .N), by="LABEL"]
dt_group = dt_group[order(-count)]
dt_group

dt_group = dt_labs_merge[, .(count = .N), by=c("LABEL","ITEMID", "FLUID")]
dt_group = dt_group[order(-count)]
dt_group

g <- ggplot(dt_group[1:20], aes(x=reorder(LABEL, 
                                          count), 
                                     y=count, fill=LABEL))
g <- g + geom_bar(stat="identity") 
g <- g + geom_text(aes(label=paste0(count," - ",
                                     sprintf("%1.2f%%", (count/sum(dt_group$count))*100))), 
                    colour="black", hjust = 1) 
g <- g + labs(title=title, x=x, y=y)
g <- g + theme(title=element_text(size=13, face="bold"))
g <- g + theme(axis.title=element_text(size=13))
g <- g + theme(legend.position="none")
g <- g + coord_flip()
g <- g + theme(axis.text.y = element_text(size=12))
g

head(dt_labs)

head(dt_labs_merge)

dt_sub = dt_labs_merge[FLUID == "pO2" & VALUENUM < 200]

dt_sub = dt_labs_merge[ITEMID == "823"]
dt_sub

# Histogram
library(plotly)
ggplot(data=dt_sub, aes(x=VALUENUM)) + 
    geom_histogram(binwidth =3)

g = ggplot(data=dt_sub, aes(y=VALUENUM, x="1")) + 
    geom_boxplot()

library(plotly)
p <- plot_ly(dt_sub, x = ~VALUENUM, type = "box")
p

dt_sub = dt_labs_merge[LABEL == "Hematocrit"]

dt_labs_merge[grep("Leu", LABEL), ]

dt_sub = dt_labs_merge[FLUID == "Urine"]

#dt_sub = dt_labs_merge[LABEL == "Leukocytes"]
dt_group = dt_sub[, .(count = .N), by=c("LABEL","ITEMID", "FLUID")]
dt_group = dt_group[order(-count)]
dt_group

head(dt_labs)

# load admissions

path_file = "data/admissions.rda"
load(file = path_file)
admissions = setDT(admissions)
str(admissions)

admissions

# load icus
path_file = "data/icustays.rda"
load(file = path_file)
icustays = setDT(icustays)

# merge admissions and icu
names(admissions)
admissions[, c("SUBJECT_ID", "HADM_ID", "ADMITTIME", "DISCHTIME")]

names(icustays)
head(icustays)
icustays[, c("SUBJECT_ID", "HADM_ID", "ICUSTAY_ID", "INTIME", "OUTTIME")]

icu_adm = merge(icustays[, c("SUBJECT_ID", "HADM_ID", "ICUSTAY_ID", "INTIME", "OUTTIME")], 
                admissions[, c("SUBJECT_ID", "HADM_ID", "ADMITTIME", "DISCHTIME")], 
                by = c("SUBJECT_ID", "HADM_ID"), all.x = TRUE)

icu_adm[, diff := ADMITTIME - INTIME]
icu_adm


icustays[, c("SUBJECT_ID", "HADM_ID", "ICUSTAY_ID", "INTIME", "OUTTIME")]

icustays[SUBJECT_ID == 85976 & HADM_ID == 192494]

dt_sub = dt_labs_merge[ITEMID == "50813"]

dt_sub = dt_sub[order(SUBJECT_ID, HADM_ID)]

dt_group = dt_sub[, .(count = .N), by=c("SUBJECT_ID", "HADM_ID")]
dt_group = dt_group[order(-count)]

one = icustays[SUBJECT_ID == 85976 & , c("SUBJECT_ID", "HADM_ID", "ICUSTAY_ID", "INTIME", "OUTTIME")]

# load icus
path_file = "data/akicreatinine.rda"
load(file = path_file)
akicreatinine = setDT(akicreatinine)

dummy = dt_sub[SUBJECT_ID == 85976]

dummy_merge = merge(dummy, one, by = c("SUBJECT_ID"), all.x = TRUE)
dummy_merge[, diff := CHARTTIME - INTIME]
dummy_merge[, diff_hour := diff / 60]
dummy_merge[, point := abs(diff_hour - 24)]
dummy_merge = dummy_merge[order(point)]
dummy_merge[1, VALUENUM]

dummy_merge[1, VALUENUM]


icu_vars = icustays[SUBJECT_ID == 85976 & HADM_ID == 192494, c("SUBJECT_ID", "HADM_ID", "ICUSTAY_ID", "INTIME", "OUTTIME")]

######################################################################################
# load data and labs
path_file = "data/akicreatinineplus.rda"
load(file = path_file)
akicreatinine = setDT(akicreatinineplus)
head(akicreatinine)

akicreatinine[is.na(vaso_rate), vaso_rate := 0]
akicreatinine$vaso_rate

# load labs
path_file = "data/dt_labs.rds"
dt_labs = readRDS(file = path_file)
dt_labs = setDT(dt_labs)

aki_labs = merge(dt_labs, akicreatinine, by = c("SUBJECT_ID", "HADM_ID"), all.y = TRUE)

create_lab = function(cod_lab, name_lab) {

    icu_labs_su = aki_labs[ITEMID == cod_lab]
    icu_labs_su[, diff := CHARTTIME - INTIME]
    icu_labs_su[, diff_hour := diff / 60]
    icu_labs_su[, point := abs(diff_hour - 24)]
    icu_labs_gr = icu_labs_su[, .(min = min(point)), by=c("SUBJECT_ID", "HADM_ID", "VALUENUM")]
    icu_labs_gr = icu_labs_gr[order(SUBJECT_ID, HADM_ID, min)]
    icu_labs_gr[, rank := 1:.N, by=c("SUBJECT_ID", "HADM_ID")]
    icu_labs_gr = icu_labs_gr[rank == 1]
    icu_labs_gr = icu_labs_gr[, c("SUBJECT_ID", "HADM_ID", "VALUENUM")]
    names(icu_labs_gr)[3] = name_lab
                
    akicreatinine_full = merge(akicreatinine_full,
                               icu_labs_gr,
                               by = c("SUBJECT_ID", "HADM_ID"),
                               all.x = TRUE)
    return (akicreatinine_full)
}

# add new data set

akicreatinine_full = akicreatinine

# keys
keys <- list(c(50813, "Lactate"),
             c(51301, "White_Blood_Cells"),     
             c(51221, "Hematocrit"),
             c(51265, "Platelet_Count"),
             c(51222, "Hemoglobin"),
             c(50820, "pH_blood"),
             c(50885, "Bilirubin_Total"),
             c(50862, "Albumin"),
             c(50954, "Lactate_Dehydrogenase_(LD)"),
             c(51218, "Granulocyte_Count"),
             c(50909, "Cortisol"),
             c(50983, "Sodium"),
             c(50971, "Potassium"),
             c(440714, "Anion_Gap"),
             c(51491, "pH_urine"),
             c(51498, "Specific_Gravity"),
             c(51492, "Protein"),
             c(51466, "Blood"),
             c(51486, "Leukocytes"),
             c(51097, "Potassium_Urine"),
             c(6648, "Chloride_Urine"),
             c(51100, "Sodium_Urine"),
             c(51082, "Creatinine_Urine"),
             c(51104, "Urea_Nitrogen_Urine"),
             c(51006, "Urea_Nitrogen_blood"))

for (item in keys) {
    code = item[[1]]
    lab = item[[2]]
    
    akicreatinine_full = create_lab(code, lab)
}

names(akicreatinine_full)
head(akicreatinine_full)

path_file = "data/train_dataset.csv"
path_file = "data/train_dataset.rda"
fwrite(x = akicreatinine_full, file = path_file)
save(akicreatinine_full, file=path_file)

## training
akicreatinine_full$AKI_CREATININA

akicreatinine_full = akicreatinine_full[FIRST_CREATININE<3.0 | RATIO_C48_C0 > 1.5]

table(akicreatinine_full$AKI_48hr)

ignore_vars = c("SUBJECT_ID", "HADM_ID", "INTIME", "FIRST_CREATININE", "CREATININE_48",
                "RATIO_C48_C0", "Urea_Nitrogen_blood", "AKI_stage_48hr",
                "AKI_stage_48hr_creat", "ICUSTAY_ID", "UrineOutput_6hr", "UrineOutput_12hr",
                "UrineOutput_24hr", "ADMITTIME", "AKI_48hr")

vars_in = setdiff(names(akicreatinine_full), ignore_vars)
train_set = akicreatinine_full[, ..vars_in]
names(train_set)
str(train_set)

table(akicreatinine_full$AKI_CREATININA)

## missingness
healthcareai::missingness(train_set) %>% 
    plot()

## X-ray
xray::anomalies(train_set)

# Split the data
split_data = healthcareai::split_train_test(d = train_set,
                                            outcome = "AKI_CREATININA",
                                            #outcome = "AKI_48hr",
                                            p = .8,
                                            seed = 7777)

## Training
model = healthcareai::machine_learn(d=split_data$train, outcome = AKI_CREATININA)
model

# Plot models
plot(model)

# Evalue models
evaluate(model, all_models = TRUE)

# Intepretation
get_variable_importance(model, top_n = 10) %>%
    plot()

# Precition over training set
predict(model) %>% 
    plot()

# Evaluation over test set
train_predictions = predict(model,  
                           as.data.frame(split_data$train), 
                           risk_groups = c(low = 30, moderate = 40, high = 20, extreme = 10))

# Evaluation over test set
test_predictions = predict(model,  
                           as.data.frame(split_data$test), 
                           risk_groups = c(low = 30, moderate = 40, high = 20, extreme = 10))
                                                                                             
test_predictions

plot(test_predictions)

# Evaluation
evaluate(test_predictions)

class_preds = predict(model, newdata = as.data.frame(split_data$test), outcome_groups = 10)

table_test = table(actual = class_preds$AKI_CREATININA, predicted = class_preds$predicted_group)
prop.table(table_test)

## Plots

dt_group = train_set[, .(count = .N), by = c("ETHNICITY", "AKI_CREATININA")]
dt_group = dt_group[order(-count)]
sub_dt = dt_group[AKI_CREATININA == 1]
g <- ggplot(dt_group, aes(x=reorder(ETHNICITY, 
                                        count), 
                                     y=count, fill=ETHNICITY))
g <- g + geom_bar(stat="identity")
g <- g + coord_flip()
g <- g + theme(legend.position="none")
g <- g + facet_wrap(~AKI_CREATININA)
g


sub_dt = dt_group[AKI_CREATININA == 0]
g <- ggplot(sub_dt[1:10], aes(x=reorder(ETHNICITY, 
                                        count), 
                              y=count, fill=ETHNICITY))
g <- g + geom_bar(stat="identity")
g <- g + coord_flip() 
g

# Correlation plot
r = cor(train_set[, c("Hematocrit", "Sodium", "Platelet_Count", "Hemoglobin",
                      "Potassium", "White_Blood_Cells", "pH_urine", "Specific_Gravity",
                      "pH_blood", "Lactate", "Bilirubin_Total", "Albumin")], 
        use="complete.obs")

ggcorrplot::ggcorrplot(r, 
                       hc.order = TRUE, 
                       type = "lower",
                       lab = TRUE)

