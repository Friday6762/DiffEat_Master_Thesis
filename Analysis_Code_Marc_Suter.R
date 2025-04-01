# Master Thesis Documentation: Data Processing

# Load preprocessed data
load("/Volumes/cds-masterprojects/diffeat/data/ diffeat_data.Rdata") # Load from Project Server

## Load Required Libraries
library("dplyr")
library("ggplot2")
library("tidyr")
library("ppcor")
library("esquisse")
library("pander")
library("reshape2") 
library("car")
library("Hmisc") 

### Remove FRK Questionnaire Items
df <- df[, !names(df) %in% grep("^v_(300|3[0-1][0-2]?)$", names(df), value = TRUE)]

### Sample Overview (Pre-Exclusion)
df %>% distinct(id) %>% nrow()  # Unique participants
range(df$age, na.rm = TRUE)  # Age range

### Rename Demographic Variables
colnames(df)[colnames(df) == "v_319"] <- "first_language"
colnames(df)[colnames(df) == "v_317"] <- "school_education" 
colnames(df)[colnames(df) == "v_318"] <- "work_education" 


### Eating Disorders
df %>% filter(current_ed == 1) %>% dplyr::select(id, current_ed, current_ed_type) # Inspect identified participants
df <- df %>% filter(id != 80) # Remove specific participant (id = 80) with Bulimic Eating Behavior
df <- df[, !grepl("^(current_ed|current_ed_type|past_ed_type|past_ed)$", names(df))]

### Eating Attitudes Test 26 (EAT-26)
EAT_var_all <- colnames(df)[colnames(df) %in% paste0("v_", c(191:216))]

# Identify Values Outside Valid Range (1-7) and Recode Invalid Values
df %>%
  dplyr::select(id, EAT_var_all) %>%
  gather(key = "variable", value = "value", EAT_var_all) %>%
  filter(value > 7 | value < 1) %>%
  distinct(id, variable)

#### Identify 
df$v_201[df$id == 94] 
df$v_215[df$id == 45] 

#recode as NA, as 0 does not exsist in the EAT-26
df$v_201[df$id == c(94)] <- NA  
df$v_215[df$id == c(45)] <- NA

#### Reverse Coding and Scoring for question 25, v_215
df <- df %>% mutate(v_215 = 7 - v_215)

# Recode EAT-26 responses (1-3 -> 0, 4 -> 1, 5 -> 2, 6 -> 3)
df[, EAT_var_all] <- lapply(df[, EAT_var_all], function(x) {
  x <- as.numeric(x)
  x[x == 1] <- 0
  x[x == 2] <- 0
  x[x == 3] <- 0
  x[x == 4] <- 1
  x[x == 5] <- 2
  x[x == 6] <- 3
  return(x)
})

df$EAT_26_total <- rowSums(df[EAT_var_all], na.rm = TRUE) # Compute total EAT-26 score

# Identify participants above EAT-26 cutoff (>=20)
EAT_26_cut_off <- df %>% filter(EAT_26_total >= 20) %>% distinct(id, .keep_all = TRUE)
print(EAT_26_cut_off$id)
df <- df %>% filter(!id %in% c(38, 88, 111)) # Remove specific participants (id = 38, 88, 111)
rm(EAT_var_all, EAT_26_cut_off)

## Data Filtering and Processing

### Select Relevant Measures
df <- df %>% filter(measure %in% c("FA", "iRD"))
df <- df %>% filter(!tract %in% c("sVTA-NAcc (L)", "sVTA-NAcc (R)", "sVTA-NAcc (bil)"))

### Extract Hemisphere Information
df <- df %>% mutate(
  hemisphere = case_when(grepl("\\(L\\)", tract) ~ "L", grepl("\\(R\\)", tract) ~ "R", TRUE ~ "B"),
  tract = gsub("\\(L\\)|\\(R\\)|\\(bil\\)", "", tract),
  tract = trimws(tract)
)

### Rename Numeric Columns
numeric_cols <- grep("^\\d+$", names(df))
names(df)[numeric_cols] <- paste0("node_", seq_along(numeric_cols))
rm(numeric_cols)

## Merge BIS-11 Data
load("/Volumes/cds-masterprojects/diffeat/data/diffeat_bis_data.Rdata")
bis_data_sub <- bis_data_sub %>% rename(BIS_attention = BARatt, BIS_motor = BARmot, BIS_nonplanning = BARnpl)
df <- merge(df, bis_data_sub, by = "id")

### Handle Missing Data
missing_ids <- df %>% dplyr::select(id, BIS_attention, BIS_motor, BIS_nonplanning) %>% pivot_longer(cols = -id, names_to = "measure", values_to = "value") %>% filter(is.na(value))
print(missing_ids)
  # Remove participants with missing data id %in% c(15, 22)
rm(missing_ids)

### Handle Outliers
# Function to detect outliers beyond 3 standard deviations
detect_outliers <- function(variable, id_column) { 
  mean_score <- mean(variable, na.rm = TRUE)  
  sd_score <- sd(variable, na.rm = TRUE)  
  lower_bound <- mean_score - 3 * sd_score  
  upper_bound <- mean_score + 3 * sd_score  
  outlier_ids <- unique(id_column[!(variable >= lower_bound & variable <= upper_bound)])  
  print(paste("Id(s):", toString(outlier_ids)))  
}
detect_outliers(df$BIS_attention, df$id) #No outlier
detect_outliers(df$BIS_motor, df$id) #One outlier in BIS_motor (id = 96)
detect_outliers(df$BIS_nonplanning, df$id) #No outlier

### Compute BIS Total Score
df <- df %>% mutate(BIS_total = rowSums(dplyr::select(., BIS_attention, BIS_motor, BIS_nonplanning), na.rm = FALSE))
rm(bis_data_sub)
rm(list = setdiff(ls(), c("diffeat_df", "df","detect_outliers"))) #clear environment


## FEV ("Fragebogen zum Eßverhalten)" subscales
FEV_var_cognitive_control <- c("v_220", "v_222", "v_226","v_230", "v_234", "v_237", "v_239", "v_244","v_246", "v_248", "v_249", "v_251","v_337", "v_339", "v_340", "v_342", "v_344", "v_347", "v_348", "v_253", "v_254") 
FEV_var_disruptability <- c("v_217", "v_218","v_223", "v_225", "v_227", "v_229", "v_231", "v_232", "v_236", "v_241", "v_243", "v_247", "v_252", "v_341", "v_345", "v_346") 
FEV_var_hunger <- c("v_219","v_221", "v_224", "v_228", "v_233", "v_235", "v_238","v_240", "v_242", "v_245", "v_250", "v_336", "v_338", "v_343")
FEV_rest <- colnames(df)[colnames(df) %in% paste0("v_", c(349:357))] 
cols_to_keep <- c(names(df)[!names(df) %in% FEV_rest], FEV_var_cognitive_control, FEV_var_hunger)
df <- df[, cols_to_keep]

### Check for invalid values outside of the range of the questionnaire ###
invalid_values <- df %>% 
  dplyr::select(id, all_of(FEV_var_disruptability)) %>% 
  pivot_longer(cols = all_of(FEV_var_disruptability), names_to = "variable", values_to = "value") %>% 
  filter(value < 1 | value > 4) %>% 
  distinct(id, variable)
print(invalid_values)
df$v_252[df$id == 29]
df$v_247[df$id == 81]
df$v_252[df$id == 82]
df$v_241[df$id == 105]
df$v_241[df$id == 113]
df$v_217[df$id == 122]
#All values are = 0

### Identify completely missing values
missing_info <- which(is.na(df[FEV_var_disruptability]), arr.ind = TRUE)  
missing_data <- data.frame(  
  id = df$id[missing_info[, 1]],  # Get corresponding id  
  Column = colnames(df[FEV_var_disruptability])[missing_info[, 2]]  # Get column names  
)  

missing_summary <- missing_data %>%
  dplyr::group_by(id, Column) %>%  
  dplyr::summarise(Missing_Count = n(), .groups = "drop") 
print(missing_summary)  
View(missing_summary)
# n = 5 had NAs in "v_341", "v_345", "v_346"

### Recoding
#Recoding (< 0): Set all 0 values in FEV_disruptability to NA  
#Important, before study coding (1,2) is recoded to questionnaire coding (0,1)

#Recoding (< 0) # Set all values of all_of(FEV_all_var)) == 0 to NA 
df[FEV_var_disruptability] <- lapply(df[FEV_var_disruptability], function(x) {  
  x <- as.numeric(x)  
  x[x == 0] <- NA  
  return(x)  
})

df <- df %>% mutate_at(vars(matches("^v_247$|^v_241$|^v_232$")), ~3 - .) #reverse coding

### Dichotomization


#### Recoding the levels of the items "v_341", "v_345", "v_346" to 1, 2 = 1; 3,4 = 0
recoding_1to4 <- colnames(df)[colnames(df) %in% c("v_341", "v_345", "v_346")]

for (col_name in recoding_1to4) {
  # Convert to character
  df[[col_name]] <- as.character(df[[col_name]])
  
  # Recode values: 1 and 2 → "1", 3 and 4 → "0"
  df[[col_name]][df[[col_name]] %in% c("1", "2")] <- "1"
  df[[col_name]][df[[col_name]] %in% c("3", "4")] <- "0"
  
  # Convert back to numeric
  df[[col_name]] <- as.numeric(df[[col_name]])
}

#View(df[1:5, FEV_var_disruptability]) #Check, if all items are dichotomized

### Recoding from study coding to questionnaire coding (1 = 1; 2 = 0) after Dichotomization      
df[FEV_var_disruptability] <- lapply(df[FEV_var_disruptability], function(x) {  
  x <- as.numeric(x)  
  x[x == 2] <- 0  
  return(x)  
})

df[FEV_var_disruptability] <- lapply(df[FEV_var_disruptability], as.numeric)
df$FEV_disruptability <- rowMeans(df[, FEV_var_disruptability], na.rm = TRUE)
#mean scores berechnen aufgrund NAs

### Handle Outliers
detect_outliers <- function(variable, id_column) { 
  mean_score <- mean(variable, na.rm = TRUE)  
  sd_score <- sd(variable, na.rm = TRUE)  
  lower_bound <- mean_score - 3 * sd_score  
  upper_bound <- mean_score + 3 * sd_score  
  outlier_ids <- unique(id_column[!(variable >= lower_bound & variable <= upper_bound)])  
  print(paste("Id(s):", toString(outlier_ids)))  
}
detect_outliers(df$FEV_disruptability, df$id) #id = 12
#remove outlier (id = 12)
df <- df %>% filter(id != 12) # One Outlier in FEV_disruptability

df <- df[, !names(df) %in% c(FEV_var_disruptability)] #
rm(list = setdiff(ls(), c("diffeat_df", "df","detect_outliers"))) #clear environment

## UPPS
UPPS_var_urgency <- c("v_256", "v_260", "v_264", "v_268", "v_272", "v_278", "v_282", "v_286", "v_290", "v_295", "v_297", "v_299")
UPPS_var_sensation_seeking <- c("v_257", "v_261", "v_265", "v_269", "v_273", "v_275", "v_279", "v_283", "v_287", "v_291", "v_296", "v_298")
UPPS_var_perseverance <- c("v_258", "v_262", "v_266", "v_270", "v_274", "v_276", "v_280", "v_284", "v_288", "v_292")
UPPS_var_premeditation <- c("v_255", "v_259", "v_263", "v_267", "v_271", "v_277", "v_281", "v_285", "v_289", "v_293", "v_294")

### UPPS reverse coding
variables_to_reverse <- c("v_256", "v_257", "v_260", "v_261", "v_264", "v_265", 
                          "v_268", "v_269", "v_272", "v_273", "v_275", "v_278", "v_279", 
                          "v_282", "v_283", "v_286", "v_287", "v_290", "v_291", 
                          "v_295", "v_296", "v_297", "v_298", "v_299")

#v_262 and v_292 already recoded. v_297 was falsely recoded during preprocessing and must be recoded again.

for (var in variables_to_reverse) {
  df[[var]] <- 5 - df[[var]]}

rm(list = setdiff(ls(), c("colors", "diffeat_df", "df","detect_outliers"))) #clear environment


### UPPS subscale score caluclation
df$UPPS_urgency <- rowMeans(df[, UPPS_var_urgency], na.rm = TRUE)  
df$UPPS_sensation_seeking <- rowMeans(df[, UPPS_var_sensation_seeking], na.rm = TRUE) 
df$UPPS_perseverance <- rowMeans(df[, UPPS_var_perseverance], na.rm = TRUE)
df$UPPS_premeditation <- rowMeans(df[, UPPS_var_premeditation], na.rm = TRUE) 
df$UPPS_total <- rowMeans(df[, c(UPPS_var_urgency, UPPS_var_sensation_seeking, UPPS_var_perseverance, UPPS_var_premeditation)], na.rm = TRUE) 

### Check for NAs
any(is.na(df$UPPS_urgency)) #check for missing values
any(is.na(df$UPPS_sensation_seeking)) #check for missing values
any(is.na(df$UPPS_perseverance)) #check for missing values
any(is.na(df$UPPS_premeditation)) #check for missing values


### Handle Outliers
detect_outliers(df$UPPS_urgency, df$id) # id == 49
detect_outliers(df$UPPS_sensation_seeking, df$id)
detect_outliers(df$UPPS_perseverance, df$id)
detect_outliers(df$UPPS_premeditation, df$id)

df <- df[, !names(df) %in% c(UPPS_var_urgency, UPPS_var_sensation_seeking, UPPS_var_perseverance, UPPS_var_premeditation)]
rm(list = setdiff(ls(), c("colors", "diffeat_df", "df", "detect_outliers"))) #clear environment


## Tractography data (RD; IRD)
df$tract <- as.factor(df$tract)
df$tract <- relevel(df$tract, "MPFC-NAcc")
df$hemisphere <- factor(df$hemisphere,
                        levels = c("L", "B", "R"),
                        labels = c("Left", "Bilateral", "Right"))

df$hemisphere <- as.factor(df$hemisphere)
df$hemisphere <- relevel(df$hemisphere, "Left")
levels(df$hemisphere)

### create middle50 incidies
nodes <- paste0("node_", c(26:75))
any(is.na(df[, nodes])) #NA values check in the middle 50% of the tracts     
df$mean_middle_50 <- rowMeans(df[, nodes]) 

### FA Overview Plot 
df_FA <- df %>%
  filter(measure == "FA") %>%
  gather(key = "nodes", value = "value", node_26:node_75)   

df_FA$nodes <- gsub("node_", "", df_FA$nodes)
df_FA$id <- as.factor(df_FA$id)
df_FA$nodes <- as.numeric(df_FA$nodes)
df_FA$value <- as.numeric(df_FA$value)

### Plot
FA_Plot <- ggplot(data = df_FA, 
                  mapping = aes(x = nodes, y = value, color = as.factor(id), group = id)) +
  geom_line(size = 0.25, alpha = 0.5) +  
  facet_grid(hemisphere ~ tract) +
  ggtitle("FA Values") +
  theme_bw() +
  guides(color = "none") +
  scale_x_continuous(breaks = c(30, 40, 50, 60, 70)) +
  xlab("Nodes") +  
  ylab("Value") +  
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_blank(),  
    strip.text.x = element_text(size = 12),  
    strip.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),  
    axis.text.x = element_text(size = 10, margin = margin(t = 2, r = 0, b = 0, l = 0)),  
    axis.text.y = element_text(size = 10, margin = margin(t = 0, r = 2, b = 0, l = 0))  
  )

print(FA_Plot)

ggsave("FA_middle50.png", plot = FA_Plot, device = "png", width = 500/85, height = 450/85, units = "in", dpi = 300)       #save as png#ggsave("FA_Plot.png", plot = iRD_Plot, device = "png", width = 10, height = 10, units = "cm", dpi = 300)       #save as png  


## iRD Overview Plot 
df_iRD <- df %>%
  filter(measure == "iRD") %>%
  gather(key = "nodes", value = "value", node_26:node_75)   

df_iRD$nodes <- gsub("node_", "", df_iRD$nodes)
df_iRD$id <- as.factor(df_iRD$id)
df_iRD$nodes <- as.numeric(df_iRD$nodes)
df_iRD$value <- as.numeric(df_iRD$value)

### Plot
iRD_Plot <- ggplot(data = df_iRD, 
                   mapping = aes(x = nodes, y = value, color = as.factor(id), group = id)) +
  geom_line(size = 0.25, alpha = 0.5) +  
  facet_grid(hemisphere ~ tract) +
  ggtitle("iRD Values") +
  theme_bw() +
  guides(color = "none") +
  scale_x_continuous(breaks = c(30, 40, 50, 60, 70)) +
  xlab("Nodes") +  
  ylab("Value") +  
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_blank(),  
    strip.text.x = element_text(size = 12),  
    strip.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),  
    axis.text.x = element_text(size = 10, margin = margin(t = 2, r = 0, b = 0, l = 0)),  
    axis.text.y = element_text(size = 10, margin = margin(t = 0, r = 2, b = 0, l = 0))  
  )

print(iRD_Plot)

ggsave("iRD_middle50.png", plot = iRD_Plot, device = "png", width = 500/85, height = 450/85, units = "in", dpi = 300)       #save as png#ggsave("iRD_Plot.png", plot = iRD_Plot, device = "png", width = 10, height = 10, units = "cm", dpi = 300)       #save as png  


# Master Thesis Documentation: Data Analysis

# Final Demographics
nrow(df)/24 # Calculate final sample size
round(mean(df$age),1)  # Mean age
round(sd(df$age),1)   # Standard deviation of age

gender <- table(df$gender) / 24 # Proportion of gender categories
round((table(df$gender)/ 24)/sum(table(df$gender)/ 24),3)*100

table(df$school_education) / 24 # Proportion of school education levels
round((table(df$school_education)/ 24)/sum(table(df$school_education)/ 24),3)*100




# rename the first language categories:
df$first_language <- as.character(df$first_language)
df$first_language[df$first_language == "deutsch"] <- "German"
df$first_language[df$first_language == "Deutsch"] <- "German"
df$first_language[df$first_language == "DEUTSCH"] <- "German"
df$first_language[df$first_language == "deutsch englisch"] <- "German/English"
df$first_language[df$first_language == "Englisch"] <- "English"
df$first_language[df$first_language == "Russisch"] <- "Russian"
df$first_language[df$first_language == "Spanisch"] <- "Spanish"
df$first_language[df$first_language == "Deutsch und Russisch"] <- "Russisch"
table(df$first_language) / 24 # Proportion of first language categories
round((table(df$first_language)/ 24)/sum(table(df$first_language)/ 24),3)*100

## BMI: Calculate BMI and its mean and standard deviations
df$BMI <- df$weight_kg / (df$height_m^2) # Calculate BMI using df$weight_kg and df$height_m 
mean(df$BMI)  # Mean BMI
sd(df$BMI)    # Standard deviation of BMI
df <- df %>% dplyr::select(-weight_kg, -height_m) #remove df$weight_kg df$height_m from df

# Hypothesis 1

## Hypotheses 1a: FA of VTA-NAcc Bilateral, UE

H1a <- subset(df, tract == "iVTA-NAcc" & measure == "FA" & hemisphere == "Bilateral")

df <- df %>% filter(id != 12) # One Outlier in FEV_disruptability
df <- df %>% filter(id != 38 & id != 88 & id != 111)

nrow(df)/24 # H1a sample size
unique(H1a$hemisphere)
unique(H1a$tract)
unique(H1a$measure)

#Standardize the variables
H1a$mean_middle_50_sdt <- scale(H1a$mean_middle_50)
H1a$FEV_disruptability_sdt <- scale(H1a$FEV_disruptability)

#Check for normality
shapiro.test(H1a$FEV_disruptability_sdt)
#distribution of FEV_disruptability_sdt significantly deviates from normality
#hist(H1a$mean_middle_50_sdt)

shapiro.test(H1a$mean_middle_50_sdt)
#distribution of mean_middle_50_sdt significantly deviates from normality
#hist(H1a$FEV_disruptability_sdt)


library("ppcor")
H1a_result <- pcor.test(H1a$mean_middle_50_sdt, 
                        H1a$FEV_disruptability_sdt, 
                        cbind(H1a$gender, H1a$age, H1a$BMI))   #Variables controlled for: "age" "gender" "BMI"


H1a_result
explained_variance <- (H1a_result$estimate)^2


### Scatterplot for H1a

#Residuals of mean_middle_50_sdt after controlling for gender, age, and BMI
residuals_mean <- residuals(lm(mean_middle_50_sdt ~ gender + age + BMI, data = H1a))
#Residuals of FEV_disruptability_sdt after controlling for gender, age, and BMI
residuals_fev <- residuals(lm(FEV_disruptability_sdt ~ gender + age + BMI, data = H1a))

#Assuming you have the data in `residuals_mean` and `residuals_fev`
data <- data.frame(residuals_mean, residuals_fev)

H1a_scatter <- ggplot(data, aes(x = residuals_mean, y = residuals_fev)) +
  geom_point(color = "#508F81", size = 3) +  # Plot points
  geom_smooth(method = "lm", color = "#334F49", linewidth = 1.2, se = FALSE) +  # Add regression line
  labs(
    x = "Residuals of Middle 50% of VTA-NAcc tract",
    y = "Residuals of Uncontrolled Eating"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),  # Axis labels size and font
    axis.text = element_text(size = 14),  # Axis text size
    panel.border = element_rect(color = "black", fill = NA),  # Border style
    plot.margin = margin(1, 1, 1, 1, "cm")  # Plot margins
  )

ggsave("H1a_scatter.png", plot = H1a_scatter, device = "png", width = 500/85, height = 450/85, units = "in", dpi = 300)       #save as png#ggsave("FA_Plot.png", plot = iRD_Plot, device = "png", width = 10, height = 10, units = "cm", dpi = 300)       #save as png  

### BMI Distribution for H1a

hist(H1a$BMI,  
     main = "",  
     xlab = "BMI",  
     ylab = "Frequency",  
     col = "#508F81",  
     border = "black",  
     breaks = 30,  
     font.lab = 2, 
     cex.lab = 1.5, 
     cex.axis = 1.2,  
     bty = "l") 

abline(v = 25, col = "red", lwd = 2, lty = 2)  # Overweight cutoff  
abline(v = 18.5, col = "blue", lwd = 2, lty = 2)  # Underweigth cutoff  
legend("topright", legend = c("Overweight (≥25)", "Underweight (≤18.5)"),  
       col = c("red", "blue"), lty = 2, lwd = 2, 
       bty = "o", cex = 0.8, inset = 0.05)  #

sum(H1a$BMI >= 25) #Sum of overweight individuals (WHO)
sum(H1a$BMI <= 18.5) #Sum of underweight individuals (WHO)


rm(list = setdiff(ls(), c("diffeat_df", "df"))) #clear environment

# Exploratory analysis

## Exploratory analysis 1: Node-wise Regression of different tracts and measures

Exp_1 <- df
nrow(Exp_1)/24 # Exp_1 sample size
unique(Exp_1$hemisphere)
unique(Exp_1$tract)
unique(Exp_1$measure)

#Colors for each tract
colors <- list(mpfc = rgb(15, 209, 219, maxColorValue = 255), ains = rgb(226, 24, 2, maxColorValue = 255),
               amyg = rgb(0, 0, 139, maxColorValue = 255),  ivta = rgb(255, 215, 0, maxColorValue = 255))

# Prepare the data for FA regression
Exp_1_fa_long <- df %>%
  filter(measure == "FA") %>%
  pivot_longer(cols = starts_with("node_"), 
               names_to = "nodes", 
               values_to = "value") %>%
  mutate(nodes = as.numeric(gsub("node_", "", nodes)))

tracts <- c("MPFC-NAcc", "AIns-NAcc", "Amy-NAcc", "iVTA-NAcc")
hemispheres <- c("Left", "Bilateral", "Right")

# Compute regression results
Exp_1_fa_regs <- Exp_1_fa_long %>%
  filter(tract %in% tracts) %>%
  group_by(tract, hemisphere, nodes) %>%
  filter(!(tract == "MPFC-NAcc" & id == '10')) %>%   # Remove outlier for MPFC-NAcc
  do({
    reg <- lm(FEV_disruptability ~ value, data = .)
    tval <- summary(reg)$coefficients[2,3]
    pval <- summary(reg)$coefficients[2,4]
    data.frame(TVALUE = tval, PVALUE = pval)
  }) %>%
  ungroup()

Exp_1_fa_regs <- Exp_1_fa_regs %>%
  mutate(HEMISPHERE = factor(hemisphere, levels = hemispheres),
         TRACT = factor(tract, levels = tracts))

#Plotting of results for the t Values of FA
tvals_FA <- ggplot(Exp_1_fa_regs, aes(x = nodes, y = TVALUE, color = TRACT, group = TRACT)) + 
  geom_point(size = 0.5, shape=21) +  # Add points for actual nodal values
  facet_grid(HEMISPHERE ~ TRACT) +
  ggtitle(expression(paste("FA ", italic("t"), "-Values"))) +  
  theme_bw() +
  guides(color = "none") +
  xlab("Nodes") +  
  ylab(expression(paste(italic("t"), " Values"))) +  
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_blank(),  
    strip.text.x = element_text(size = 12),  
    strip.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),  
    axis.text.x = element_text(size = 10, margin = margin(t = 2, r = 0, b = 0, l = 0)),  
    axis.text.y = element_text(size = 10, margin = margin(t = 0, r = 2, b = 0, l = 0))  
  ) +
  scale_color_manual(values = c("MPFC-NAcc" = colors$mpfc, 
                                "AIns-NAcc" = colors$ains, 
                                "Amy-NAcc" = colors$amyg, 
                                "iVTA-NAcc" = colors$ivta)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 0.5)

print(tvals_FA)

ggsave("tvals_FA", plot = tvals_FA, device = "png", width = 500/85, height = 450/85, units = "in", dpi = 300)       #save as png#ggsave("iRD_Plot.png", plot = iRD_Plot, device = "png", width = 10, height = 10, units = "cm", dpi = 300)       #save as png  

#Plotting of results for the p Values of FA
pvals_FA <- ggplot(Exp_1_fa_regs, aes(x = nodes, y = PVALUE, color = TRACT, group = TRACT)) + 
  geom_point(size = 0.5, shape=21) +  # Add points for actual nodal values
  facet_grid(HEMISPHERE ~ TRACT) +
  ggtitle(expression(paste("FA ", italic("p"), "-Values"))) + 
  theme_bw() +
  guides(color = "none") +
  xlab("Nodes") +  
  ylab(expression(paste(italic("p"), " Values"))) + 
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_blank(),  
    strip.text.x = element_text(size = 12),  
    strip.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),  
    axis.text.x = element_text(size = 10, margin = margin(t = 2, r = 0, b = 0, l = 0)),  
    axis.text.y = element_text(size = 10, margin = margin(t = 0, r = 2, b = 0, l = 0))  
  ) +
  scale_color_manual(values = c("MPFC-NAcc" = colors$mpfc, 
                                "AIns-NAcc" = colors$ains, 
                                "Amy-NAcc" = colors$amyg, 
                                "iVTA-NAcc" = colors$ivta)) +
  # Add grey dotted line at y = 0
  geom_hline(yintercept = 0.05, color = "grey", linetype = "dashed", size = 0.5)

print(pvals_FA)

ggsave("pvals_FA", plot = pvals_FA, device = "png", width = 500/85, height = 450/85, units = "in", dpi = 300)       #save as png#ggsave("iRD_Plot.png", plot = iRD_Plot, device = "png", width = 10, height = 10, units = "cm", dpi = 300)       #save as png  


# Node wise regression for iRD measure

# Prepare the data for iRD regression
Exp_1_iRD_long <- df %>%
  filter(measure == "iRD") %>%
  pivot_longer(cols = starts_with("node_"), 
               names_to = "nodes", 
               values_to = "value") %>%
  mutate(nodes = as.numeric(gsub("node_", "", nodes)))

tracts <- c("MPFC-NAcc", "AIns-NAcc", "Amy-NAcc", "iVTA-NAcc")
hemispheres <- c("Left", "Bilateral", "Right")

Exp_1_iRD_regs <- Exp_1_iRD_long %>%
  filter(tract %in% tracts) %>%
  group_by(tract, hemisphere, nodes) %>%
  filter(!(tract == "MPFC-NAcc" & id == '10')) %>%   # Remove outlier for MPFC-NAcc
  do({
    reg <- lm(FEV_disruptability ~ value, data = .)
    tval <- summary(reg)$coefficients[2,3]
    pval <- summary(reg)$coefficients[2,4]
    data.frame(TVALUE = tval, PVALUE = pval)
  }) %>%
  ungroup()

Exp_1_iRD_regs <- Exp_1_iRD_regs %>%
  mutate(HEMISPHERE = factor(hemisphere, levels = hemispheres),
         TRACT = factor(tract, levels = tracts))

#Plotting of results for the t Values of iRD
tvals_iRD <- ggplot(Exp_1_iRD_regs, aes(x = nodes, y = TVALUE, color = TRACT, group = TRACT)) + 
  geom_point(size = 0.5, shape=21) +  # Add points for actual nodal values
  facet_grid(HEMISPHERE ~ TRACT) +
  ggtitle(expression(paste("iRD ", italic("t"), "-Values"))) +  # Italicize "t"
  theme_bw() +
  guides(color = "none") +
  xlab("Nodes") +  
  ylab(expression(paste(italic("t"), " Values"))) +  # Italicize "t" in y-axis label
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_blank(),  
    strip.text.x = element_text(size = 12),  
    strip.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),  
    axis.text.x = element_text(size = 10, margin = margin(t = 2, r = 0, b = 0, l = 0)),  
    axis.text.y = element_text(size = 10, margin = margin(t = 0, r = 2, b = 0, l = 0))  
  ) +
  scale_color_manual(values = c("MPFC-NAcc" = colors$mpfc, 
                                "AIns-NAcc" = colors$ains, 
                                "Amy-NAcc" = colors$amyg, 
                                "iVTA-NAcc" = colors$ivta)) +
  # Add grey dotted line at y = 0
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 0.5)

print(tvals_iRD)
ggsave("tvals_iRD.png", plot = tvals_iRD, device = "png", width = 500/85, height = 450/85, units = "in", dpi = 300)       #save as png#ggsave("iRD_Plot.png", plot = iRD_Plot, device = "png", width = 10, height = 10, units = "cm", dpi = 300)       #save as png  


#Plotting of results for the p Values of iRD
pvals_iRD <- ggplot(Exp_1_iRD_regs, aes(x = nodes, y = PVALUE, color = TRACT, group = TRACT)) + 
  geom_point(size = 0.5, shape=21) +  # Add points for actual nodal values
  facet_grid(HEMISPHERE ~ TRACT) +
  ggtitle(expression(paste("iRD ", italic("p"), "-Values"))) + 
  theme_bw() +
  guides(color = "none") +
  xlab("Nodes") +  
  ylab(expression(paste(italic("p"), " Values"))) + 
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    strip.background = element_blank(),  
    strip.text.x = element_text(size = 12),  
    strip.text.y = element_text(size = 12),  
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),  
    axis.text.x = element_text(size = 10, margin = margin(t = 2, r = 0, b = 0, l = 0)),  
    axis.text.y = element_text(size = 10, margin = margin(t = 0, r = 2, b = 0, l = 0))  
  ) +
  scale_color_manual(values = c("MPFC-NAcc" = colors$mpfc, 
                                "AIns-NAcc" = colors$ains, 
                                "Amy-NAcc" = colors$amyg, 
                                "iVTA-NAcc" = colors$ivta)) +
  # Add grey dotted line at y = 0
  geom_hline(yintercept = 0.05, color = "grey", linetype = "dashed", size = 0.5)

print(pvals_iRD)

ggsave("iRD_pvals.png", plot = pvals_iRD, device = "png", width = 500/85, height = 450/85, units = "in", dpi = 300)       #save as png#ggsave("iRD_Plot.png", plot = iRD_Plot, device = "png", width = 10, height = 10, units = "cm", dpi = 300)       #save as png  


## Exploratory analysis 2: Correlation Analysis of BIS-11 and UPPS

### Data preparation
Exp_2 <- subset(df, tract == "iVTA-NAcc" & measure == "FA" & hemisphere == "Bilateral")
Exp_2 <- Exp_2[!Exp_2$id %in% c(15, 22, 71, 70),]
Exp_2_n <- length(unique(Exp_2$id))

# Select scales of interest
scales <- Exp_2[, c("BIS_attention", "BIS_motor", "BIS_nonplanning", "BIS_total", 
                    "UPPS_urgency", "UPPS_sensation_seeking", "UPPS_perseverance", 
                    "UPPS_premeditation", "UPPS_total")]

# Compute correlation and p-values
rcorr_res <- rcorr(as.matrix(scales), type = "pearson")
cor_matrix <- rcorr_res$r
p_matrix <- rcorr_res$P

# Convert to long format
cor_data <- melt(cor_matrix)
p_data <- melt(p_matrix)
colnames(cor_data) <- c("Var1", "Var2", "value")
cor_data$p_value <- p_data$value

cor_data <- cor_data %>%
  mutate(
    Var1 = factor(Var1, levels = colnames(scales)),
    Var2 = factor(Var2, levels = colnames(scales))
  ) %>%
  filter(as.numeric(Var1) > as.numeric(Var2))  # strictly lower triangle

# Add stars based on p-value
cor_data$stars <- cut(cor_data$p_value,
                      breaks = c(-Inf, 0.01, 0.05, Inf),
                      labels = c("**", "*", ""))
cor_data$label <- paste0(round(cor_data$value, 2), cor_data$stars)

# Create full grid for all combinations
full_grid <- expand.grid(Var1 = colnames(scales), Var2 = colnames(scales)) %>%
  mutate(
    Var1 = factor(Var1, levels = colnames(scales)),
    Var2 = factor(Var2, levels = colnames(scales))
  )

plot_data <- left_join(full_grid, cor_data, by = c("Var1", "Var2"))

new_labels <- c(
  BIS_attention = "Attention (BIS-11)",
  BIS_motor = "Motor (BIS-11)",
  BIS_nonplanning = "Non-Planning (BIS-11)",
  BIS_total = "BIS Total",
  UPPS_urgency = "Urgency (UPPS)",
  UPPS_sensation_seeking = "Sensation Seeking (UPPS)",
  UPPS_perseverance = "Perseverance (UPPS)",
  UPPS_premeditation = "Premeditation (UPPS)",
  UPPS_total = "UPPS Total"
)

Exp2_CorPlot <- ggplot(plot_data, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), color = "black", size = 3, na.rm = TRUE) +
  scale_fill_gradient2(
    low = "#FAE20C", high = "#6E49FA", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab", na.value = "white"
  ) +
  scale_x_discrete(labels = new_labels) +  # X-axis at bottom
  scale_y_discrete(labels = new_labels) +
  coord_fixed() +
  theme_bw(base_size = 12) +
  labs(
    x = NULL,
    y = NULL,
    title = expression(bold("Visual Correlation Matrix"))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 11, angle = 35, vjust = 1, hjust = 1, family = "Arial", color = "black"),
    axis.text.y = element_text(size = 11, family = "Arial", color = "black"),
    legend.position = "none"
  )

ggsave("Exp2_CorPlot.png", plot = Exp2_CorPlot, device = "png", width = 500/85, height = 450/85, units = "in", dpi = 300)       #save as png#ggsave("iRD_Plot.png", plot = iRD_Plot, device = "png", width = 10, height = 10, units = "cm", dpi = 300)       #save as png  












