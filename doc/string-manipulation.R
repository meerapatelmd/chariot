## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(chariot)

## -----------------------------------------------------------------------------
sample_strings <-
tibble::tribble(
        ~official_title,
                        "Clinical Performance of Existing Wearers of Avaira Sphere (Enfilcon A) Following a Refit With Clariti Elite Sphere (Somofilcon A) Lenses for 4 Weeks",
                        "An Open-label Exploratory Phase II Study of the Safety and Immunogenicity of Repeated 'rhC1INH' Administration of 50 U/Kg in Patients With Hereditary C1 Inhibitor Deficiency ('HAE')",
                        "Molecular Signature of Valproic Acid in Breast Cancer With Functional Imaging Assessment - a Pilot",
                        "The Role of Fractional CO2 Laser in Consolidation Treatment of Recurrent Vulvovaginal Candidiasis (RVVC) ：a Study Protocol for a Randomized Controlled Trial",
                        "Colonization, Safety, Tolerance, and Effects of Three Probiotic Strains on the Immune System of Healthy Adults",
                        "The Reduction of Microalbuminuria in Japanese Hypertensive Subjects With Type 2 Diabetes Mellitus Treated With Valsartan or Amlodipine: Study Design for the Shiga Microalbuminuria Reduction Trial (SMART)",
                        "A Randomized Open-Label Non-Inferiority Study to Examine the Impact of Pertussis Vaccination of Healthcare Workers on Post-exposure Prophylaxis",
                        "Pilot Study 1 of Outpatient Control-to-Range - System and Monitoring Testing",
                        "Prophylactic Sub-lay Non-absorbable Mesh Following Emergent Midline Laparotomy Clean/Contaminated Field: Early Results of a Randomized Double-blind Prospective Trial: PROMETHEUS",
                        "A Phase 1, Randomized, Double-Blind, Placebo-Controlled Study To Evaluate The Safety, Tolerability, Pharmacokinetics And Pharmacodynamics Of PF-06252616 In Healthy Subjects",
                        "Patient Benefit From the New Modular Shoulder Prosthesis PROMOS - a Multicentre Cohort Study",
                        "Hospital-based Home Care for Children With Cancer",
                        "Serum Levels of Advanced Glycation End-products After Dietary Intervention in Hypertensive Patients: Study Protocol of a Randomized Clinical Trial.",
                        "The Effect of Blood Flow Restriction Training on Muscle Atrophy Following Knee Surgery; A Randomized Control Trial",
                        "Otimização do Sistema de Saúde no Brasil Com Telemedicina",
                        "Direct-acting Antiviral Therapy and Reinfection Among People With Chronic Hepatitis C Virus Infection and Recent Injecting Drug Use in the Prison Setting (The SHARP-P Study)",
                        "A Phase 1, Open-label, 3-period, Randomized, Partially Fixed Sequence, 2-way Crossover Study With A Microdose Of [11c]Pf-06427878 Administered With And Without Two Non-radioactive Doses Of Pf-06427878 To Characterize Tissue Distribution Using Positron Emission Tomography In Healthy Adult Male Subjects",
                        "Evaluation of the Ulthera System for Treating Axillary Hyperhidrosis",
                        "Hepatitis C Education for Pregnant Women With Opiate Addiction - Phase 1",
                        "A Multi-Center Randomized Controlled Trial of Intramedullary Nails Versus Sliding Hip Screws in the Management of Intertrochanteric Fractures of the Hip",
                        "Autologous Cytokine-induced Killer Cells Combined Chemotherapy in Advanced Pancreatic Cancer: A Prospective, Randomized, Open, Single Center Phase II Study",
                        "Sodium Intake in Chronic Kidney Disease (STICK): A Randomised Controlled Trial",
                        "A Double-Blind, Placebo-Controlled Evaluation of the Safety and Efficacy of Cariprazine in the Acute Exacerbation of Schizophrenia",
                        "A Phase II Trial of Pazopanib in Patients With Metastatic Alveolar Soft Part Sarcoma",
                        "Arimidex/Faslodex/Iressa Study: A Phase II Trial of Primary Systemic Therapy Using a Combination of Arimidex, Faslodex and Iressa (Gefitinib) in Postmenopausal Women With Hormone Receptor Positive Breast Cancer",
                        "Comparison of the Long-term Effects on Mortality and Cardiovascular Morbidity of Percutaneous Coronary Intervention With Drug-eluting Stent Versus Bare-metal Stent. Randomized, Five-year Prospective, Multicenter Clinical Trial",
                        "A Prospective, Post-market, Multi-center Comparative Study of the Efficacy of Flex Intramedullary(IM) Rod in Terms of Range of Motion (ROM) Improvement.",
                        "Does Prescriptive Treatment of the Hips Improve Outcomes in Patients With Low Back Pain? A Randomized Controlled Trial",
                        "Treating Stimulant Addiction With Repetitive Transcranial Magnetic Stimulation",
                        "Protocol to Obtain Blood Samples for Leukemia Research",
                        "Assessing Inherited Markers of Metabolic Syndrome in the Young",
                        "Structural Hinders to and Promoters of Good Maternal Care in Rural China",
                        "A Phase II Study of Bevacizumab With Docetaxel and Capecitabine in the Neoadjuvant Setting for Breast Cancer Patients",
                        "A 6wk Open-Label, Randomised, Multicentre, Phase IIIb, Parallel Group Study to Compare the Safety & Efficacy of Rosuvastatin 40mg in Comb.With Ezetimibe 10mg in Subjects With Hypercholesterolaemia & CHD or Atherosclerosis or a CHD Risk Equiv. (10 yr Risk Score >20%).",
                        "The Effect of Bread Fortification With Phosphorus and Lysine on Postprandial Glycaemia and Thermogenesis",
                        "Comparison of Ferrous Sulfate, Polymaltose Complex and Iron-zinc in Iron Deficiency Anemia",
                        "Clinical Effects of Adjunctive Local Sodium Hypochlorite Gel in Minimally Invasive Non Surgical Debridement (MINSD) of Periodontal Pockets. A 6-month Randomized Controlled Clinical Trial",
                        "An Open-Label, Multicenter, Multinational Study to Assess the Safety,Tolerability and Pharmacokinetics of Aerosolized Amikacin Delivered Via the Pulmonary Drug Delivery System (NKTR-061) in Intubated and Mechanically- Ventilated Patients With Nosocomial Pneumonia",
                        "Role of Canagliflozin on Gene Expression and Function of CD34+ Endothelial Progenitor Cells and Renal Function in Patients With Type 2 Diabetes",
                        "COMPARING THE OUTCOME IN PATIENTS OF ACUTE PANCREATITIS, WITH AND WITHOUT PROPHYLACTIC ANTIBIOTICS.",
                        "Caelyx(R) in Breast Cancer in the Elderly. Pegylated Liposomal Doxorubicin (Caelyx(R)) as Monotherapy in Elderly Patients With Locally Advanced and/or Metastatic Breast Cancer.",
                        "A Single Arm, Open Label, Perspective Study to Determine the Efficacy and Safety of Icotinib Combine Cryotherapy for Advanced NSCLC Patients",
                        "Manual Lymphatic Drainage in Women Undergone to Thigh Lifting After Bariatric Surgery",
                        "Evaluation of Limb-Girdle Muscular Dystrophy",
                        "A Phase I-II Trial of Gleevec (Imatinib Mesylate) in Combination With Chlorambucil in Previously Treated Chronic Lymphocytic Leukemia (CLL) Patients",
                        "Chlorhexidine Impregnated Cloths to Prevent Skin and Soft Tissue Infections in Marine Officer Candidates: A Randomized, Double-Blind, Placebo-Controlled Trial",
                        "Effect of Lactobacillus Rhamnosus GG (LGG) on Infant Crying, Intestinal Microbiota, and Intestinal Inflammation in Infants With Colic",
                        "Phase IV Trial,The Efficacy and Safety Study of Fuganlin Oral Liquid in Children With Acute Upper Respiratory Infection",
                        "A Randomized, Double-blind, Repeat Dose, Two Period Crossover Study to Evaluate the Safety and Tolerability, Pharmacokinetics, and Pharmacodynamics of Inhaled Fluticasone Furoate/Vilanterol 100/25 Micrograms in Children Aged 5 to 11 Years With Persistent Asthma",
                        "The 'Concept' of Parametria: a Proposed Classification of Parametrectomy for the Treatment of Deep Infiltrating Endometriosis. A Prospective, Clinical Trial"
)

## -----------------------------------------------------------------------------
sample_strings_words <-
process_words(data = sample_strings,
              concept_col = official_title)
sample_strings_words

## -----------------------------------------------------------------------------
process_unique_words(data = sample_strings_words,
                     words_col = `official_title Words`)

## -----------------------------------------------------------------------------
process_longest_words(data = sample_strings_words,
                      concept_col = official_title,
                      words_col = `official_title Words`)

## -----------------------------------------------------------------------------
process_first_parentheses(data = sample_strings,
              concept_col = official_title)

