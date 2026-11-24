# Mayotte-contact-matrices
Estimation and sensitivity analysis of age-specific contact matrices from the MayCov serosurvey in Mayotte.


# 🌍 Estimation and Sensitivity Analysis of Age-Specific Contact Matrices from a Short Survey in Mayotte

<p align="center">
  <img src="https://img.shields.io/badge/Research-Epidemiological%20Modelling-blue" />
  <img src="https://img.shields.io/badge/Study-Mayotte%2C%202021-orange" />
  <img src="https://img.shields.io/badge/Methods-Statistical%20Analysis-success" />
  <img src="https://img.shields.io/badge/Status-Research%20Project-purple" />
</p>

<p align="center">
  <b>Contact patterns • Statistical estimation • Reciprocity • Sensitivity analysis</b>
</p>

---

## 📖 Overview

This repository contains the computational materials, statistical analyses, and supplementary results associated with the research paper:

**"Estimation and Sensitivity Analysis of Age-Specific Contact Matrices from a Short Survey in Mayotte"**

The study investigates the estimation of age-specific contact matrices in Mayotte, a population characterized by a particularly young age structure, using data from the **2021 MayCov serosurvey**.

Particular attention is given to the impact of unusually large reported contact counts (heaping), exclusion rules, disagreement between reciprocal age-specific reports, and assumptions about contacts among minors.

The study further examines how alternative contact matrix constructions affect their agreement with the observed age profile of seroprevalence.

## 👥 Authors

- **Solym M. Manou-Abi**¹,*
- **Essoham Ali**²,³
- **Julien Balicchi**⁴

**Affiliations**

1. Laboratoire de Mathématiques et Applications, UMR CNRS 7348, Université de Poitiers, France.
2. Institut de Mathématiques Appliquées, Université Catholique de l’Ouest, Angers, France.
3. Université Bretagne Sud, UMR CNRS 6205, LMBA, Vannes, France.
4. Agence régionale de santé de Mayotte, Mamoudzou, France.

*Corresponding author: [solym.manou.abi@math.univ-poitiers.fr](mailto:solym.manou.abi@math.univ-poitiers.fr)*

---

## 📊 Study population

The MayCov serosurvey included **4,467 participants**, with the following questionnaire distribution:

| Characteristic | Sample size | Percentage |
|:---|---:|---:|
| Adult questionnaire | 3,490 | 78.1% |
| Child questionnaire | 977 | 21.9% |
| Female participants | 3,064 | 68.6% |
| Male participants | 1,403 | 31.4% |
| Blood samples analysed | 3,266 | 73.1% |

The contact questionnaire was completed by **3,396 respondents**, providing 16,980 age-specific answers.

### 🏠 Socio-demographic characteristics

The adult respondents' characteristics highlight several important features of the study population:

- 34.0% lived in sheet-metal or wooden housing.
- 33.1% reported having no water access inside their dwelling.
- 62.8% were not employed.
- The median household size was 4 members.

Calibration weights were used to account for differences between the sample and the target population.

## 🔬 Research objectives

The analysis focuses on five complementary questions:

1. **📈 Contact reporting and heaping:** How do unusually large reported contact counts affect estimated contact distributions?
2. **🧹 Exclusion rules:** How sensitive are contact estimates to the removal of respondents reporting at least 100 contacts?
3. **🔄 Reciprocity:** How substantial is the disagreement between the two directions of age-specific contact reporting?
4. **👨‍👩‍👧 Contacts among minors:** How do alternative assumptions about contacts between minors influence the resulting matrices?
5. **🧬 Epidemiological relevance:** How do alternative contact matrix constructions compare with the observed age-specific seroprevalence profile?

## 📉 Main findings

The analysis highlights several important findings:

- **Large reported counts:** 394 respondents reported at least 100 contacts in one or more age groups.
- **Sensitivity to exclusions:** The weighted mean number of outside-household contacts over two days was 133.7 across all complete questionnaires, compared with 14.8 and 13.3 under the two exclusion rules.
- **Persistent asymmetry:** Substantial disagreement between reciprocal age-specific reports remained after filtering.
- **Sensitivity to assumptions:** The epidemiological ranking of alternative matrices depended on assumptions about contacts among minors.
- **Household-only comparison:** At the reference level of outside-household contacts among minors, paired bootstrap intervals for the deviance difference included zero under both exclusion rules. When the minor-to-minor component was doubled, the household-only matrix achieved lower observed deviance than all reconciled candidates considered on the searched grid.

These findings emphasize the importance of reporting assumptions and conducting sensitivity analyses when estimating contact matrices from short questionnaires.

## 📊 Visual results

The paper includes several graphical analyses illustrating the main findings.

### 1. Distribution of reported contacts

The distribution of outside-household contacts reveals a substantial concentration of large reported values, including answers of 100 or more contacts.

### 2. Age-specific contact matrices

The estimated matrices compare:

- Outside-household contacts under the E100 exclusion rule.
- Outside-household contacts under the E50 exclusion rule.
- Household contact patterns.
- Daily contact matrices under alternative scenarios.

### 3. Reciprocity sensitivity

The reciprocity elasticity analysis examines how the estimated relationship between reciprocal contact reports changes according to the largest reported contact count retained.

## 🗂️ Repository structure

The repository is organized to facilitate reproducibility and transparent access to the computational analyses.

```text
mayotte-contact-matrices/
│
├── README.md
│
├── data/
│   └── README.md
│
├── R/
│   └── Statistical and computational functions
│
├── scripts/
│   ├── 01_data_preparation.R
│   ├── 02_sample_description.R
│   ├── 03_contact_reports.R
│   ├── 04_contact_matrices.R
│   ├── 05_reciprocity_analysis.R
│   └── 06_sensitivity_analysis.R
│
├── results/
│   ├── figures/
│   └── tables/
│
└── supplementary/
    └── Additional analyses and materials
```

*Note: This is the proposed repository organization; file names can be adjusted to match the final analysis scripts.*

## ⚙️ Reproducibility

The statistical analyses are intended to be reproducible using the accompanying computational scripts.

To reproduce the results:

1. Clone or download this repository.
2. Install the required statistical software and packages.
3. Obtain access to the authorized study data, where applicable.
4. Run the scripts in the appropriate order.
5. Consult the `results/` directory for generated figures and tables.

Further details on software versions, dependencies, and execution instructions will be provided as the repository is finalized.

## 🔐 Data availability and confidentiality

The MayCov serosurvey contains health-related information.

Individual-level participant data are therefore **not distributed through this repository**.

Access to the original data remains subject to the applicable authorization, data protection, and confidentiality requirements.

Only non-confidential materials, analytical code, and results suitable for public dissemination should be included in this repository.

## 📚 Keywords

`Contact matrices` · `Mayotte` · `Epidemiological modelling` · `Heaping` · `Reciprocity` · `Seroprevalence` · `Sensitivity analysis` · `Statistical estimation` · `Population health`

## 📄 Citation

If you use this repository or build upon the computational methods presented in this work, please cite the associated scientific publication:

Manou-Abi, S. M., Ali, E., & Balicchi, J. *Estimation and Sensitivity Analysis of Age-Specific Contact Matrices from a Short Survey in Mayotte.*

The complete bibliographic reference will be added following publication.

## 🤝 Acknowledgements

The authors acknowledge the contributions of the MayCov serosurvey participants and the teams involved in data collection and epidemiological investigations in Mayotte.

---

<p align="center">
  <b>🌍 Understanding contact patterns to improve epidemiological modelling and public health research.</b>
</p>

<p align="center">
  © Solym M. Manou-Abi · Université de Poitiers · France
</p>
