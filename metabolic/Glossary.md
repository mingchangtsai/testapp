# Glossary
This section explains what each of the variables mean.

### Test Summary Variables
- *Name*: Name of athlete in "last name, first name" format.
- *Test_Date*: Date of when the test was completed in the TrueOne 2400 MMC computer.
- *Test_Time*: Time of day when the test was began in the TrueOne 2400 MMC computer.
- *Age*: Age of athlete in years.
- *Sex*: Sex of athlete.
- *Height*: Height of athlete in centimeteres.
- *Weight*: Weight of athlete in kilograms.
- *BMI*: Body mass index of athlete in kg/m<sup>2</sup>.
- *Temperature*: Temperature entered into the TrueOne 2400 MMC computer when test was administered.
- *RH*: Relative humidity entered into the TrueOne 2400 MMC computer when test was administered.
- *Test_duration*: Duration of the test.
- *Workload_peak*: The peak workload completed during the test and calculated in a pro rata manner.
- *Peak values*: The peak value achieved during the test and calculated using duration set in the 'Calculation Intervals for Averages during Processing' argument in the 'Advanced Settings'.
- *VO2max_age_pred*: The predicted V̇O<sub>2</sub>max of the individual, based on their sex, age, and weight. Reference equation derived from the FRIEND cohort (*doi:* [10.1016/j.pcad.2017.03.002](https://doi.org/10.1016/j.pcad.2017.03.002)).
- *VO2max_percent_age_pred*: The ratio of the V̇O<sub>2</sub>peak measured during the test to the V̇O<sub>2</sub>max predicted using the FRIEND reference equation.
- *VO2max_percentile*: Percentile the V̇O<sub>2</sub>peak measured during the tes falls under, based on reference standards determined from the FRIEND cohort (*doi:* [10.1016/j.mayocp.2021.08.020](https://doi.org/10.1016/j.mayocp.2021.08.020)). Percentiles are only available for tests completed using a treadmill or cycling ergometer.
- *VO2max_percentile_desc*: Qualitative descriptors of V̇O<sub>2</sub>peak measured during the test. Based on reference standards (i.e., percentiles) determined from the FRIEND cohort (*doi:* [10.1016/j.mayocp.2021.08.020](https://doi.org/10.1016/j.mayocp.2021.08.020)) and descriptors from the [ACSM's Guidelines for Exercise Testing and Prescription, 11th. ed](https://www.acsm.org/education-resources/books/guidelines-exercise-testing-prescription). Descriptors are only available for tests completed using a treadmill or cycling ergometer.
- *Workload at VO2max*: The estimated workload at V̇O<sub>2</sub>max calculated by firstly regressing V̇O<sub>2</sub> on workload, and then extrapolating the relationship to the measured V̇O<sub>2</sub>peak on the test.
- *Fatmax_workload*: The estimated workload that elicits the highest rate of fat oxidation. Determined by calculating fat oxidation values for each workload, depicting it graphically as a function of workload, and then constructing a third-order polynomial curve with intersection in (0,0) (*doi:* [10.1007/s00421-006-0290-x](https://doi.org/10.1016/j.pcad.2017.03.002)).
- *Fatmax_HR*: The estimated %HRpeak that elicits the highest rate of fat oxidation. Calculated using the same method as Fat<sub>max</sub>_workload.
- *Fatmax_VO2*: The estimated %VO2peak that elicits the highest rate of fat oxidation. Calculated using the same method as Fat<sub>max</sub>_workload.
- *MFO_gmin*: The estimated maximal rate of fat oxidation in grams/minute. Calculated using the same method as Fat<sub>max</sub>_workload.

### Average Stage Data
- *Time_mss*: End time in minute:second format for the given stage. 
- *Time_m*: End time in minute decimal format for the given stage.
- *Time_s*: End time in seconds for the given stage.
- *CHO_gmin*: CHO oxidation in grams/minute, estimated using the [Peronnet & Massicotte, 1991](https://pubmed.ncbi.nlm.nih.gov/1645211/) equation.
- *FAT_gmin*: Fat oxidation in grams/minute, estimated using the [Peronnet & Massicotte, 1991](https://pubmed.ncbi.nlm.nih.gov/1645211/) equation.
- *CHO_kcalmin*: CHO oxidation in kilocalories/minute, with the assumption that 1 gram of CHO provides approximately 4 kilocalories.
- *FAT_kcalmin*: Fat oxidation in kilocalories/minute, with the assumption that 1 gram of fat provides approximately 9 kilocalories.
- *Total_kcalmin*: Estimated total energy expenditure in kilocalories/minute, with the assumption of zero protein metabolism.
- *Total_kcalhr*: Estimated total energy expenditure in kilocalories/hour, with the assumption of zero protein metabolism.
- *O2_kJL*: The amount of energy expended per liter of O<sub>2</sub> consumed in kilojoules/liter, estimated using the [Peronnet & Massicotte, 1991](https://pubmed.ncbi.nlm.nih.gov/1645211/) equation.
- *O2_kcalL*: The amount of energy expended per liter of O<sub>2</sub> consumed in kilocalories/liter, estimated using the [Peronnet & Massicotte, 1991](https://pubmed.ncbi.nlm.nih.gov/1645211/) equation.
- *WE_kJmin*: Estimated aerobic work expenditure in kilojoules/minute.
- *P_Js*: Estimated aerobic power in joules/second (i.e., watts).
- *EE_kcalmin*: Estimated aerobic energy expenditure in kilocalories/minute.
- *MP_kJs*: Estimated metabolic power in kilojoules/second, estimated using the [Peronnet & Massicotte, 1991](https://pubmed.ncbi.nlm.nih.gov/1645211/) equation.
- *MP_Wkg*: Estimated metabolic power in watts/kilogram, estimated using the [Peronnet & Massicotte, 1991](https://pubmed.ncbi.nlm.nih.gov/1645211/) equation.
- *RE_mlkgkm*: Running economy expressed in milliliters of O<sub>2</sub>/kilogram/kilometer.
- *RE_kcalkm*: Running economy expressed in kilocalories/kilometer.
- *RE_kcalkgkm*: Running economy expressed in kilocalories/kilogram/kilometer.
- *CE_LminW*: Cycling economy expressed in liters of O<sub>2</sub>/minute/watts.
- *CE_WLmin*: Cycling economy expressed in watts/(liter of O<sub>2</sub>/minute).
- *CE_kJL*: Cycling economy expressed in kilojoules/liter of O<sub>2</sub>.
- *GE_Js*: Gross economy calculated as the ratio of work accomplished to energy expended in joules/second.
- *GE_kcalmin*: Gross economy calculated as the ratio of work accomplished to energy expended in kilocalories/minute.
- *Perc_HR_peak*: Percent of peak heart rate achieved during the test.
- *Perc_VO2_peak*: Percent of peak V̇O<sub>2</sub>max achieved during the test.