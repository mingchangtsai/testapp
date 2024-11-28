### General Overview
There are three tabs:
1. <ins>About the app</ins>: Find information to help you get started and understand key terms found in the application, such as the **Usage Guide** and **Glossary**
2. <ins>Step Test</ins>: Enter your step test protocol and analysis preferences. Preview the results with tables and plots to visualize the output.
3. <ins>ExPhysLab Tools</ins>: Access the *Lactate Thresholds App* and *Exercise Thresholds App* from ExPhysLab to help you identify the physiological thresholds.
<br>

### Instructions on how to process the data

#### Upload File
- Upload the file containing the expired gas data from the metabolic cart. The file must adhere to the following:
  1. The file must be in *.xlsx* or *.xls* format.
     - Currently, the *.xls* file directly exported from the True One 2400 is not compatible.
     - As an imperfect workaround, please open and re-save the file as either a *.xlsx* or *.xls* file.
  2. The *Time* column in the file must be in **m:ss** format, not in decimal minutes.
  3. The following columns must be present in the file and in the following order:
     - "TIME", "VO2", "VO2/kg", "VCO2", "RER", "RR", "Vt", "VE", "VE/VO2", "VE/VCO2", "FEO2", "FECO2", and "HR".

#### Written Info
- Fill in the table with the information that was manually recorded during the test.
- Indicate how many steps (i.e., stages) were completed (including rest). You can either use the buttons on the screen, or manually type in the number of stages in the box.
  - Please note: This table contains *Cadence*, *Stroke Rate*, and *Grade*. As such, ensure you populate enough rows for all the stages, even if *Lactate* or *RPE* data was not collected for that stage.

#### Protocol Options
- Fill in the test protocol.
  - For treadmill assessments, please input the work rate in kilometers/hour, rather than miles/hour.
  - For cycle and rowing ergometer assessments, you have the option of inputting the work rates in watts or watts/kilogram.
- For *End Time (m:ss)*, input the actual duration of the test, not when the test ended according to MMC time.
  - E.g., If an athlete rests for 1 minute prior to the start of the test and cycles for 25:03 afterwards, enter '25:03' and not '26:03'.

#### Baseline Options
- Indicate whether a baseline stage was completed prior to the start of the test. If a baseline stage was completed, indicate the duration and intensity of the stage.
  - E.g., If an athlete rests for 5 minutes prior to the start of the test, enter '5:00' and '0'.
  - E.g., If an athlete warm-ups for 10 minutes at 75 W prior to the start of the test, enter '10:00' and '75'.

#### Break Options
- Indicate whether there are 'breaks' (or a temporary change in protocol) in between the stages of the test. If breaks were used, indicate the duration and intensity of the stage.
  - E.g., If the athlete completes a GXT with blood lactate measurements on the treadmill and stopped for 30 seconds between each stage, enter '0:30' and '0'.

#### Second Protocol
- Indicate whether there was a permanent change in protocol at some point during the test. If so, indicate when this change occured, the starting intensity of the new protocol, the increment in work rate between stages, and then duration of the stages.
- For *When Second Protocol Begins (m:ss)*, input the time when the protocol transition occurs during the test, not when the test ended according to MMC time.

#### Advanced Options
- *Calculation Intervals for Averages during Processing (seconds)* sets the duration of data that will be used in the assignment of an "average" for a given stage.
  - E.g., "30" means the last 30 seconds of data for a given stage will be averaged.
- *Include Last Row of Each Stage in Averaging* indicates whether the last row of data in each stage will be included in the calculation of the "average" for a given stage during "protocol_1".
  - Averages for "protocol_2" stages are always calculated including the last row.
  - Please note: This feature is currently coded to only allow the user to exclude the very last row of a stage. As such, it is influenced by the exported average interval duration.
- *Variables of Interest* indicates which variables you are interested in calculating the "peak".
  - This is reflected in the "test_summary" file.
  - Defaults to "Lactate", "RPE", "HR", "VO2", and "VO2kg".

<br>

### Previewing the outputs
After you've entered all the arguments and initialized the processing of the data, a *Success* alert will appear. However, please be cognizant that this simply informs you that the code was successfully able to be run. As such, it is recommended you scan the outputs to ensure the data has been processed correctly.

This can be done in tabular form, of which there are 3 tabular outputs:
1. <ins>Test Summary</ins>: Contains the test-level data, such as athlete information, test information, and peak values.
2. <ins>Average Stage Data</ins>: Contains average data for each stage and metrics that can be subsequently derived, such as substrate oxidation and exercise efficiency.
3. <ins>Parvo EG Data</ins>: Is the dataset containing the highest resolution data.

If there are errors, please go back to the **Options** tab and ensure all the protocol arguments have been entered correctly. If need be, the tables can be manually altered; simply double-click the cell and change the value. For example, the peak HR registered on a Polar watch HR receiver may be greater than that on the True One. As such, you can manually change the HR_peak value. If you're happy with the tables as they are, the tables can be downloaded in either Excel and CSV formats. 

To quickly graphically visualize the data, the **Graphs** sub-tab allows you to plot any two of the variables against one another from either the <ins>Average Stage Data</ins> or the <ins>Parvo EG Data</ins> datasets. If any values are updated in the tables, they will also be reflected in the plots.