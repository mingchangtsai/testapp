# App Outline

This application is intended to assist with the processing of pulmonary gas exchange and ventilatory data measured using the Parvo Medics True One 2400 metabolic cart. Other metabolic carts are currently not supported.

Please refer to the **Usage Guide** tab for instructions and information on using the application before your first use.

<br>

## Contact
If you have any questions, please contact Andy Hung at ahung@csipacific.ca

<br>

*Last update: Sept 9, 2024*

<br>

**Fixes in latest update:**
- Peak intensity calculation fixed
- Changed so real time is entered into 'End Time (m:ss)' and 'When Second Protocol Begins (m:ss)', rather than MMC time
- Removed extra rows of data that exceed the end test time
- More stop messages so errors can be better diagnosed
- Added 'Rowing Ergometer' feature
- Added 'Training Zones' feature

**Planned large updates to come:**
- Automatically update a database
- Ramp test
- MRT calculation

**Known bugs / Fixes to come:**
- If use 'Break' and workload does not equal to 0, will populate odd looking row into 'Average Stage Data Table'
- Ordering of variables in graphs (e.g., 'Time' in m:ss format)
- Improve UI layout
