# Georgia-Air-Quality
Analysis and production of graphics to understand and display for the public trends in Georgia Air Quality over the last decade.

### Notes about the Ozone Data
* Note that the Pike County Ozone Data is in different units. To convert to the same units. It is necessary to divide the Pike County Data by 1000. This is because the Pike data was collected by EPA while the rest is EPD data.

* Georgia DOT data is only available for a few years. It can be discarded

* The months of November, December, January, and February are not a part of the official data set. They are collected as setup for the ozone year and should not be included as a part of the analysis because they will likely lower aggregations artificially.

* Any POC coded 2  are calibration data and can be discarded __HOWEVER The GA Forestry Data is all marked as POC 2. So what do I do with that?__ 

Ask Janet about Qualifiers and how mean is calculated

Ask Susan about Macon W and Columbus CRLAB sites

### Notes about the other Data
SO2 has POC 1,2,3 and 5, should I remove them? Should I use them?
POC 2 -> Keep it
POC 3 -> L&A
POC 5 ->Brunswick and Columbus Airport


Lead has POC 1,2,3, and 4. Same question.

PM 2.5 has POC 1,2,3 same question