Shiny Analytic Data Product
================
Capt Spencer Butt
r format(Sys.Date(), '%d %B %Y')

Section 1
---------

### 1.1 Name

IDPS Log Autoencoder Anomaly Detector (IDPS-LAAD)

### 1.2: IDPS-LAAD Title

This analytic data product is designed to construct an Autoencoder Neural Network (ANN) to detect anomalous data observations within Intrusion Detection Prevention System (IDPS) log file data. This analytic will test multiple ANN hyperparameters to determine the optimal settings supporting anomaly detection assuming that anomalous observations correspond to potentially adverse network traffic.

### 1.3: IDPS Log Autoencoder Anomaly Detector Description

IDPS-LAAD is designed aid expeditious discovery of potentially hostile actions on a computer network protected by various intrusion detection sensors. The IDPS log files are consolidated by independent software systems to be analyzed by human cyber security experts. Most cyber security agencies have a limited cyber security staff, making the review of the number of cyber logs collected during normal day-to-day operations problematic. Additionally, cyber security staffs require a spectrum of analytic tools to analyze cyber security files to limit the probability of hostile cyber actions going undetected.

IDPS-LAAD is a tool to be used by cyber security experts with limited programming experience, and little-to-no statistical and/or machine learning expertise. Users will interact with the program via graphical user interface using keyboard and mouse inputs a with which a standard computer user is familiar. There are no known security concerns nor are there appearance constrains to which the program must adhere.

IDPS-LAAD is designed to read in a .csv containing the IDPS cyber log. The program will allow users to select what features (variables) in the data they wish to analyze. The program will then automatically prepare the data for ANN analysis. Once the data is prepared for ANN analysis, the IDPS-LAAD will automatically generate a designed experiment to test ANN hyperparameters, and subsequently run ANNs testing each of the hyperparameters. Once the 'optimal' hyperparameters are found the the analytic will analyze the IDPS cyber data and graphically display the results as well as output a user defined top 'n' anomalous observations.

Section 2
---------

<table style="width:100%;">
<colgroup>
<col width="6%" />
<col width="7%" />
<col width="5%" />
<col width="11%" />
<col width="9%" />
<col width="6%" />
<col width="14%" />
<col width="23%" />
<col width="16%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Feature</th>
<th align="right">Priority</th>
<th align="left">Status</th>
<th align="left">Value to User</th>
<th align="left">User Inputs</th>
<th align="left">Outputs</th>
<th align="left">Purpose of Output</th>
<th align="left">Sufficient Time to Deadline?</th>
<th align="left">Required in Version?</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Read in .csv</td>
<td align="right">5</td>
<td align="left">Not Started</td>
<td align="left">Allows user to specify what IDPS log file to analyze for anomalies indicating potential hostile cyber traffic</td>
<td align="left">File selection</td>
<td align="left">None</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Select Features</td>
<td align="right">6</td>
<td align="left">Not Started</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">Format Data for ANN</td>
<td align="right">7</td>
<td align="left">Not Started</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Build Designed Experiment</td>
<td align="right">8</td>
<td align="left">Not Started</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">Evaluate ANNs</td>
<td align="right">1</td>
<td align="left">Not Started</td>
<td align="left">Enable testing of multiple hyperparameter settings for ANNs to determine optimal settings</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Train Final ANN</td>
<td align="right">2</td>
<td align="left">Not Started</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left">Display Anomalous</td>
<td align="right">3</td>
<td align="left">Not Started</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left">Display top 'n' Anomalous</td>
<td align="right">4</td>
<td align="left">Not Started</td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
<td align="left"></td>
</tr>
</tbody>
</table>

IDPS-LAAD will perform the following functions:

1.  Read in a .csv file composed of individual IDPS cyber log observations described by features

2.  Allow the user to select which features the user wants to retain for analysis

3.  Automatically convert the selected features into a format compatible with Autoencoder Neural Networks
    1.  Adjust for missing data
        1.  Add a missing column for Port Numbers and IP Addresses with a value of '1' where that data is missing
        2.  Missing categorical data will be flagged with a 'missing' label

    2.  One-Hot Encode Categorical Features
    3.  Convert IP Addresses into 32-bit binary number, each column containing one bit
    4.  Scale the data frame
    5.  Split the data frame into training and test data frames

4.  Construct a designed experiment to test ANN hyperparameters

5.  Evaluate the ANNs using the hyperparameters in 4.

6.  Automatically select the optimal hyperparameters and build an ANN using those hyperparameters

7.  Graphically display via histogram anomalous observations using the ANN built in 6.

8.  Output a user specified top 'n' anomalous observations
