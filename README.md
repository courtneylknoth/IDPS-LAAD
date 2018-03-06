Shiny Analytic Data Product
================
Capt Spencer Butt
06 March, 2018

[![Build Status](https://travis-ci.org/SpencerButt/IDPS-LAAD.svg?branch=master)](https://travis-ci.org/SpencerButt/IDPS-LAAD) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/SpencerButt/IDPS-LAAD?branch=master&svg=true)](https://ci.appveyor.com/project/SpencerButt/IDPS-LAAD)

### 1.1 Name

IDPS Log Autoencoder Anomaly Detector (IDPS-LAAD)

### 1.2: IDPS-LAAD Title

This analytic data product is designed to construct an Autoencoder Neural Network (ANN) to detect anomalous data observations within Intrusion Detection Prevention System (IDPS) log file data. This analytic will test multiple ANN hyperparameters to determine the optimal settings supporting anomaly detection assuming that anomalous observations correspond to potentially adverse network traffic

### 1.3: IDPS Log Autoencoder Anomaly Detector Description

IDPS-LAAD is designed aid expeditious discovery of potentially hostile actions on a computer network protected by various intrusion detection sensors. The IDPS log files are consolidated by independent software systems to be analyzed by human cyber security experts. Most cyber security agencies have a limited cyber security staff, making the review of the number of cyber logs collected during normal day-to-day operations problematic. Additionally, cyber security staffs require a spectrum of analytic tools to analyze cyber security files to limit the probability of hostile cyber actions going undetected.

IDPS-LAAD is a tool to be used by cyber security experts with limited programming experience, and little-to-no statistical and/or machine learning expertise. Users will interact with the program via graphical user interface using keyboard and mouse inputs a with which a standard computer user is familiar. There are no known security concerns nor are there appearance constrains to which the program must adhere.

IDPS-LAAD is designed to read in a .csv containing the IDPS cyber log. The program will allow users to select what features (variables) in the data they wish to analyze. The program will then automatically prepare the data for ANN analysis. Once the data is prepared for ANN analysis, the IDPS-LAAD will automatically generate a designed experiment to test ANN hyperparameters, and subsequently run ANNs testing each of the hyperparameters. Once the 'optimal' hyperparameters are found the the analytic will analyze the IDPS cyber data and graphically display the results as well as output a user defined top 'n' anomalous observations.

Section 2
---------

<table style="width:100%;">
<colgroup>
<col width="23%" />
<col width="3%" />
<col width="4%" />
<col width="22%" />
<col width="11%" />
<col width="8%" />
<col width="7%" />
<col width="10%" />
<col width="7%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Analytic Feature</th>
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
<td align="left">Import IDPS Cyber Log File</td>
<td align="right">7</td>
<td align="left">Not Started</td>
<td align="left">GUI based file import</td>
<td align="left">.csv IDPS Log</td>
<td align="left">data frame</td>
<td align="left">Information to User</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="even">
<td align="left">User feature Selector</td>
<td align="right">8</td>
<td align="left">Not Started</td>
<td align="left">GUI based data preparation</td>
<td align="left">GUI Input Selection</td>
<td align="left">data frame</td>
<td align="left">Information to User</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="odd">
<td align="left">Adjust Data Frame for Missing Categorical Observations</td>
<td align="right">9</td>
<td align="left">Not Started</td>
<td align="left">automatically format data for ANN analysis</td>
<td align="left">None</td>
<td align="left">data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="even">
<td align="left">Adjust Data Frame for Missing Port Number Observations</td>
<td align="right">10</td>
<td align="left">Not Started</td>
<td align="left">automatically format data for ANN analysis</td>
<td align="left">None</td>
<td align="left">data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="odd">
<td align="left">Adjust Data Frame for Missing IP Address Observations</td>
<td align="right">11</td>
<td align="left">Not Started</td>
<td align="left">automatically format data for ANN analysis</td>
<td align="left">None</td>
<td align="left">data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="even">
<td align="left">Adjust Data Frame for Missing Continuous Observations</td>
<td align="right">12</td>
<td align="left">Not Started</td>
<td align="left">automatically format data for ANN analysis</td>
<td align="left">None</td>
<td align="left">data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="odd">
<td align="left">One-Hot Encode Categorical Features</td>
<td align="right">13</td>
<td align="left">Not Started</td>
<td align="left">automatically format data for ANN analysis</td>
<td align="left">None</td>
<td align="left">data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="even">
<td align="left">Convert IP Addresses to 32 Binary Columns</td>
<td align="right">14</td>
<td align="left">Not Started</td>
<td align="left">automatically format data for ANN analysis</td>
<td align="left">None</td>
<td align="left">data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="odd">
<td align="left">Convert Port Number to 16 Binary Columns</td>
<td align="right">15</td>
<td align="left">Not Started</td>
<td align="left">automatically format data for ANN analysis</td>
<td align="left">None</td>
<td align="left">data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="even">
<td align="left">Scale each column of the data frame</td>
<td align="right">16</td>
<td align="left">Not Started</td>
<td align="left">automatically format data for ANN analysis</td>
<td align="left">None</td>
<td align="left">data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="odd">
<td align="left">Split data frame into train and test subsets</td>
<td align="right">17</td>
<td align="left">Not Started</td>
<td align="left">automatically format data for ANN analysis</td>
<td align="left">None</td>
<td align="left">data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="even">
<td align="left">Query User for Custom or Default Hyperparameter Designed Experiment</td>
<td align="right">18</td>
<td align="left">Not Started</td>
<td align="left">GUI based method to select novice/expert modes of operation</td>
<td align="left">GUI Input Selection</td>
<td align="left">None</td>
<td align="left">Internal to Analytic</td>
<td align="left">No</td>
<td align="left">No</td>
</tr>
<tr class="odd">
<td align="left">Build Default or Import Custom Experimental Design</td>
<td align="right">1</td>
<td align="left">Not Started</td>
<td align="left">GUI based method to select novice/expert modes of operation</td>
<td align="left">.csv Designed Experiment or None</td>
<td align="left">DOE data frame</td>
<td align="left">Internal to Analytic</td>
<td align="left">Yes</td>
<td align="left">Yes (Default Only)</td>
</tr>
<tr class="even">
<td align="left">Run Designed Experiment</td>
<td align="right">2</td>
<td align="left">Not Started</td>
<td align="left">Identify best ANN hyperparameter settings</td>
<td align="left">GUI Execute Button</td>
<td align="left">Status Bar</td>
<td align="left">Information to User</td>
<td align="left">Yes</td>
<td align="left">Yes</td>
</tr>
<tr class="odd">
<td align="left">Present Designed Experiment Results</td>
<td align="right">3</td>
<td align="left">Not Started</td>
<td align="left">Display Hyperparameter experimental design results</td>
<td align="left">None</td>
<td align="left">Test Design Results</td>
<td align="left">Information to User</td>
<td align="left">Yes</td>
<td align="left">Yes</td>
</tr>
<tr class="even">
<td align="left">Construct ANN using Best Hyperparameters Found In Designed Experiment</td>
<td align="right">4</td>
<td align="left">Not Started</td>
<td align="left">ANN trained to detect anomalies</td>
<td align="left">GUI Execute Button</td>
<td align="left">Status Bar</td>
<td align="left">Information to User</td>
<td align="left">Yes</td>
<td align="left">Yes</td>
</tr>
<tr class="odd">
<td align="left">Use ANN to Detect Anomalies</td>
<td align="right">5</td>
<td align="left">Not Started</td>
<td align="left">Anomaly Detection</td>
<td align="left">GUI Execute Button</td>
<td align="left">Histogram of Anomalies</td>
<td align="left">Identify Anomalies</td>
<td align="left">Yes</td>
<td align="left">Yes</td>
</tr>
<tr class="even">
<td align="left">Display Top 'n' Anomalies</td>
<td align="right">6</td>
<td align="left">Not Started</td>
<td align="left">List of most likely anomales to evaluate for hostile cyber action</td>
<td align="left">Number (n) anomalies to display</td>
<td align="left">List/Table</td>
<td align="left">Identify Anomalies</td>
<td align="left">Yes</td>
<td align="left">Yes</td>
</tr>
</tbody>
</table>
