# Lyckliga_gatan
Code for geographical context
Geocontext: R Implementation
Overview
Geocontext is an R implementation of the k-nearest neighbor geographical context analysis methodology, originally developed in the Equipop software (Östh, Malmberg, & Andersson, 2014), current version (Östh, 2024). This implementation offers significant performance improvements over previous versions by utilizing a kd-tree method for distance matrix construction and efficient data.table operations to replace record-by-record loops.
Citation
If you use this code in your research, please cite:
•	Östh, John. (2024). Equipop. https://www.uu.se/en/department/human-geography/research/equipop: John Östh. Retrieved from https://www.uu.se/en/department/human-geography/research/equipop
•	Östh, John, Malmberg, Bo, & Andersson, Eva K. (2014). Analysing segregation using individualised neighbourhoods. In C. D. Lloyd, I. G. Shuttleworth, & D. W. S. Wong (Eds.), Social-spatial segregation: Concepts, processes and outcomes (pp. 135-162). Bristol: Policy Press.
•	Python implementation: P. Hennerdal, geocontext (2019), GitHub repository, https://github.com/PonHen/geocontext
•	This R implementation: Fowler, Christopher S. (2019). Geocontext: R Implementation. 

Installation
The Geocontext package requires the following R dependencies:
•	data.table
•	RANN
•	purrr
•	sf
•	ggplot2 (for visualization)
Install these packages using:
install.packages(c("data.table", "RANN", "purrr", "sf", "ggplot2"))

Files
•	geocontext_functions.R: Core functions for geocontext analysis
•	Geocontext_R_BM.R: Example script demonstrating usage with sample data
•	sample_data.csv: Example dataset for testing the implementation

Getting Started
1.	Download and extract the files to a directory on your computer.
2.	Open Geocontext_R_BM.R in RStudio or your preferred R environment.
3.	Set the data directory by modifying the data_dir variable near the beginning of the script:
4.  # Set the path to your data directory
5.  # Users should change this to match their own system
6.  data_dir <- "path/to/your/data/directory"
For example:
◦	Windows: data_dir <- "C:/Users/YourName/Documents/geocontext_data"
◦	Mac: data_dir <- "/Users/YourName/Documents/geocontext_data"
◦	Linux: data_dir <- "/home/yourname/documents/geocontext_data"
7.	Configure the analysis parameters in the setup section:
◦	yearList: Years for which to conduct analysis
◦	groupList: Population group columns in your data
◦	totalID: Column containing total population
◦	X.coord and Y.coord: Coordinate column names in your data
◦	kValueList: Context sizes to analyze (k-values)
◦	samp.le: Set to TRUE to run on a sample of data first
8.	Run the script to perform the analysis.

Input Data Format
Your input CSV file should contain:
•	X and Y coordinates (specified by X.coord and Y.coord parameters)
•	Total population column (specified by totalID parameter)
•	Group population columns (specified by groupList parameter)

Output
The script generates two types of output files in your data directory:
1.	R data file (.Rdata): Contains the complete geocontext analysis results with all variables
2.	CSV file (result.csv): Contains the same results in CSV format for use with other software
Output filenames include the k-value range and year of analysis for easy identification.

Sample Data
The provided sample_data.csv contains synthetic data to demonstrate the functionality. Users should replace this with their own data following the required format.

Performance Considerations
•	The implementation is designed for efficiency with large datasets
•	For very large datasets, consider sampling to test functionality first (set samp.le = TRUE)
•	Memory requirements scale with the size of the input data and largest k-value
•	
License
MIT License

Copyright (c) 2019, Chris Fowler

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


