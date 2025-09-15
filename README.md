<img src="assets/camp-logo.png" alt="CAMP Logo" width="20" style="vertical-align: middle;"> <span style="font-size: 3em; font-weight: bold;"> CAMP – (Re)Constructing the Archaeology of Mobile Pastoralism</span>

This project implements a complete analytical framework for portable X-ray fluorescence (pXRF) geochemical data from ethno-archaeological sites, specifically focused on understanding spatial patterns of human activity areas. The workflow integrates compositional data analysis with advanced geostatistical modeling techniques to process, analyze, and interpret geospatial geochemical datasets.

- **Data Preparation & Quality Control**: Handles pXRF data preprocessing including spatial coordinate matching, limit of detection (LOD) correction, element selection based on analytical reliability, and compositional data closure using expectation-maximization algorithms for below-detection-limit imputation.

- **Exploratory Statistical Analysis**: Implements compositional data analysis using centered log-ratio (CLR) and isometric log-ratio (ILR) transformations, principal component analysis on CLR-transformed data, correlation analysis adapted for compositional constraints, cluster analysis using Aitchison distances, and multivariate statistical testing including PERMANOVA and discriminant analysis.

- **Spatial Analysis**: Applies exploratory spatial data analysis including elemental mapping, inverse distance weighting interpolation, swath plots for directional trends, variogram analysis for spatial correlation structures, and anisotropy assessment for directional spatial dependencies.

- **Geostatistical Modeling**: Utilizes advanced cokriging techniques with linear models of coregionalization (LMC), universal cokriging for non-stationary spatial processes, omnidirectional and directional variogram modeling, Maximum/Minimum Autocorrelation Factor (MAF) analysis for dimensionality reduction, and comprehensive model validation through cross-validation and spatial accuracy assessment.

## Scripts

- `data_preparation`: Preprocesses raw pXRF data including spatial matching, LOD correction, element filtering, and compositional imputation for analysis‑ready datasets.  

- `exploratory_data_analysis`: Performs statistical analysis of compositional data using CLR/ILR transformations, PCA, clustering, and multivariate hypothesis testing.  

- `exploratory_spatial_analysis`: Conducts spatial exploration through elemental mapping, interpolation, variogram analysis, and directional dependency assessment.  

- `geostatistics`: Implements advanced cokriging models with LMC fitting, MAF analysis, and comprehensive spatial prediction validation.

## Getting Started

To set up and use this project, follow these steps:

1. Clone the repository:

   ```bash
   git clone https://github.com/project-camp/camp-public.git
   cd camp-public
   ```
2. Create and activate the Conda environment:

   ```bash
   conda env create -f ./environment/environment.yml
   conda activate camp-public
   ```

## Contributing

This project is part of ongoing research. For collaboration opportunities, methodological discussions, or access to datasets, please contact the research team through the issues section or reach out to the corresponding authors listed in associated publications.

## License

Copyright 2025 UPF, all rights reserved.
