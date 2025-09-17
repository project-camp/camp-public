<div align="center">
  <img src="assets/camp-logo.png" alt="CAMP Logo" width="80">
  <h1 style="border-bottom:none;">CAMP</h1>
  <p><strong>(Re)Constructing the Archaeology of Mobile Pastoralism</strong></p>
</div>

<div align="center">

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Conda Env](https://img.shields.io/badge/conda%20env-brightgreen.svg)](./environment/environment.yml)
[![Project Status](https://img.shields.io/badge/status-active-brightgreen.svg)](https://github.com/project-camp/camp-public)

</div>

This project provides a complete analytical framework for portable X-ray fluorescence (pXRF) geochemical data from ethno-archaeological sites. The workflow integrates compositional data analysis with advanced geostatistical modeling to understand spatial patterns of human activity areas.

---

## 🔬 Core Features

<details>
<summary><strong>Data Preparation & Quality Control</strong></summary>
Handles pXRF data preprocessing including spatial coordinate matching, limit of detection (LOD) correction, element selection based on analytical reliability, and compositional data closure using expectation-maximization algorithms for below-detection-limit imputation.
</details>

<details>
<summary><strong>Exploratory Statistical Analysis</strong></summary>
Implements compositional data analysis using centered log-ratio (CLR) and isometric log-ratio (ILR) transformations, principal component analysis on CLR-transformed data, correlation analysis adapted for compositional constraints, cluster analysis using Aitchison distances, and multivariate statistical testing including PERMANOVA and discriminant analysis.
</details>

<details>
<summary><strong>Spatial Analysis</strong></summary>
Applies exploratory spatial data analysis including elemental mapping, inverse distance weighting interpolation, swath plots for directional trends, variogram analysis for spatial correlation structures, and anisotropy assessment for directional spatial dependencies.
</details>

<details>
<summary><strong>Geostatistical Modeling</strong></summary>
Utilizes advanced cokriging techniques with linear models of coregionalization (LMC), universal cokriging for non-stationary spatial processes, omnidirectional and directional variogram modeling, Maximum/Minimum Autocorrelation Factor (MAF) analysis for dimensionality reduction, and comprehensive model validation through cross-validation and spatial accuracy assessment.
</details>

---

## 📂 Project Structure & Scripts

The analytical workflow is divided into four sequential scripts:

-   `01_data_preprocessing`: Prepares raw pXRF data for analysis.
-   `02_multivariate_analysis`: Performs statistical analysis and clustering.
-   `03_spatial_analysis`: Conducts spatial exploration and variogram analysis.
-   `04_geostatistical_modeling`: Implements advanced cokriging models and validation.

---

## 🚀 Getting Started

This guide provides comprehensive instructions for setting up the project's computational environment.

**1. Download and Install Miniforge:**
Visit the Miniforge page and download for all operating systems: [Miniforge](https://github.com/conda-forge/miniforge/releases/latest)

- Windows: Use the Miniforge3-Windows-x86_64.exe
- macOS: Download either:
   ``` 
   curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh"
   bash Miniforge3-$(uname)-$(uname -m).sh
   ``` 
- Linux: 
   ``` 
   curl -L -O "https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh"
   bash Miniforge3-$(uname)-$(uname -m).sh
   ```

**2. Clone the repository:**

   ```bash
   git clone [https://github.com/project-camp/camp-public.git](https://github.com/project-camp/camp-public.git)
   cd camp-public
   ```
**3. Create and activate the Conda environment:**

   ```bash
   conda env create -f ./environment/environment.yml
   conda activate camp-public
   ```

**4. Install CRAN packages:**

```
Rscript ./environment/install_cran_packages.r
```

---

## 📜 License

Copyright 2025 UPF, all rights reserved.

---

## 🙏 Acknowledgments

This work is supported by ERC grant (CAMP-CoG, n. 101088842). Views and opinions expressed are however those of the author only and do not necessarily reflect those of the European Union or the European Research Council. Neither the European Union nor the granting authority can be held responsible for them.

<div align="center">
<img src="assets/erc_acknowledgements.png" alt="Funded by the European Union and the European Research Council" width="150">
</div>