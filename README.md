RLandsat
========

Collection of R code for Landsat imagery manipulation and Land Cover Classification.
Primarily developed for Landsat 8 OLI imagery.
Scripts include:

- Landsat 8 Image and MTL reading
- Cropping and Masking images when a ROI is provided
- Topographic and Atmospheric correction using SRTM or ASTER GDEM
- PCA on stacked bands
- Vegetation index
- Unsupervised (k-means cluster) classification
- Signature development and simple "purify" aproach
- Calculate Separability Index based on Jeffries-Matusita Distance
- Supervised classification with Random Forest and Structure Vector Machine
- Create WEKA files from Landsat bands (stack)

