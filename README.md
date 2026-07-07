# Evaluating open-source approaches for estimating level-of-service attributes in transport choice modelling

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21224724.svg)](https://doi.org/10.5281/zenodo.21224724)

Replication code for:
> Roberts, H. S., Calastri, C., Batley, R. (under review) "Evaluating open-source approaches for estimating level-of-service attributes in transport choice modelling". Manuscript submitted for publication.


## Overview
This repository contains the code used to process travel survey data, compute routing attributes using three methods, and estimate revealed-preference choice models.

## Repository structure
- `input/raw/` – raw input data (not included)
- `input/processed/` – processed datasets (not included)
- `R/` – processing and modelling scripts
- `output/` – model outputs and figures (not included)
- `docs/` – workflow documentation

## Workflow
The complete workflow is documented in:
👉 **docs/workflow.html**

## Data availability
The following inputs are not included:
- DECISIONS survey data (due to privacy concerns)
- OS Multimodal Routing Network (due to licensing restrictions)
- Google Routes API key

## Requirements
- R ≥ 4.x
- Packages: `tidyverse`, `sf`, `apollo`, `r5r`, `httr`, `jsonlite`, `quantreg`, `patchwork`, `losdos` (available from author's github)

## Citation
If you use this code in your research, please cite the underlying paper and this repository as follows:

Roberts, H. S., Calastri, C., Batley, R. (under review) "Evaluating open-source approaches for estimating level-of-service attributes in transport choice modelling". Manuscript submitted for publication.

Roberts, H. (2026) “Replication code for evaluation of attribute estimation methods”. Zenodo. doi:10.5281/zenodo.21224724.


**BibTeX:**

```bibtex
@article{roberts_under_review_evaluating,
  author      = {Roberts, Harry Samuel and Calastri, Chiara and Batley, Richard},
  title       = {Evaluating open-source approaches for estimating level-of-service attributes in transport choice modelling},
  year        = {under review},
  note        = {Manuscript submitted for publication}
}

@software{roberts_2026_21224724,
  author       = {Roberts, Harry},
  title        = {Replication code for evaluation of attribute
                   estimation methods},
  month        = jul,
  year         = 2026,
  publisher    = {Zenodo},
  version      = {v1.0.0},
  doi          = {10.5281/zenodo.21224724},
  url          = {https://doi.org/10.5281/zenodo.21224724},
}
```

## License
MIT License. See LICENSE file for details.

## Author

Harry Roberts ([ts22hr@leeds.ac.uk](mailto:ts22hr@leeds.ac.uk))

Institute for Transport Studies, University of Leeds

