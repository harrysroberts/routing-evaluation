# Evaluating open-source approaches for estimating level-of-service attributes in transport choice modelling

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21224724.svg)](https://doi.org/10.5281/zenodo.21224724)

Replication code for:
> Roberts, H. S., Calastri, C., Batley, R. (2026) "Evaluating open-source approaches for estimating unobserved trip attributes in transport choice modelling". The 58th Universities’ Transport Study Group Annual Conference, 15 July, Guildford, UK.


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

Roberts, H. S., Calastri, C., Batley, R. (2026) "Evaluating open-source approaches for estimating unobserved trip attributes in transport choice modelling". The 58th Universities’ Transport Study Group Annual Conference, 15 July, Guildford, UK.

Roberts, H. (2026) “Replication code for evaluation of attribute estimation methods”. Zenodo. doi:10.5281/zenodo.21224724.


**BibTeX:**

```bibtex
@inproceedings{roberts2026evaluating,
  author       = {Roberts, H. S. and Calastri, C. and Batley, R.},
  title        = {Evaluating Open-Source Approaches for Estimating Unobserved Trip Attributes in Transport Choice Modelling},
  booktitle    = {Proceedings of the 58th Universities' Transport Study Group Annual Conference},
  year         = {2026},
  address      = {Guildford, UK},
  month        = jul,
  day          = {15}
}

@software{roberts_2026_21224724,
  author       = {Roberts, Harry},
  title        = {Replication code for evaluation of attribute estimation methods},
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

