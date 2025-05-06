# Markov Chain Monte Carlo (MCMC) Analysis

This repository contains Jupyter Notebooks and scripts for performing Markov Chain Monte Carlo (MCMC) analysis on ECQ data. The analysis includes training and evaluation of probabilistic models using PyMC and ArviZ libraries.

## Project Structure

- **Notebooks**:
  - `MCMC_ECQ_TREINAMENTO.ipynb`: Notebook for training MCMC models on ECQ data.
  - `MCMC_ECQ_ANALISE.ipynb`: Notebook for analyzing the results of MCMC models.
- **Scripts**:
  - `MCMC_ECQ_SEM_ANF.py`: Python script for ECQ analysis without ANF grouping.
  - `MCMC_ECQ.py`: Main script for running MCMC models.
- **Data**:
  - `DADOS BRUTOS/`: Raw data files in Excel format.
  - `modelos/`: Directory containing model traces in NetCDF format.
  - `Output/`: Directory for storing analysis results.
- **References**:
  - `Referencias/`: Directory containing reference documents and papers.

## Requirements

- Python 3.8+
- Required libraries:
  - pandas
  - pymc
  - arviz
  - tqdm
  - matplotlib
  - numpy

## Setup

1. Clone the repository:
   ```bash
   git clone <repository-url>
   ```
2. Install the required libraries:
   ```bash
   pip install -r requirements.txt
   ```
3. Place the raw data files in the `DADOS BRUTOS/` directory.

## Usage

### Training the Model

1. Open the `MCMC_ECQ_TREINAMENTO.ipynb` notebook.
2. Run the cells sequentially to train the MCMC model.
3. Save the model trace in the `modelos/` directory.

### Analyzing the Results

1. Open the `MCMC_ECQ_ANALISE.ipynb` notebook.
2. Load the saved model trace from the `modelos/` directory.
3. Perform analysis and export the results to the `Output/` directory.

## Outputs

- **Prioritization Results**:
  - `priorizacao_sites_ECQ_final.xlsx`: Final prioritization of sites based on ECQ improvement potential.
- **Consolidated Data**:
  - `consolidado_83.xlsx`: Consolidated data for ANF 83.

## References

- PyMC Documentation: https://www.pymc.io/projects/docs/en/stable/
- ArviZ Documentation: https://arviz-devs.github.io/arviz/

## License

This project is licensed under the MIT License.