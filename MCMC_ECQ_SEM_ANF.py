import pandas as pd
import pymc as pm
import arviz as az
from tqdm.auto import tqdm  # Substitui o tqdm padrão

# Carregar dados
data = pd.read_excel('ECQ_FEV25.xlsx', sheet_name='Export')
data = data[data['ANF'] == 83]
data = data[data['MUNICIPIO'] == 'CAMPINA GRANDE']

# Agrupar por Município
grouped = data.groupby(['MUNICIPIO'])

with pm.Model() as modelo_simplificado:
    # Hiperpriors globais
    mu_global = pm.Beta("mu_global", alpha=2, beta=2)  # Média global
    phi_global = pm.HalfNormal("phi_global", sigma=10)  # Variabilidade entre municípios
    
    # Variabilidade entre sites
    phi_site = pm.HalfNormal("phi_site", sigma=10)
    
    # Loop por municípios (apenas 1 neste caso)
    for municipio, group in grouped:
        n_tests = group['TESTES_ECQ'].values
        n_success = group['TESTES_ECQ_OK'].values
        
        # Média do município
        mu_municipio = pm.Beta(
            f"mu_municipio_{municipio}", 
            alpha=mu_global * phi_global,
            beta=(1 - mu_global) * phi_global
        )
        
        # Taxas de sucesso dos sites
        theta_site = pm.Beta(
            f"theta_site_{municipio}",
            alpha=mu_municipio * phi_site,
            beta=(1 - mu_municipio) * phi_site,
            shape=len(n_tests)
        )
        
        # Likelihood
        pm.Binomial(
            f"obs_{municipio}",
            n=n_tests,
            p=theta_site,
            observed=n_success)
    
    # Amostragem ajustada
    trace = pm.sample(
        draws=500,
        tune=200,
        chains=2,
        target_accept=0.9,
        progressbar=True,
        step_kwargs={"nuts": {
            "progressbar_kwargs": {
                "format": "{l_bar}{bar}| {n_fmt}/{total_fmt} [{elapsed}<{remaining}]",
                "ascii": True,  # Usa caracteres simples
            }
        }},
        cores=2  # Reduza se necessário
    )


# Salvar resultados
idata = az.from_pymc(trace)
idata.to_netcdf('trace_ECQ_simplificado.nc')

az.summary(trace)  # Verifique R-hat (~1.0) e ESS (>100)
az.plot_trace(trace)