import pandas as pd
import pymc as pm
import arviz as az

# Carregar dados
data = pd.read_excel('ECQ_FEV25.xlsx')

data = data[data['ANF'] == 83]
data = data[data['MUNICIPIO'] == 'CAMPINA GRANDE']
data
# Agrupar por ANF e Município (DEFINA AQUI)
grouped = data.groupby(['ANF', 'MUNICIPIO'])

with pm.Model() as modelo_flex_phi_estimado:
    # Hiperpriors para ANFs (Nível 3)
    anfs = data['ANF'].unique()
    n_anfs = len(anfs)
    
    mu_anf = pm.Beta("mu_anf", alpha=2, beta=2, shape=n_anfs)
    sigma_anf = pm.HalfNormal("sigma_anf", sigma=0.1, shape=n_anfs)
    
    # Phi estimado para municípios e sites
    phi_municipio = pm.HalfNormal("phi_municipio", sigma=10)  # Nível 2
    phi_site = pm.HalfNormal("phi_site", sigma=10)            # Nível 1
    
    # Mapeamento ANF -> índice
    anf_id_map = {anf: idx for idx, anf in enumerate(anfs)}
    
    # Loop por cada grupo (ANF + Município)
    theta_sites_list = []
    for (anf, municipio), group in grouped:
        # Dados do município
        n_tests = group['TESTES_ECQ'].values
        n_success = group['TESTES_ECQ_OK'].values
        idx_anf = anf_id_map[anf]
        
        # Nível 2: Município (mu_municipio ~ ANF)
        mu_municipio = pm.Beta(
            f"mu_municipio_{anf}_{municipio}", 
            alpha=mu_anf[idx_anf] * phi_municipio,
            beta=(1 - mu_anf[idx_anf]) * phi_municipio
        )
        
        # Nível 1: Sites (theta_site ~ município)
        theta_site = pm.Beta(
            f"theta_site_{anf}_{municipio}",
            alpha=mu_municipio * phi_site,
            beta=(1 - mu_municipio) * phi_site,
            shape=len(n_tests)
        )
        
        # Likelihood
        pm.Binomial(
            f"obs_{anf}_{municipio}",
            n=n_tests,
            p=theta_site,
            observed=n_success)
    
    # Amostragem
 #   trace = pm.sample(500, tune=100, chains=2, target_accept=0.9, progressbar=True)
    trace = pm.sample(
    draws=500,
    tune=100,
    chains=2,
    target_accept=0.9,
    progressbar=True,
    step_kwargs={"nuts": {"progressbar_kwargs": {"format": "{desc}: {percentage:.0f}%|{bar}| {n_fmt}/{total_fmt} [{elapsed}<{remaining}]"}}},
    cores=8,
)

idata = az.from_pymc(trace)
idata.tonetcdf('trace_ECQ.nc')

# PREVISAO DE CONCLUSAO : 27/03/25

# Para carregar roda
# idata = az.from_netcdf('trace_ECQ.nc')
# trace = idata.pymc.trace
