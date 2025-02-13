# Modulos
import pandas as pd
import numpy as np
import statsmodels.formula.api as smf
import os

# Functions
    # ENOE treatment
def ENOE(DB):
    selection = ["eda",       # Age
                 "sex",       # Sex
                 "t_loc",     # Loc size
                 "ent",       # State
                 "anios_esc", # School years
                 "cs_p13_1",  # School levels
                 "ing_x_hrs", # Income per hour worked
                 "hrsocup",   # Hrs worked per week
                 "emp_ppal"]  # Empleo formal o informal 
    
    DB = DB[selection]
    
    # Convert to a int64 data type format
    DB = DB.replace(" ", 0)
    DB = DB.dropna(how = "any")
    DB = DB.astype("int64")
    
    # Persons aged 15 or over and excluding codes 98 (Age not specified for those over 12 years of age and over) 99 (Age not specified for children under 0 to 11 years of age)
    DB = DB[(DB["eda"] >= 15) & (DB["eda"] != 98) & (DB["eda"] != 99)]

    # Personas con más de cero horas trabajadas 
    DB = DB[DB["hrsocup"] > 0]
    
    # Personas con más de cero horas trabajadas 
    DB = DB[DB["ing_x_hrs"] > 0]
    
    # Persons with specified years of schooling
    DB = DB[DB["anios_esc"] != 99]
    
    # People with specified educational levels
    DB = DB[DB["cs_p13_1"] != 99]
    
    # Levels assignation 

    # Sex
    niveles = [DB["sex"] == 1,
               DB["sex"] == 2]
    
    DB["sex"] = np.select(niveles, ["Hombre",
                                    "Mujer"])
    
    DB["sex"] = DB["sex"].astype("category")
    
    # Empleo formal o informal
    niveles = [DB["emp_ppal"] == 1,
               DB["emp_ppal"] == 2]
    
    DB["emp_ppal"] = np.select(niveles, ["informal",
                                         "formal"])
    
    DB["emp_ppal"] = pd.Categorical(DB["emp_ppal"], categories=["sin empleo",
                                                                "informal",
                                                                "formal"])
    
    # Tamaño de la localidad
    niveles = [DB["t_loc"] == 1,
               DB["t_loc"] == 2,
               DB["t_loc"] == 3,
               DB["t_loc"] == 4]
    
    DB["t_loc"] = np.select(niveles, ["Urbano",
                                      "Urbano",
                                      "Urbano",
                                      "Rural"])
    
    DB["t_loc"] = pd.Categorical(DB["t_loc"], categories=["Rural",
                                                          "Urbano"])
    
    # Educational 
    niveles = [DB["cs_p13_1"] == 0,
               DB["cs_p13_1"] == 1,
               DB["cs_p13_1"] == 2,
               DB["cs_p13_1"] == 3,
               DB["cs_p13_1"] == 4,
               DB["cs_p13_1"] == 5,
               DB["cs_p13_1"] == 6,
               DB["cs_p13_1"] == 7,
               DB["cs_p13_1"] == 8,
               DB["cs_p13_1"] == 9]    
    
    DB["cs_p13_1"] = np.select(niveles,["Ninguna",
                                        "Preescolar",
                                        "Primaria",
                                        "Secundaria",
                                        "Preparatoria o bachillerato",
                                        "Normal",
                                        "Carrera técnica",
                                        "Profesional",
                                        "Maestría",
                                        "Doctorado"])
    
    # Educational levels primary, secondary, high school or baccalaureate, professional, master's, doctorate
    DB = DB[DB["cs_p13_1"].isin(["Ninguna",
                                 "Primaria",
                                 "Secundaria",
                                 "Preparatoria o bachillerato",
                                 "Profesional",
                                 "Maestría",
                                 "Doctorado"])]
    
    DB["cs_p13_1"] = pd.Categorical(DB["cs_p13_1"], categories = ["Ninguna",
                                                                  "Primaria",
                                                                  "Secundaria",
                                                                  "Preparatoria o bachillerato",
                                                                  "Profesional",
                                                                  "Maestría",
                                                                  "Doctorado"], ordered = True)
    
    # Federal entities of Mexico
    entidades = [DB["ent"] == 1,
                 DB["ent"] == 2,
                 DB["ent"] == 3,
                 DB["ent"] == 4,
                 DB["ent"] == 5,
                 DB["ent"] == 6,
                 DB["ent"] == 7,
                 DB["ent"] == 8,
                 DB["ent"] == 9,
                 DB["ent"] == 10,
                 DB["ent"] == 11,
                 DB["ent"] == 12,
                 DB["ent"] == 13,
                 DB["ent"] == 14,
                 DB["ent"] == 15,
                 DB["ent"] == 16,
                 DB["ent"] == 17,
                 DB["ent"] == 18,
                 DB["ent"] == 19,
                 DB["ent"] == 20,
                 DB["ent"] == 21,
                 DB["ent"] == 22,
                 DB["ent"] == 23,
                 DB["ent"] == 24,
                 DB["ent"] == 25,
                 DB["ent"] == 26,
                 DB["ent"] == 27,
                 DB["ent"] == 28,
                 DB["ent"] == 29,
                 DB["ent"] == 30,
                 DB["ent"] == 31,
                 DB["ent"] == 32]

    DB["ent"] = np.select(entidades,["Aguascalientes",
                                     "Baja California",
                                     "Baja California Sur",
                                     "Campeche",
                                     "Coahuila de Zaragoza",
                                     "Colima",
                                     "Chiapas",
                                     "Chihuahua",
                                     "Ciudad de México",
                                     "Durango",
                                     "Guanajuato",
                                     "Guerrero",
                                     "Hidalgo",
                                     "Jalisco",
                                     "México",
                                     "Michoacán de Ocampo",
                                     "Morelos",
                                     "Nayarit",
                                     "Nuevo León",
                                     "Oaxaca",
                                     "Puebla",
                                     "Querétaro",
                                     "Quintana Roo",
                                     "San Luis Potosí",
                                     "Sinaloa",
                                     "Sonora",
                                     "Tabasco",
                                     "Tamaulipas",
                                     "Tlaxcala",
                                     "Veracruz de Ignacio de la Llave",
                                     "Yucatán",
                                     "Zacatecas"])
    
    DB["ent"] = DB["ent"].astype("category")
        
    # Experience
    DB["exp"] = DB["eda"] - DB["anios_esc"]
    
    return DB

    # Rename some columns in old data frames
def db_rename(DB):
    try:
        # Renombramos la columna
        DB.rename(columns={'t_loc_tri':'t_loc'}, inplace=True)
        return DB
    except KeyError:
        pass
    
    # Convertir a minúsculas
def convertir_a_minusculas(DB):
    DB.columns = [col.lower() for col in DB.columns]
    return DB

# Database, table corresponding to sociodemographic variables
path = r"C:\Users\parra\Documents\Tesis\Bases\Base_data"
ENOE_files = os.listdir(path)
ENOE_list = [pd.read_csv(os.path.join(path, file), encoding="unicode_escape", low_memory=False) for file in ENOE_files]

# aplicar la fórmula para obtener los datos del modelo
ENOE_modelo = [ENOE(db_rename(convertir_a_minusculas(data_frame))) for data_frame in ENOE_list]

# INPC to have the income in real terms
path = r"C:\Users\parra\Documents\Tesis\Bases"
INPC = pd.read_csv(os.path.join(path,"INPC.csv"), encoding='unicode_escape')
INPC["Fecha"] = pd.to_datetime(INPC["Fecha"])

# Agregar la columna 'year' y "quarter"
INPC["year"] = INPC["Fecha"].dt.year
INPC["quarter"] = INPC["Fecha"].dt.quarter

# Agrupar por año y trimestre
INPC_agrupado = INPC.groupby(["year", "quarter"])["INPC"].agg("mean")

# Filtramos el índice por año y trimestre
INPC_agrupado = INPC_agrupado[((INPC_agrupado.index.get_level_values(0) != 2020) | (INPC_agrupado.index.get_level_values(1) != 2)) &
                              ((INPC_agrupado.index.get_level_values(0) != 2024) | (INPC_agrupado.index.get_level_values(1) != 1))]

# Agregar el valor del INPC en cada data frame
for i in range(len(ENOE_modelo)):
    DB = ENOE_modelo[i]
    DB["INPC"] = INPC_agrupado.iloc[i]

# Calcular el ingreso en términos reales
for i in ENOE_modelo:
    i['ing_x_hrs'] = (i["ing_x_hrs"]/i["INPC"])*100
del i

# Calcular la ecuación de ingresos de Mincer para cada trimestre

# Lista para almacenar los modelos
formula_years = 'np.log(ing_x_hrs) ~ anios_esc + exp + np.square(exp) + sex + t_loc + emp_ppal'
formula_levels = 'np.log(ing_x_hrs) ~ cs_p13_1 + exp + np.square(exp) + sex + t_loc + emp_ppal'
lista_modelos_years = []
lista_modelos_levels = []

# Iterar para cada elemento de la lista ENOE_modelos
for i, df in enumerate(ENOE_modelo):
    # Ajustar el modelo para cada data frame
    modelo_years = smf.ols(formula_years, data=df).fit()
    modelo_leves = smf.ols(formula_levels, data=df).fit()
    
    # Guardar el modelo en la lista
    lista_modelos_years.append(modelo_years)
    lista_modelos_levels.append(modelo_leves)
    
# Obtener los parámetros de cada modelo y almacenarlos en una lista
params_list_years = [modelo.params for modelo in lista_modelos_years]
r2_list_years = [modelo.rsquared for modelo in lista_modelos_years]
bse_list_years = [modelo.bse for modelo in lista_modelos_years]

params_list_levels = [modelo.params for modelo in lista_modelos_levels]
r2_list_levels = [modelo.rsquared for modelo in lista_modelos_levels]
bse_list_levels = [modelo.bse for modelo in lista_modelos_levels]

# Crear un DataFrame a partir de la lista de parámetros y errores estandar
INPC_dates = INPC_agrupado.to_frame()
INPC_dates = INPC_dates.reset_index()
INPC_dates["Fecha"] = INPC_dates["year"].astype(str) + "Q" + INPC_dates["quarter"].astype(str)
INPC_dates["Fecha"] = pd.to_datetime(INPC_dates["Fecha"])

df_params_years = pd.DataFrame(params_list_years)
df_params_years["R2"] = r2_list_years
df_params_years["Fecha"] = INPC_dates["Fecha"]

df_bse_years = pd.DataFrame(bse_list_years)
df_bse_years["R2"] = r2_list_years
df_bse_years["Fecha"] = INPC_dates["Fecha"]

df_params_years = pd.merge(df_params_years, df_bse_years, on=["Fecha", "R2"], suffixes=('_params', '_bse'))


df_params_levels = pd.DataFrame(params_list_levels)
df_params_levels["R2"] = r2_list_levels
df_params_levels["Fecha"] = INPC_dates["Fecha"]

df_bse_levels = pd.DataFrame(bse_list_levels)
df_bse_levels["R2"] = r2_list_levels
df_bse_levels["Fecha"] = INPC_dates["Fecha"]

df_params_levels = pd.merge(df_params_levels, df_bse_levels, on=["Fecha", "R2"], suffixes=('_params', '_bse'))


# Guardar los datos
df_params_years.to_csv(r'C:\Users\parra\Documents\Tesis\Bases\resultados\df_params_years.csv', index=False)
df_params_levels.to_csv(r'C:\Users\parra\Documents\Tesis\Bases\resultados\df_params_levels.csv', index=False)

# Hacer los mismos modelos pero para cada estado
estados_names = ["Aguascalientes",
                 "Baja California",
                 "Baja California Sur",
                 "Campeche",
                 "Coahuila de Zaragoza",
                 "Colima",
                 "Chiapas",
                 "Chihuahua",
                 "Ciudad de México",
                 "Durango",
                 "Guanajuato",
                 "Guerrero",
                 "Hidalgo",
                 "Jalisco",
                 "México",
                 "Michoacán de Ocampo",
                 "Morelos",
                 "Nayarit",
                 "Nuevo León",
                 "Oaxaca",
                 "Puebla",
                 "Querétaro",
                 "Quintana Roo",
                 "San Luis Potosí",
                 "Sinaloa",
                 "Sonora",
                 "Tabasco",
                 "Tamaulipas",
                 "Tlaxcala",
                 "Veracruz de Ignacio de la Llave",
                 "Yucatán",
                 "Zacatecas"]

        # Lits of states data frames                       
def bases_estados(ent):
    df = [i[i["ent"] == ent ] for i in ENOE_modelo]
    return df

data_estados = [bases_estados(i) for i in estados_names]

        # Models for each state
        # Years
def models_estados(num):
    l = []
    for i in data_estados[num]:
        if not i.empty:  # Verifica si el data frame no está vacío
            model = smf.ols('np.log(ing_x_hrs) ~ anios_esc + exp + np.square(exp) + sex + t_loc + emp_ppal', data=i).fit()
            l.append(model)
    return l

models_estados_years = [models_estados(i) for i in range(0,32)]
        
        # Levels
def models_estados_2(num):
    l = []
    for i in data_estados[num]:
        if not i.empty:  # Verifica si el data frame no está vacío
            model = smf.ols('np.log(ing_x_hrs) ~ cs_p13_1 + exp + np.square(exp) + sex + t_loc + emp_ppal', data=i).fit()
            l.append(model)
    return l
    
models_estados_levels = [models_estados_2(i) for i in range(0,32)]

# Guardar los resultados
# Ruta de la carpeta donde se guardarán los archivos CSV
output_folder_levels = r'C:\Users\parra\Documents\Tesis\Bases\resultados'
output_folder_years = r'C:\Users\parra\Documents\Tesis\Bases\resultados'


# Crear listas de DataFrames para cada estado y año
df_params_states_years_list = []
df_params_states_levels_list = []

for i in range(32):
    # Obtener los parámetros y R2 de cada modelo y almacenarlos en listas para años
    params_list_state_years = [modelo.params for modelo in models_estados_years[i]]
    r2_list_state_years = [modelo.rsquared for modelo in models_estados_years[i]]

    # Crear un DataFrame a partir de las listas de parámetros para años
    df_params_state_years = pd.DataFrame(params_list_state_years)
    df_params_state_years["R2"] = r2_list_state_years
    df_params_state_years["Fecha"] = INPC_dates["Fecha"]  # Asegúrate de ajustar la columna "Fecha" según tus datos

    # Guardar el DataFrame para años como archivo CSV
    output_filename_years = os.path.join(output_folder_years, f'df_params_states_years_{estados_names[i]}.csv')
    df_params_state_years.to_csv(output_filename_years, index=False)

    # Obtener los parámetros y R2 de cada modelo y almacenarlos en listas para niveles
    params_list_state_levels = [modelo.params for modelo in models_estados_levels[i]]
    r2_list_state_levels = [modelo.rsquared for modelo in models_estados_levels[i]]

    # Crear un DataFrame a partir de las listas de parámetros para niveles
    df_params_state_levels = pd.DataFrame(params_list_state_levels)
    df_params_state_levels["R2"] = r2_list_state_levels
    df_params_state_levels["Fecha"] = INPC_dates["Fecha"]  # Asegúrate de ajustar la columna "Fecha" según tus datos

    # Guardar el DataFrame para niveles como archivo CSV
    output_filename_levels = os.path.join(output_folder_levels, f'df_params_states_levels_{estados_names[i]}.csv')
    df_params_state_levels.to_csv(output_filename_levels, index=False)

    # Agregar los DataFrames a las listas
    df_params_states_years_list.append(df_params_state_years)
    df_params_states_levels_list.append(df_params_state_levels)

