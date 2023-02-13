import numpy as np
import pandas as pd
from ema_workbench import (
  RealParameter, ScalarOutcome, Constant, 
  Model,ema_logging,perform_experiments,
  )
result_file_name = r.tgt_product+'_Beta_1.csv'

result_path = r.path+"Outputs/Results/"+r.tgt_product+"/RSA/MonteCarlo/"+result_file_name
model_CE = r.model_CE_beta_py
path = r.path

if __name__ == "__main__":
  model = Model("modelCE", function=model_CE)
  model.levers = [
    RealParameter("epsilon_max_01", 0.0, 1.0), 
    RealParameter("w_max_01", 0.0, 1.0),
    RealParameter("r_LE_max_01", 0.0, 1.0),
    RealParameter("r_rmn_max_01", 0.0, 1.0), 
    RealParameter("r_rcy_max_01", 0.0, 1.0),
    RealParameter("r_rcy2_max_01", 0.0, 1.0),
    RealParameter("r_drusA_max_01", 0.0, 1.0),
    RealParameter("cr_o_01", 0.0, 1.0), 
    RealParameter("Rir_01", 0.0, 1.0),
    RealParameter("sr_B_01", 0.0, 1.0),
    RealParameter("Rental_period_01", 0.0, 1.0),
    RealParameter("CE_period_01", 0.0, 1.0)
  ]
  
  model.outcomes = [
    ScalarOutcome("TMR_sum"),
    ScalarOutcome("FD_sum"),
    ScalarOutcome("GHG_sum"),
    ScalarOutcome("GHG_MaterialProduction"),
    ScalarOutcome("GHG_Assembly"),
    ScalarOutcome("GHG_Reduc_Reman"),
    ScalarOutcome("GHG_Reduc_Rcy"),
    ScalarOutcome("GHG_Trans1st"),
    ScalarOutcome("GHG_Use_Owned"),
    ScalarOutcome("GHG_Use_Rental"),
    ScalarOutcome("GHG_Disposal"),
    ScalarOutcome("GHG_Reuse"),
    ScalarOutcome("GHG_Reman"),
    ScalarOutcome("GHG_Recycle"),
  ]
  
  model.constants = [
    Constant("cr_r", 1.0),
    Constant("sr_D", 0.5),
  ]
  ema_logging.log_to_stderr(ema_logging.INFO)
  experiments, outputs = perform_experiments(model, policies=10)

df_op = pd.DataFrame(outputs)
df_ex = pd.DataFrame(experiments)
df_op_ex = pd.concat([df_ex,df_op],axis=1)

df_op_ex.to_csv(result_path)

