with Types; use Types;

with Adaquant_Types; use Adaquant_Types;
with Matrix_Types; use Matrix_Types;

Package Charge_Density is
   Procedure Form_Charge_Density_Matrix
                         (Initial_Charge_Density : in tCharge_Density;
                          Averaging_Type         : in tAverager;
                          Hamiltonian_Matrix     : in tCore_Hamiltonian_Matrix;
                          Transformation_Matrix  : in tTransformation_Matrix;
                          Charge_Density_Matrix  : out tCharge_Density_Matrix;
                                         Error   : out tSource_Data_Error);
   Procedure Update_Charge_Density_Matrix
                         (Averaging_Type         : in tAverager;
                          Coefficient_Matrix     : in tCoefficient_Matrix;
                          Charge_Density_Matrix  : in out tCharge_Density_Matrix;
                          Convergence            : out long_float);
end Charge_Density;
