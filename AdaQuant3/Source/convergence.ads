
with Adaquant_Types; use Adaquant_Types;
with Matrix_Types; use Matrix_Types;

Package Convergence is
   Procedure Average_Charge_Density
                          (Averaging_Type          : in tAverager;
                           Previous_Density_Matrix : in tCharge_Density_Matrix;
                           Density_Matrix          : in out tCharge_Density_Matrix;
                           Convergence             : out Long_Float);
end Convergence;
