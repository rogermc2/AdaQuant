with Types; use Types;
with Basis_Set; use Basis_Set;
with Element_Basis_Set; use Element_Basis_Set;
with Shell_Basis_Set; use Shell_Basis_Set;

Package Basis_Functions is
   Type tGaussian_Function is private;

 procedure Get_Gaussian_Basis_Function (aBasis_Set : in tBasis_Set_Map;
                        Element        : in tElement;
                        Energy_Level   : in tEnergy_Level;
                        Shell          : in tElectron_Shell;
                        Basis_Function : out tGaussian_Function;
                        Error          : out tSource_Data_Error);
private
   Type tGaussian_Function is new float;
end Basis_Functions;
