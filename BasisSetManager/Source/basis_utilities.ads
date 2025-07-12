
with Types; use Types;
with Basis_Set; use Basis_Set;
with Element_Basis_Set; use Element_Basis_Set;
with Shell_Basis_Set; use Shell_Basis_Set;

Package Basis_Utilities is

   procedure Get_Basis (aBasis_Set   : in tBasis_Set_Map;
                        Element      : in tElement;
                        Energy_Level : in tEnergy_Level;
                        Shell        : in tElectron_Shell;
                        Basis        : in out tPGBF_Contraction_Set;
                        Error        : out tSource_Data_Error);
end Basis_Utilities;
