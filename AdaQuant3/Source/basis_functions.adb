with Basis_Utilities; use Basis_Utilities;

Package body Basis_Functions is

 procedure Get_Gaussian_Basis_Function (aBasis_Set   : in tBasis_Set_Map;
                        Element        : in tElement;
                        Energy_Level   : in tEnergy_Level;
                        Shell          : in tElectron_Shell;
                        Basis_Function : out tGaussian_Function;
                        Error          : out tSource_Data_Error) is
    Contraction_Set : tPGBF_Contraction_Set;
 begin
    Get_Basis (aBasis_Set, Element, Energy_Level, Shell, Contraction_Set, Error);
    Basis_Function := 0.0;
    For Index in 1 .. Contraction_Set'size loop
       Basis_Function := Basis_Function;
    end loop;
 end Get_Gaussian_Basis_Function;

end Basis_Functions;
