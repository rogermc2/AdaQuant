with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Types; use Types;

Package Shell_Basis_Set is
   Type tContraction_ID is new Natural;
   Type tPGBF is record
      Coeff         : tCoefficient;
      Exponent      : tExponent;
      Normalization : tNormalization;
   end record;

   Package Contraction_Vector is new  Ada.Containers.Vectors
     (Index_Type => tContraction_ID, Element_Type => tPGBF);
   Type tPGBF_Contraction_Set is new Contraction_Vector.Vector with null record;

   Package Shell_Basis_Set_Map is new  Ada.Containers.Ordered_Maps
     (Key_Type => tElectron_Shell, Element_Type => tPGBF_Contraction_Set);
   Type  tShell_Basis_Set is new Shell_Basis_Set_Map.Map with null record;

   function Shell_Contains (Shell_Basis_Set : in tShell_Basis_Set;
                            Shell           : tElectron_Shell) return Boolean;
   procedure Form_Shell_Basis_Sets (Shell_Symbols   : in tBasis_Shell_Code;
                                    Source_file     : in out tFile_Type;
                                    Shell_Basis_Set : out tShell_Basis_Set;
                                    Error           : out tSource_Data_Error);

end Shell_Basis_Set;
