with Ada.Containers.Indefinite_Ordered_Maps;

with Types; use Types;
with Element_Basis_Set; use Element_Basis_Set;

Package Basis_Set is
  Package Basis_Set_Map is
     new Ada.Containers.Indefinite_Ordered_Maps
         (Key_Type => tElement, Element_Type => tEnergy_Level_Basis_Set);
  Type tBasis_Set_Map is new Basis_Set_Map.Map with null record;

  procedure Form_Basis_Set_Map (Source_file : in out tFile_Type;
                                Basis_Set   : out tBasis_Set_Map;
                                Error       : out tSource_Data_Error);
  procedure Get_Basis_Set (Basis_Set_Symbol : out tBasis_Set_Symbol;
                           Basis_Set        : out tBasis_Set_Map;
                           Error            : out tSource_Data_Error);
  procedure Get_Basis_Set (Basis_Set_Name   : in tBasis_Name;
                           Basis_Set_Symbol : out tBasis_Set_Symbol;
                           Basis_Set        : out tBasis_Set_Map;
                           Error            : out tSource_Data_Error);
  function Basis_Set_Contains (Basis_Set : in tBasis_Set_Map;
                               Element   : tElement) return Boolean;

end Basis_Set;
