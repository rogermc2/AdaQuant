with Ada.Containers.Ordered_Maps;
with Shell_Basis_Set; use Shell_Basis_Set;

with Types; use Types;

Package Element_Basis_Set is

   Package Element_Energy_Level is new Ada.Containers.Ordered_Maps
             (Key_Type => tEnergy_Level, Element_Type => tShell_Basis_Set);
   Type  tEnergy_Level_Basis_Set is new Element_Energy_Level.Map with null record;

   function Contains_Level (Basis_Set : in tEnergy_Level_Basis_Set;
                            Level     : tEnergy_Level) return Boolean;
   procedure Form_Element_Energy_Level_Map (theElement : in tElement;
                     Shells               : in tString_2;
                     Source_file          : in out tFile_Type;
                     Element_Energy_Level : in out tEnergy_Level_Basis_Set;
                     Error                : out tSource_Data_Error);

end Element_Basis_Set;
