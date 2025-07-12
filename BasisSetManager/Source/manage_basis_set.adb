with Ada.Text_IO; use Ada.Text_IO;

with Types; use Types;
with Initialize_Basis_Set_Loader;  use Initialize_Basis_Set_Loader;
with basis_set; use basis_set;
with shell_basis_set;
with Basis_Utilities;

procedure Manage_Basis_Set is
   Basis_Set_Symbol : tBasis_Set_Symbol;
   Descriptor_File  : tFile_Type;
   Basis_Set        : tBasis_Set_Map;
   Basis            : Shell_Basis_Set.tPGBF_Contraction_Set;
   Result           : tSource_Data_Error;
begin
   Put_line ("Get basis set test.");
   Initialize ( Descriptor_File, Basis_Set_Symbol, Result);
   If Result = No_Error then
      -- The basis set descriptor_file is now open.
      Form_Basis_Set_Map (Descriptor_File, Basis_Set, Result);
      If Result /= No_Error then
         Put_line ("Form_Basis_Set_Map returned: "
                   & tSource_Data_Error'Image (Result));
      end if;

      Put_line ("Basis Set symbol: " & tBasis_Set_Symbol'image(Basis_Set_Symbol));
      Basis_Utilities.Get_Basis(Basis_Set, C, 1, S, Basis, Result);
      Put_line ("Get basis set returned: " & tSource_Data_Error'Image(Result));
   else
      Put ("Manage_Base_Set: Initialize returned: "
           & tSource_Data_Error'Image (Result));
   end if;

   Put_Line ("Manage_Base_Set is terminating.");

end Manage_Basis_Set;
