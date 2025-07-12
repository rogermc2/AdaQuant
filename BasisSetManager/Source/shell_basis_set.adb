With Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Long_Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
use Ada.Numerics;
with Ada.Exceptions; use Ada.Exceptions;

Package body Shell_Basis_Set is
   Package Elementary_Float_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (long_float);

   function Shell_Contains (Shell_Basis_Set : in tShell_Basis_Set;
                            Shell : in tElectron_Shell) return Boolean is
   begin
      return Shell_Basis_Set.Contains (Shell);
   end Shell_Contains;

   procedure Form_Shell_Basis_Sets (Shell_Symbols   : in tBasis_Shell_Code;
                                    Source_file     : in out tFile_Type;
                                    Shell_Basis_Set : out tShell_Basis_Set;
                                    Error : out tSource_Data_Error) is
   use Elementary_Float_Functions; -- provides ** for float exponents.
      Contraction_Set1 : tPGBF_Contraction_Set;
      Contraction_Set2 : tPGBF_Contraction_Set;
      Shell_1          : tElectron_Shell;
      Shell_2          : tElectron_Shell;
      Num_Shells       : Integer range 1..2;
      aLine            : Unbounded_String;
      Primitive_Item1  : tPGBF;
      Primitive_Item2  : tPGBF;
      Contraction_Set_InComplete  : Boolean := True;
      Line_No          : Positive_Count;
      Last_Digit       : Positive;
   begin
      Error := No_Error;
      case Shell_Symbols is
         when s  => Num_Shells := 1;
                    Shell_1 := s;
         when d  => Num_Shells := 1;
                    Shell_1 := d;
         when sp => Num_Shells := 2;
                    Shell_1 := s;
                    Shell_2 := p;
      end case;

      --  for each contraction item, get the Exponent and Coefficient
      --  for each shell;
      while Contraction_Set_InComplete  And not End_Of_File (Source_file) loop
         aLine := To_Unbounded_String (Get_Line (Source_file));
         -- Put_Line(To_String(aLine));
         Contraction_Set_InComplete := To_String(aLine)(1) = ' ';
         If Contraction_Set_InComplete then
            Ada.Long_Float_Text_IO.Get (Slice (aLine, 6, 16),
                                   Primitive_Item1.exponent, Last_Digit);
            Ada.Long_Float_Text_IO.Get (Slice (aLine, 29, 40),
                                   Primitive_Item1.coeff, Last_Digit);
            Contraction_Set1.Append (Primitive_Item1);
            If Num_Shells = 2 then
               Primitive_Item2.exponent := Primitive_Item1.exponent;
               Ada.Long_Float_Text_IO.Get (Slice (aLine, 52, 64), Primitive_Item2.coeff,
                                      Last_Digit);
               Contraction_Set2.Append (Primitive_Item2);
            end if;

         elsif not End_Of_File (Source_file) then
            Line_No := Line (Source_file) - 1;
            Reset (Source_file);
            Set_Line (Source_file, Line_No);
         end if;
      end loop;

      Shell_Basis_Set.include(Shell_1,Contraction_Set1);
      If Num_Shells = 2 then
         Shell_Basis_Set.include(Shell_2,Contraction_Set2);
      end if;
exception
      when anError : Constraint_Error =>
         Put("Shell_Basis_Set.Form_Shell_Basis_Sets returned constraint error: ");
         Put_Line(Exception_Information(anError));
         Error := eConstraint_Error;
      when anError : End_Error =>
         Put_Line("Shell_Basis_Set.Form_Shell_Basis_Sets returned end of file error: ");
         Put_Line(Exception_Information(anError));
         Error := eEnd_Error;
      when anError :  others =>
         Put_Line("An exceptiom occurred in Shell_Basis_Set.Form_Shell_Basis_Sets.");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Form_Shell_Basis_Sets;

end Shell_Basis_Set;
