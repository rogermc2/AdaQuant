with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded;
With Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use  Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Element_Shell_Symbol_Maps;
use Element_Shell_Symbol_Maps;

Package body Element_Basis_Set is
   function Contains_Level (Basis_Set : in tEnergy_Level_Basis_Set;
                            Level : tEnergy_Level) return Boolean is
   begin
      return Basis_Set.Contains (Level);
   end Contains_Level;

   procedure Form_Element_Energy_Level_Map (theElement : in tElement;
                           Shells      : in tString_2;
                           Source_file :  in out tFile_Type;
                           Element_Energy_Level : in out tEnergy_Level_Basis_Set;
                           Error : out tSource_Data_Error) is
      aLine               : Unbounded_String;
      Line_Num            : Positive_Count;
      element_and_shells  : String (1 .. 7);
      element_string      : String(1..2);
      string_Length       : Integer;
      Shell_Symbols       : tBasis_Shell_Code;
      its_Shells          : tString_2 := To_Upper(Shells);
      Energy_Level        : tEnergy_Level := 1;
      Shell_Basis_Set     : tShell_Basis_Set;
      Continue            : Boolean := True;
   begin
      If Shell_Symbol_Map_Contains (its_Shells) then
         Shell_Symbols := Shells_Code (its_Shells);
         Form_Shell_Basis_Sets (Shell_Symbols, Source_file,
                                Shell_Basis_Set, Error);
         If Error = No_Error then
            Element_Energy_Level.include (Energy_Level, Shell_Basis_Set);
         else
            Put_line ("Form_Shell_Basis_Sets returned Error: "
                      & tSource_Data_Error'Image (Error));
        end if;
      else
         Error := eShell_Symbol_Error;
         Put_line ("Form_Element_Energy_Level_Map, Shell_Symbol_Map_doesn't contain: "
                      & Shells);
      end if;

      If Error = no_error then
	 -- Look for another set for the same element
         Energy_Level := Energy_Level + 1;

         while Continue And not End_Of_File(Source_file) loop
            while Continue And not End_Of_File (Source_file) loop
              -- Skip any comment lines
               aLine := Text_IO.Get_Line (Source_file);
               element_string := To_Upper(To_String (aLine)(1..2));
               Continue := not Element_Symbol_Map_Contains(element_string);
            end loop;
            Continue := Element_Symbol (element_string) = theElement;
            If not Continue And not End_Of_File (Source_file) then
               -- this is a different element
               Line_Num := Line (Source_file) - 1;
               Reset(Source_file);
               Set_Line(Source_file, Line_Num);
            elsif not End_Of_File (Source_file)  then
               string_Length := Length (aLine);
               If string_Length = 6 then
                  element_and_shells := To_String (aLine)(1..6) & One_Blank;
               elsif string_Length > 6 then
                  element_and_shells := To_String (aLine)(1..7);
               end if;
               its_Shells := To_Upper(element_and_shells(6..7));
               Continue := Error = no_error And
                           Shell_Symbol_Map_Contains (its_Shells);
                If Continue then
                   Shell_Symbols := Shells_Code (its_Shells);
                   Form_Shell_Basis_Sets (Shell_Symbols, Source_file,
                                          Shell_Basis_Set, Error);
                  If Error = No_Error then
                      Element_Energy_Level.include
                          (Energy_Level, Shell_Basis_Set);
                  end if;
                end if;
            end if;
         end loop;
      end if;

exception
      when anError : Constraint_Error =>
         Put("Element_Basis_Set.Form_Element_Energy_Level_Map returned ");
         Put_Line(Exception_Information(anError));
         Error := eConstraint_Error;
      when anError : End_Error =>
         Put_Line("Element_Basis_Set.Form_Element_Energy_Level_Map returned ");
         Put_Line(Exception_Information(anError));
         Error := eEnd_Error;
      when anError :  others =>
         Put_Line("An exceptiom occurred in Element_Basis_Set.Form_Element_Energy_Level_Map.");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Form_Element_Energy_Level_Map;

end Element_Basis_Set;
