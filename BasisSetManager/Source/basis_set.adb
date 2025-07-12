With Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO; use  Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions; use  Ada.Exceptions;

with Element_Shell_Symbol_Maps; use Element_Shell_Symbol_Maps;
with Shell_Basis_Set; use Shell_Basis_Set;
with Initialize_Basis_Set_Loader; use Initialize_Basis_Set_Loader;

Package body Basis_Set is

   function Basis_Set_Contains (Basis_Set : in tBasis_Set_Map;
                                Element   : tElement) return Boolean is
   begin
      return Basis_Set.Contains (Element);
   end Basis_Set_Contains;

 -- --------------------------------------------------------------------------

   procedure Form_Basis_Set_Map (Source_file : in out tFile_Type;
                                 Basis_Set   : out tBasis_Set_Map;
                                 Error       : out tSource_Data_Error) is
      aLine                  : Unbounded_String;
      element_and_shells     : String(1..7);
      this_Element           : tElement;
      Shells                 : tString_2;
      string_Length          : Integer;
      Element_Energy_Level   : tEnergy_Level_Basis_Set;
      Continue               : Boolean := True;
   begin
      Error := No_Error;

   while Continue And not End_Of_File(Source_file) loop
      while Continue And not End_Of_File(Source_file) loop
         -- Skip lines until element
         aLine := Text_IO.Get_Line (Source_file);
         string_Length := Length (aLine);
         If string_Length = 6 then
            element_and_shells := To_Upper(To_String(aLine)(1 .. 6)) & One_Blank;
         elsif string_Length > 6 then
            element_and_shells := To_Upper(To_String(aLine)(1 .. 7));
         end if;
         Continue := not Element_Symbol_Map_Contains (element_and_shells (1 .. 2));
      end loop;
      -- Element and shells characters found
      Continue := not End_Of_File (Source_file);
      If Continue then
         this_element := Element_Symbol (element_and_shells(1 .. 2));
         Shells := element_and_shells(5..6 );
--     Put_Line ("Form_Basis_Set_Map, element: " & tElement'image(this_element));
--     Put_Line ("Form_Basis_Set_Map, shells: " & Shells);
         Form_Element_Energy_Level_Map (this_Element, Shells, Source_file,
                                        Element_Energy_Level, Error) ;
         Continue :=  Error = no_Error;

         If Continue then
            Basis_Set.include (this_Element, Element_Energy_Level);
         else
               Put_Line ("Form_Basis_Set_Map, Form_Element_Energy_Level_Map " &
                         "returned Error : " & tSource_Data_Error'image(Error));
         end if;
      end if;
    end loop;

   exception
      when anError : Constraint_Error =>
         Put("Basis_Set.Form_Basis_Set_Map returned ");
         Put_Line(Exception_Information(anError));
         Error := eConstraint_Error;
      when anError : End_Error =>
         Put_Line("Basis_Set.Form_Basis_Set_Map returned ");
         Put_Line(Exception_Information(anError));
         Error := eEnd_Error;
      when anError :  others =>
         Put_Line("An exceptiom occurred in Basis_Set.Form_Basis_Set_Map.");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Form_Basis_Set_Map;

  -- --------------------------------------------------------------------------

   procedure Get_basis_Set (Basis_Set_Symbol : out tBasis_Set_Symbol;
                            Basis_Set        : out tBasis_Set_Map;
                            Error            : out tSource_Data_Error) is
   use Ada.Strings.Unbounded.Text_IO;
     Basis_Descriptor_File : tFile_Type;
   begin
      Initialize (Basis_Descriptor_File, Basis_Set_Symbol, Error);
      If Error = No_Error then
         -- The basis set descriptor file is now open,
         Form_Basis_Set_Map (Basis_Descriptor_File, Basis_Set, Error);
         Close (Basis_Descriptor_File);
      else
         Put_Line ("Manage_Base_Set: Initialize returned an error.");
      end if;
   exception
      when anError : others =>
         Put("Basis_Set.Get_Basis_Set returned ");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Get_basis_Set;

   -- --------------------------------------------------------------------------

   procedure Get_basis_Set (Basis_Set_Name   : in tBasis_Name;
                            Basis_Set_Symbol : out tBasis_Set_Symbol;
                            Basis_Set        : out tBasis_Set_Map;
                            Error            : out tSource_Data_Error) is
   use Ada.Strings.Unbounded.Text_IO;
     Source_file           : tFile_Type;
     Basis_Descriptor_File : tFile_Type;
   begin
      Initialize (Basis_Set_Name, Basis_Descriptor_File, Basis_Set_Symbol, Error);
      If Error = No_Error then
         -- The basis set descriptor file is now open,
         Form_Basis_Set_Map (Basis_Descriptor_File, Basis_Set, Error);
         Close (Basis_Descriptor_File);
      else
         Put_Line ("Manage_Base_Set: Initialize returned an error.");
      end if;
   exception
      when anError : others =>
         Put("Basis_Set.Get_Basis_Set returned ");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Get_basis_Set;

end Basis_Set;
