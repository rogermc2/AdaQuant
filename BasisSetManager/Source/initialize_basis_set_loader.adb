with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use  Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use  Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with Basis_Name_Map; use Basis_Name_Map;

package body Initialize_Basis_Set_Loader is

   procedure Initialize (Descriptor_File  : in out tFile_Type;
                         Basis_Set_Symbol : out tBasis_Set_Symbol;
                         Error            : out tSource_Data_Error) is
     Basis_Set_Name    : tBasis_Name;
     Basis_Directory   : String := "../BasisSetManager/basis";
     Directory_Set     : Boolean := False;
   begin
      Error := No_Error;
      Set_Directory (Basis_Directory);
      Directory_Set := True;
      Put_Line("Basis data set name: ");
      Basis_Set_Name := Text_IO.Get_Line;
      Basis_Set_Symbol := Get_Basis_Set_Symbol(Basis_Set_Name) ;
      If Basis_Set_Symbol = Unknown then
         Error := eUnknown_Basis_Set;
         Put_Line ("We don't know " & To_String (Basis_Set_Name) & "!");
         New_Line;
      else
         Open (Descriptor_File, In_File, To_Upper (To_String (Basis_Set_Name))
               &  ".txt");
      end if;

   exception
      when anError : Ada.IO_Exceptions.Name_Error  =>
         if not Directory_Set then
            Put_Line ("Initialize_Basis_Set_Loader.Initialize: " &
                      "There is no directory " & Basis_Directory);
             Error := eDirectory_Name_Error;
         end if;
         Put_Line(Exception_Information(anError));

      when anError : others =>
         Put_Line ("An exception occurred in " &
                   "Initialize_Basis_Set_Loader.Initialize! ");
        Put_Line(Exception_Information(anError));
        Error := eError;
   end Initialize;

--   -------------------------------------------------------------------------

   procedure Initialize (Basis_Set_Name    : tBasis_Name;
                         Descriptor_File   : in out tFile_Type;
                         Basis_Set_Symbol  : out tBasis_Set_Symbol;
                         Error             : out tSource_Data_Error) is
     Basis_Directory   : String := "/Ada Projects/Basis Set Manager/basis";
     Directory_Set     : Boolean := False;
  begin
      Error := No_Error;
      Set_Directory (Basis_Directory);
      Directory_Set := True;
      Basis_Set_Symbol := Get_Basis_Set_Symbol(Basis_Set_Name);
      If Basis_Set_Symbol = Unknown then
         Error := eUnknown_Basis_Set;
         Put_Line ("We don't know " & To_String (Basis_Set_Name) & "!");
         New_Line;
      else
         Open (Descriptor_File, In_File, To_Upper (To_String (Basis_Set_Name)) &
               ".txt");
      end if;

   exception
      when anError : Ada.IO_Exceptions.Name_Error  =>
          If not Directory_Set then
            Put_Line ("Initialize_Basis_Set_Loader.Initialize: " &
                      "There is no directory " & Basis_Directory);
             Error := eDirectory_Name_Error;
         end if;
         Put_Line(Exception_Information(anError));

      when anError : others =>
        Put_Line ("An exception occurred in " &
                  "Initialize_Basis_Set_Loader.Initialize! ");
        Put_Line(Exception_Information(anError));
        Error := eError;
  end Initialize;

end Initialize_Basis_Set_Loader;
