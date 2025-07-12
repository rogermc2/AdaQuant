
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Fixed;
with Ada.Characters.Handling; use  Ada.Characters.Handling;
with Ada.Directories; use Ada.Directories;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with Basis_Name_Map;
with Element_Shell_Symbol_Maps; use Element_Shell_Symbol_Maps;
with Initialize_Basis_Set_Loader;

with Cartesian; use Cartesian;
with Tools;

package body Initialize_AdaQuant is
   Type tMolecule_Parameter_Symbol is
            (eNAME, eUNITS, eCHARGE, eMULTIPLICITY, eATOMLIST, eENDList);
   Package Molecule_Parameters_Package is new  Ada.Containers.Ordered_Maps
     (Key_Type => tString_13, Element_Type => tMolecule_Parameter_Symbol);
   Molecule_Parameter_Map :  Molecule_Parameters_Package.Map
     := Molecule_Parameters_Package.Empty_Map;

   Package Options_Method_Package is new  Ada.Containers.Ordered_Maps
     (Key_Type => tString_7, Element_Type => tCalculation_Method);
   Options_Method_Map :  Options_Method_Package.Map
     := Options_Method_Package.Empty_Map;

   Procedure Apply_Units (Units : in tUnits; Atoms : in out tAtom_List);
   Package Charge_IO is new Ada.Text_IO.Integer_IO(tCharge);
   Package Multiplicity_IO is new Ada.Text_IO.Integer_IO(tMultiplicity);

   Function Get_SCF_Method return tCalculation_Method;
   procedure Parse_Molecule_Descriptor_File (File_Name : in tFile_Name;
                                             Molecule  : out tMolecule;
                                             Error     : out tSource_Data_Error);
   procedure Process_Descriptor_file (Descriptor_File : in tFile_Type;
                                      Molecule  : out tMolecule;
                                      Error     : out tSource_Data_Error);

   -- --------------------------------------------------------------------------

   Procedure Apply_Units (Units : in tUnits; Atoms : in out tAtom_List) is
      Scale : pfloat;
      Procedure Do_Scaling (Index : Atom_List_Package.Cursor) is
         Item : tAtom := Atom_List_Package.Element(Index);
      begin
         Item.Position.x := Scale * Item.Position.x;
         Item.Position.y := Scale * Item.Position.y;
         Item.Position.z := Scale * Item.Position.z;
         Atom_List_Package.Replace_Element
                           (Atom_List_Package.List (Atoms), Index, Item);
      end Do_Scaling;
   begin
      Case Units is
      when Bohr => Scale := 1.0;
      when Angstrom => Scale := 1.0/0.52918;
      end Case;
      If Units /= Bohr then
         Iterate (Atoms, Do_Scaling'access);
      end if;
      exception
      when anError : others =>
        Put_Line ("An exception occurred in Initialize_Adaquant.Apply_Units!");
        Put_Line(Exception_Information(anError));
   end Apply_Units;

   -- --------------------------------------------------------------------------

   Function Get_SCF_Method return tCalculation_Method is
   use Ada.Strings.Unbounded.Text_IO;
   use Ada.Strings.Fixed;
      aLine        : Unbounded_String;
      Line_Length  : Natural;
      Options_Text : String := Seven_Blanks;
      Process      : Boolean := False;
      Max_Count    : constant Positive := 4;
      Count        : Positive range 1 .. Max_Count := 1;
      Method       : tCalculation_Method;
   begin
      while Process = False and Count < Max_Count loop
        Put_Line ("SCF Method (Undefined, RHF, UHF, DFT, MINDO3, UMINDO3): ");
        aLine := Text_IO.Get_Line;
        Line_Length := Length (aLine);
        If Line_Length < 7 then
           Replace_Slice(Options_Text, 1, Line_Length,
                         To_Upper (To_String (aLine)));
        else
           Replace_Slice(Options_Text, 1, 7,
                         To_Upper (To_String (aLine)));
        end if;
        Process :=
           Options_Method_Map.Contains (Options_Text);
        If Process then
           Method := Options_Method_Map.Element (Options_Text);
        else
            Put_Line ("Invalid method: " & To_String(aLine));
            Options_Text := Seven_Blanks;
            Count := Count + 1;
        end if;
      end loop;

      If not Process then
        Method := RHF;
        Put_Line ("Maximum attempts exceeded, so setting method to RHF.");
      end if;
      return Method;
   exception
      when anError : others =>
        Put_Line ("An exception occurred in Initialize_Adaquant.Get_SCF_Method!");
        Put_Line(Exception_Information(anError));
      return Undefined;
   end Get_SCF_Method;

-- --------------------------------------------------------------------------

   procedure Initialize (Molecule         : out tMolecule;
                         Options          : out tOptions;
                         Basis_Set_Symbol : out tBasis_Set_Symbol;
                         CGBF_Orbitals    : in out tCGBF_Orbital_Set;
                         File_Name        : out tFile_Name;
                         Error            : out tSource_Data_Error) is
      use Ada.Strings.Unbounded.Text_IO;
      use Ada.Strings.Fixed;
      use Ada.Containers;
      aLine             : Unbounded_String;
      Basis_Set         : tBasis_Set_Map;
      Process           : Boolean := False;
      Max_Count         : constant Positive := 4;
      Count             : Positive range 1..Max_Count := 1;
   begin
      Error := No_Error;
      Options.Calculation_Method := Get_SCF_Method;

      Put_Line("Molecule description file name: ");
      File_Name := Text_IO.Get_Line;
      Parse_Molecule_Descriptor_File (File_Name, Molecule, Error);

      If Error = no_Error then
         Get_basis_Set (Basis_Set_Symbol,Basis_Set, Error);
         If Error = no_Error then
           Form_CGBF_Orbitals (Basis_Set, Molecule, CGBF_Orbitals, Error);
         else
            Put_Line ("Initialize_Adaquant.Initialize, "
                    & "Basis_Set.Get_basis_Set returned the error : "
                    & tSource_Data_Error'image(Error));
         End If;
      else
         Put_Line ("Initialize_Adaquant.Initialize, "
                 & "Basis_Set.Parse_Molecule_Descriptor_File returned an error.");
      end if;

   exception
      when anError : others =>
        Put_Line ("An exception occurred in Initialize_Adaquant.Initialize!");
        Put_Line(Exception_Information(anError));
        Error := eError;
  end Initialize;

 -- --------------------------------------------------------------------------

  procedure Parse_Molecule_Descriptor_File (File_Name : in tFile_Name;
                                   Molecule  : out tMolecule;
                                   Error     : out tSource_Data_Error) is
  use Tools;  -- for NVC_Directory
     Descriptor_File : tFile_Type;
     Directory_Set   : Boolean := False;
  begin
     Error := no_Error;
     Set_Directory (Get_Molecule_Directory);
     --  The exception Name_Error is propagated if the string
     --  given as Directory does not identify an existing directory.
     Directory_Set := True;
     Open (Descriptor_File, In_File, To_String (File_Name));
     Process_Descriptor_file (Descriptor_File, Molecule, Error);
     Close (Descriptor_File);
   exception
      when anError : Ada.IO_Exceptions.Name_Error  =>
          If not Directory_Set then
             Put_Line ("Parse_Descriptor_File: There is no directory "
                       & Get_Molecule_Directory);
             Error := eDirectory_Name_Error;
         else
             -- File not found
            Put_Line ("We can't find the file " & To_String(File_Name) & "!");
            Error := eFile_Name_Error;
         end if;
      when  anError : Ada.Text_IO.Status_Error =>
         -- File is already open
         Error := eStatus_Error;

      when  anError : others =>
         Put_Line ("An exception occurred in Parse_Descriptor_File!");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Parse_Molecule_Descriptor_File;

-- --------------------------------------------------------------------------

   procedure Process_Descriptor_file (Descriptor_File : in tFile_Type;
                                      Molecule        : out tMolecule;
                                      Error           : out tSource_Data_Error) is
   use Ada.Strings.Unbounded.Text_IO;
   use Ada.Strings.Fixed;
   use Charge_IO;
   use Multiplicity_IO;
      aLine             : Unbounded_String;
       Line_Length      : Natural;
      Field             : Unbounded_String;
      Parameter         : tMolecule_Parameter_Symbol;
      Parameter_Text    : String := Thirteen_Blanks;
      Last              : Positive;
      Name              : Unbounded_String;
      Atoms             : tAtom_List;
      Units             : tUnits;
      Charge            : tCharge;
      Multiplicity      : tMultiplicity;
      Process           : Boolean := False;
   begin
     while not End_Of_File (Descriptor_File) loop
        aLine := To_Unbounded_String (Get_Line (Descriptor_File));
        Line_Length := Length (aLine);
        If Line_Length > 13 then
           Parameter_Text := To_Upper (Slice (aLine, 1, 13));
           Process :=
              Molecule_Parameter_Map.Contains(Parameter_Text);
           If Process then
             Parameter :=
                 Molecule_Parameter_Map.Element(Parameter_Text);
           end if;
        else
          Parameter_Text := Thirteen_Blanks;
          Replace_Slice(Parameter_Text, 1, Line_Length,
                        To_Upper(To_String(aLine)));
          Process :=
              Molecule_Parameter_Map.Contains (Parameter_Text);
          If Process then
             Parameter :=
                    Molecule_Parameter_Map.Element (Parameter_Text);
          End If;
        end if;

        If Process then
           If Line_Length > 15 then
              Field := Trim (Tail (aLine, Line_Length - 15), Right);
           else
              Field := To_Unbounded_String("");
           end if;

           case Parameter is
            when eNAME => Name := Field;
            when eUNITS => If Field = "Bohr" then
                  Units := Bohr;
               elsif Field = "Angstrom" then
                  Units := Angstrom;
               else
                  Error := eUnits_Error;
                  Put_Line ("Parse_Descriptor_File, Invalid Units: "
                            & Field);
               end if;
            when eCHARGE => Get (To_String (Field), Charge, Last);
            when eMULTIPLICITY => Get (To_String (Field), Multiplicity, Last);
            when eATOMLIST => Make_Atom_List (Descriptor_File, Atoms, Error);
            when eENDLIST => Null;
            end case;
        end if;
     end loop;
     Apply_Units(Units, Atoms);
     Define_Molecule (Name, Atoms, Units, Charge, Multiplicity,
                      Molecule, Error);
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Process_Descriptor_file!");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Process_Descriptor_file;

-- --------------------------------------------------------------------------

begin  -- Initialize_Adaquant

   Options_Method_Map.Include ("RHF    ", RHF);
   Options_Method_Map.Include ("UHF    ", UHF);
   Options_Method_Map.Include ("DFT    ", DFT);
   Options_Method_Map.Include ("MINDO3 ", MINDO3);
   Options_Method_Map.Include ("UMINDO3", UMINDO3);

   Molecule_Parameter_Map.Include("NAME         ", eNAME);
   Molecule_Parameter_Map.Include("UNITS        ", eUNITS);
   Molecule_Parameter_Map.Include("CHARGE       ", eCHARGE);
   Molecule_Parameter_Map.Include("MULTIPLICITY ", eMULTIPLICITY);
   Molecule_Parameter_Map.Include("ATOMLIST     ", eATOMLIST);
   Molecule_Parameter_Map.Include("END          ", eENDLIST);

end Initialize_Adaquant;
