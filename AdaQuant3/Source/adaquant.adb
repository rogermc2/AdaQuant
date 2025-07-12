with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;

with Types; use Types;
with Basis_Set; use Basis_Set;

with AdaQuant_Types; use AdaQuant_Types;
with Initialize_AdaQuant; use Initialize_AdaQuant;
with Molecule; use Molecule;
with Self_Consistent_Field; use Self_Consistent_Field;

procedure AdaQuant is
   use Ada.Containers;
   Molecule         : tMolecule;
   Basis_Set_Symbol : tBasis_Set_Symbol;
   CGBF_Orbitals    : tCGBF_Orbital_Set;
   File_Name        : tFile_Name;
   Options          : tOptions;
   Error            : tSource_Data_Error;
   SCF_Result       : tSCF_Result;
   -- dump             : Character;
begin
   Options.Orthogonalization_Method := Canonical;
   Initialize (Molecule, Options, Basis_Set_Symbol,
               CGBF_Orbitals, File_Name, Error);
   if Error = no_Error then
      SCF_Result := Solve_For_Self_Consistent_Field
                                    (Basis_Set_Symbol, CGBF_Orbitals,
                                     Molecule, File_Name, Options);
   else
      Put_Line ("AdaQuant, Initialize_AdaQuant.Initialize returned "
                & tSource_Data_Error'image(Error));
   end if;
  -- Put_Line ("Press any key to quit.");
  -- Get(dump);
   Put_Line ("AdaQuant closing down.");
end AdaQuant;
