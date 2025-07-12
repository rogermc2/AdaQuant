with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;

with Types; use Types;
with Basis_Set; use Basis_Set;
with AdaQuant_Types; use AdaQuant_Types;
with Molecule; use Molecule;

package Initialize_Adaquant is

   procedure Initialize (Molecule         : out tMolecule;
                         Options          : out tOptions;
                         Basis_Set_Symbol : out tBasis_Set_Symbol;
                         CGBF_Orbitals    : in out tCGBF_Orbital_Set;
                         File_Name        : out tFile_Name;
                         Error            : out tSource_Data_Error);

end Initialize_Adaquant;
