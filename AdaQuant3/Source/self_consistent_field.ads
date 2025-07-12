with Types; use Types;
with Basis_Set; use Basis_Set;

with AdaQuant_Types; use AdaQuant_Types;
with Molecule; use Molecule;

Package Self_Consistent_Field is
   Type tSCF_Result is private;

   function Solve_For_Self_Consistent_Field
                           (Basis_Set_Symbol   : in tBasis_Set_Symbol;
                            Orbitals           : in tCGBF_Orbital_Set;
                            Molecule           : in tMolecule;
                            Molecule_File_Name : in tFile_Name;
                            Options            : in tOptions)
                            return tSCF_Result;
private

   Type tSCF_Result is
      record
         method : tCalculation_Method;
      end record;
end Self_Consistent_Field;
