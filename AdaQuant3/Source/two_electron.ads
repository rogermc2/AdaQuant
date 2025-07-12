
with Types; use Types;

with Molecule; use Molecule;
with Matrix_Types; use Matrix_Types;

Package Two_Electron is
   procedure Form_Two_Electron_Matrix
                          (Basis_Set           : in tCGBF_Orbital_Set;
                           Two_Electron_Matrix : in out tTwo_Electron_Matrix;
                           Symmetric           : in Boolean := False);
   procedure Get_Two_Electron_Matrix (Basis_Set_Symbol : in tBasis_Set_Symbol;
                                   Orbitals            : in tCGBF_Orbital_Set;
                                   Two_Electron_Matrix : in out tTwo_Electron_Matrix;
                                   Molecule_File_Name  : in tFile_Name);
   procedure Save_Two_Electron_Matrix
                           (Two_Electron_Matrix : in tTwo_Electron_Matrix;
                            Basis_Set_Symbol : in tBasis_Set_Symbol;
                            Molecule_File_Name  : in tFile_Name);
end Two_Electron;
