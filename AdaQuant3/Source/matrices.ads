
with Types; use Types;

with AdaQuant_Types; use AdaQuant_Types;
with Matrix_Types; use Matrix_Types;
with Cartesian; use Cartesian;
with Maths; use Maths;
with Integrals; use Integrals;
with Molecule; use Molecule;
with Coefficients; use Coefficients;

package Matrices is
   use Long_Float_Arrays;
   Procedure Create_Core_Hamiltonian_Matrix
                          (Basis_Set : in tCGBF_Orbital_Set;
                           Molecule  : in tMolecule;
                           Hamiltonian_Matrix   : in out tCore_Hamiltonian_Matrix);
   Procedure Form_Overlap_Matrix(Basis_Set      : in tCGBF_Orbital_Set;
                                 Overlap_Matrix : out tLong_Float_Matrix);

   Procedure Form_Transformation_Matrix (Overlap_Matrix : in tLong_Float_Matrix;
                                  Method : in tOrthog_Method := Symmetric;
                   Transformation_Matrix : out tTransformation_Matrix;
                                   Error : out tSource_Data_Error);
end Matrices;
