with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
use Ada.Containers;

with Types; use Types;
with Basis_Set; use Basis_Set;
with Shell_Basis_Set; use Shell_Basis_Set;

with Adaquant_Types; use Adaquant_Types;
with Maths; use Maths;
with Matrix_Types; use Matrix_Types;

Package Tools is

   use Long_Float_Arrays;
   Package BasisMaps is new  Ada.Containers.Ordered_Maps
                  (Key_Type=>Unbounded_String, Element_Type=>Unbounded_String);

   Function Get_Molecule_Directory return String;
   Function get_basis_data(name : String) return tBasis_Data;
   function Contains (name :  String)  return Boolean;
   Function Diagonal_Matrix (aVector : in tLong_Float_Vector)
                                       return tLong_Float_Matrix;
--   Print_CGBF_Orbitals(CGBF_Orbitals : in tCGBF_Orbital_Set);
   Procedure Print_Contraction_Item (Curs : in Contraction_Vector.Cursor);
   Procedure Print_Matrix_Type(Matrix_Type : in tMatrix_Type);
   Procedure Print_Square_Matrix(theMatrix : in tLong_Float_Matrix);
   Procedure Print_Square_Matrix (theMatrix : in tLong_Float_Matrix;
                                 start     : in Positive; last : in Positive);
   Procedure Print_Vector (theVector : in tLong_Float_Vector);
   Procedure Swap_Cols (Matrix       : in out tLong_Float_Matrix;
                        Col_A, Col_B : in integer);
   Procedure Symmetrize_Matrix(theMatrix : in out tLong_Float_Matrix);
  end Tools;
