with Ada.Numerics.Generic_Real_Arrays; use Ada.Numerics;

Package Matrix_Types is
   Package Long_Float_Arrays is new Generic_Real_Arrays (Long_Float);
   Type tLong_Float_Matrix is new Long_Float_Arrays.Real_Matrix;
   Type tLong_Float_Vector is new Long_Float_Arrays.Real_Vector;

   Subtype tCoefficient_Matrix is tLong_Float_Matrix;
   Subtype tCharge_Density_Matrix is tLong_Float_Matrix;
   Subtype tCore_Hamiltonian_Matrix is tLong_Float_Matrix;
   Subtype tFock_Matrix is tLong_Float_Matrix;
   Subtype tOverlap_Matrix is tLong_Float_Matrix;
   Subtype tTwo_Electron_Matrix is tLong_Float_Matrix;
   Subtype tTransformation_Matrix is tLong_Float_Matrix;
   Subtype tEigen_Values is tLong_Float_Vector;
   Subtype tEigen_Vectors is tLong_Float_Matrix;

   Function "*" (Left, Right : in tLong_Float_Matrix) return tLong_Float_Matrix;
   Function "*" (Left : in tLong_Float_Vector; Right : in tLong_Float_Matrix)
                                              return tLong_Float_Matrix;
end Matrix_Types;
