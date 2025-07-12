
with Matrix_Types; use Matrix_Types;
with Maths; use Maths;

Package Coefficients is
   procedure Form_Coefficient_Matrix(Fock_Matrix : in tFock_Matrix;
                           Transformation_Matrix : in tTransformation_Matrix;
                                     eValues     : in out tEigen_Values;
                            Coefficient_Matrix : in out tCoefficient_Matrix);
end Coefficients;
