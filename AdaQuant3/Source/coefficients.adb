with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Tools;

Package body Coefficients is
   use Long_Float_Arrays;
   procedure Form_Coefficient_Matrix (Fock_Matrix  : in tFock_Matrix;
                             Transformation_Matrix : in tTransformation_Matrix;
                                      eValues      : in out tEigen_Values;
                                Coefficient_Matrix : in out tCoefficient_Matrix) is
      Size                     : Natural := Fock_Matrix'Length;
      Symmetrized_Fock_Matrix  : tLong_Float_Matrix
                                     := tLong_Float_Matrix(Fock_Matrix);
      eVectors                 : tEigen_Vectors(1 .. Size, 1 .. Size);
   begin
      Tools.Symmetrize_Matrix(tLong_Float_Matrix(Symmetrized_Fock_Matrix));
      Long_Float_Arrays.Eigensystem (Real_Matrix (Symmetrized_Fock_Matrix),
                                     Real_Vector (eValues), Real_Matrix (eVectors));
      -- Eigensystem returns eVectors as rows
      Coefficient_Matrix := Transpose(tCoefficient_Matrix(eVectors));
--      Put_Line ("Coefficients, Eigen-values:");
--      Tools.Print_Vector (tReal_Vector(eValues));
--      Put_Line ("Coefficients matrix:");
--      Tools.Print_Square_Matrix (Coefficient_Matrix, 1, 5);
        Coefficient_Matrix := Transformation_Matrix*Coefficient_Matrix;
   exception
      when anError :  others =>
        Put_Line("An exceptiom occurred in Coefficients.Form_Coefficient_Matrix.");
        Put_Line(Exception_Information(anError));
   end Form_Coefficient_Matrix;
end Coefficients;
