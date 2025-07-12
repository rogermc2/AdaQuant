with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics; use Ada.Numerics;

with Tools; use Tools;
with Maths; use Maths;
with Convergence; use Convergence;

Package body Charge_Density is
   Function Charge_Density_Element (Matrix : in tCoefficient_Matrix;
                                    Row    : in Positive; Col : in Positive)
                                    return long_float;

   Function Charge_Density_Element (Matrix : in tCoefficient_Matrix;
                                    Row    : in Positive; Col : in Positive)
                                    return long_float is
      Size : Positive := Floor(0.5*float(Matrix'Length));
      Sum  : long_float := 0.0;
   begin
      For C_Col in 1 .. Size loop
         Sum := Sum + Matrix(Row, C_Col)*Matrix(Col, C_Col);
      end loop;
      return 2.0*Sum;
   exception
      when anError :  others =>
         Put_Line ("An exceptiom occurred in Charge_Density."
                 & "Update_Charge_Density_Matrix.");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end Charge_Density_Element;

-- ------------------------------------------------------------

   Procedure Form_Charge_Density_Matrix
                         (Initial_Charge_Density : in tCharge_Density;
                          Averaging_Type         : in tAverager;
                          Hamiltonian_Matrix     : in tCore_Hamiltonian_Matrix;
                          Transformation_Matrix  : in tTransformation_Matrix;
                          Charge_Density_Matrix  : out tCharge_Density_Matrix;
                          Error                  : out tSource_Data_Error) is
      use Long_Float_Arrays;
      Size               : Natural := Hamiltonian_Matrix'Length;
      Fock_Matrix        : tFock_Matrix(1..Size,1..Size);
      Eigenvalues        : Long_Float_Arrays.Real_Vector(1..Size);
      Coefficient_Matrix : tCoefficient_Matrix(1..Size,1..Size);
   begin
      Error := no_Error;
      -- Szabo and Ostlund's equations (3.266) - (3.268)
      -- using the Hamiltonian_Matrix as a first approximation to a Fock matrix
      -- and transforming it to the orthonormalized basis set.
      case Initial_Charge_Density is
         when eZero =>
            For Row in Charge_Density_Matrix'First .. Charge_Density_Matrix'Last
            loop
              For Col in Charge_Density_Matrix'First .. Charge_Density_Matrix'Last
              loop
                Charge_Density_Matrix (Row, Col) := 0.0;
              end loop;
            end loop;
         when eFock => Fock_Matrix := tFock_Matrix(Transpose(Transformation_Matrix)
                                    * Hamiltonian_Matrix * Transformation_Matrix);

            -- Put_Line ("Form_Charge_Density_Matrix's Fock matrix:");
            -- Print_Square_Matrix (tLong_Float_Matrix (Fock_Matrix));
            Symmetrize_Matrix(Fock_Matrix);
            Eigensystem (tLong_Float_Matrix (Fock_Matrix),
                         Eigenvalues, Coefficient_Matrix);
            -- Eigensystem returns eVectors as rows
            Coefficient_Matrix := Transpose(Coefficient_Matrix);
            -- Put_Line ("Form_Charge_Density_Matrix, eigenvalues:");
            -- Print_Vector (tLong_Float_Vector (Eigenvalues));
            -- Put_Line ("Coefficient matrix:");
            -- Print_Square_Matrix (tLong_Float_Matrix (Coefficient_Matrix));
            -- Put_Line ("Compare F.Ct with Ct.E:");
            -- Print_Square_Matrix (tLong_Float_Matrix (Fock_Matrix
            --            * Coefficient_Matrix - Coefficient_Matrix *
            --              Diagonal_Matrix (tLong_Float_Vector (Eigenvalues))));

            Coefficient_Matrix := Transformation_Matrix * Coefficient_Matrix;
            -- Put_Line ("Transformed coefficient matrix:");
            -- Print_Square_Matrix (tLong_Float_Matrix (Coefficient_Matrix));
            For Row in Charge_Density_Matrix'First .. Charge_Density_Matrix'Last
            loop
              For Col in Charge_Density_Matrix'First .. Charge_Density_Matrix'Last
              loop
                Charge_Density_Matrix (Row, Col) := 0.0;
                For i in Charge_Density_Matrix'First ..
                     Floor (float(Charge_Density_Matrix'Last) / 2.0) loop
                     Charge_Density_Matrix (Row, Col) :=
                          Charge_Density_Matrix (Row, Col)
                        + Coefficient_Matrix (Row, i)* Coefficient_Matrix (Col, i);
                end loop;
              Charge_Density_Matrix (Row, Col) :=
                                     2.0 * Charge_Density_Matrix (Row, Col);
              end loop;
            end loop;
            -- Put_Line ("Form_Charge_Density_Matrix, Charge density matrix:");
            -- Print_Square_Matrix (Charge_Density_Matrix);
        end case;

   exception
      when anError :  others =>
         Put_Line ("An exceptiom occurred in Charge_Density"
                   & ".Form_Charge_Density_Matrix.");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Form_Charge_Density_Matrix;

-- ------------------------------------------------------------

   Procedure Update_Charge_Density_Matrix
                         (Averaging_Type         : in tAverager;
                          Coefficient_Matrix     : in tCoefficient_Matrix;
                          Charge_Density_Matrix  : in out tCharge_Density_Matrix;
                          Convergence            : out long_float) is
      Size : Positive := Coefficient_Matrix'Length;
      Old_Charge_Density_Matrix : tCharge_Density_Matrix := Charge_Density_Matrix;
   begin
      For Row in 1..Size loop
         For Col in 1 .. Size loop
            Charge_Density_Matrix (Row, Col) :=
                  Charge_Density_Element (Coefficient_Matrix, Row, Col);
         end loop;
      end loop;

      Average_Charge_Density (Averaging_Type, Old_Charge_Density_Matrix,
                              Charge_Density_Matrix, Convergence);
      -- Put_Line ("Updated charge density matrix:");
      -- Print_Square_Matrix (Charge_Density_Matrix);

   exception
      when anError :  others =>
         Put_Line ("An exceptiom occurred in Charge_Density."
                 & "Update_Charge_Density_Matrix.");
         Put_Line(Exception_Information(anError));
   end Update_Charge_Density_Matrix;

-- ------------------------------------------------------------

end Charge_Density;
