with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Maths; use Maths;
with Tools;

Package body Fock is
   use Long_Float_Arrays;

   Function Electronic_Energy (Core_Matrix    : in tCore_Hamiltonian_Matrix;
                               Fock_Matrix    : in tFock_Matrix;
                               Density_Matrix : in tCharge_Density_Matrix)
                               return long_float;

   -- --------------------------------------------------------------------------

   Function Electronic_Energy (Core_Matrix    : in tCore_Hamiltonian_Matrix;
                               Fock_Matrix    : in tFock_Matrix;
                               Density_Matrix : in tCharge_Density_Matrix)
                               return long_float is
      Size : Positive := Core_Matrix'Length;
      sum  : long_float := 0.0;
   begin
      For Row in 1 .. Size loop
         For Col in 1 .. Size loop
            Sum := Sum + Density_Matrix (Row, Col)
                       *(Core_Matrix (Row, Col) + Fock_Matrix (Row, Col));
         end loop;
      end loop;
      return 0.5 * Sum;
   end Electronic_Energy;

-- --------------------------------------------------------------------------

   -- Form_Fock_Matrix implements Szabo and Ostlund
   -- equations (3.154) and (3.177)
   Procedure Form_Orthogonal_Fock_Matrix
                         (Hamiltonian_Matrix    : in tCore_Hamiltonian_Matrix;
                          Density_Matrix        : in tCharge_Density_Matrix;
                          Two_Electron_Matrix   : in tTwo_Electron_Matrix;
                          Transformation_Matrix : in tTransformation_Matrix;
                          Fock_Matrix           : in out tFock_Matrix;
                          Electron_Energy       : out Long_Float) is
      Size      : Natural := Hamiltonian_Matrix'Length;
      G_element : long_float;
   begin
      For Row in 1 .. Size loop
         For Col in 1 .. Size loop
            G_element := 0.0;
           For Sum_Row in 1 .. Size loop
              For Sum_Col in 1 .. Size loop
                 G_element := G_element + Density_Matrix (Sum_Row, Sum_Col)
                   * (Two_Electron_Matrix
                         ((Row-1) * Size + Col, (Sum_Col-1) * Size + Sum_Row)
                       - 0.5*Two_Electron_Matrix
                            ((Row-1) * Size + Sum_Row, (Sum_Col-1) * Size + Col));
              end loop;
            end loop;
            -- Szabo and Ostlund equation (3.154)
         Fock_Matrix(Row, Col) := Hamiltonian_Matrix(Row, Col) + G_element;
         end loop;
      end loop;
      -- Put_Line ("Form_Fock_Matrix, Fock matrix");
      -- Put_Line ("     (cf S&O equation (3.273)):");
      -- Tools.Print_Square_Matrix (Fock_Matrix, 1, 5);
      -- Szabo and Ostlund equation (3.184)
      Electron_Energy := Electronic_Energy (Hamiltonian_Matrix, Fock_Matrix,
                                            Density_Matrix);
      -- Szabo and Ostlund equation (3.177)
      Fock_Matrix :=  Transpose (Transformation_Matrix)
                    * Fock_Matrix * Transformation_Matrix;
      -- Put_Line ("Form_Fock_Matrix, Transformed Fock matrix:");
      -- Tools.Print_Square_Matrix (Fock_Matrix);
   exception
      when anError :  others =>
         Put_Line("An exceptiom occurred in Fock.Form_Fock_Matrix.");
         Put_Line(Exception_Information(anError));
   end Form_Orthogonal_Fock_Matrix;

-- ------------------------------------------------------------
end Fock;
