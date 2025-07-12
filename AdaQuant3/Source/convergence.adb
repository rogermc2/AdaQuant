with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

with Maths; use Maths;

Package body Convergence is
   use Long_Float_Arrays;
   alpha : constant Long_Float := 0.5;
   Procedure Anderson_Averager
                (Previous_Density_Matrix : in tCharge_Density_Matrix;
                 Density_Matrix          : in out tCharge_Density_Matrix);

   Procedure Simple_Averager
                (Previous_Density_Matrix : in tCharge_Density_Matrix;
                 Density_Matrix          : in out tCharge_Density_Matrix);

   -- ----------------------------------------------------------------------------

   Procedure Average_Charge_Density
                       (Averaging_Type             : in tAverager;
                        Previous_Density_Matrix    : in tCharge_Density_Matrix;
                        Density_Matrix             : in out tCharge_Density_Matrix;
                        Convergence                : out Long_Float) is
   use tFloat_Elementary_Functions;
      Size            : Positive := Density_Matrix'Length;
   begin
      Convergence := 0.0;
      case Averaging_Type is
         when eSimple_Average => Simple_Averager
                                        (Previous_Density_Matrix, Density_Matrix);
         when eAnderson => Anderson_Averager
                                        (Previous_Density_Matrix, Density_Matrix);
         when others => Null;
      end case;
      For Row in 1..Size loop
         For Col in 1 .. Size loop
            -- Convergence is squared difference of average (new) and previous
            -- density matrix elements.
           Convergence := Convergence + (Density_Matrix (Row, Col)
                                        - Previous_Density_Matrix (Row, Col))**2;
            end loop;
      end loop;
      Convergence := SqRt(Convergence / Long_Float(Size**2));
   end Average_Charge_Density;

   -- ----------------------------------------------------------------------------

   Procedure Anderson_Averager
                (Previous_Density_Matrix : in tCharge_Density_Matrix;
                 Density_Matrix          : in out tCharge_Density_Matrix) is
   begin
      Null;
   end Anderson_Averager;

      -- ----------------------------------------------------------------------------

   Procedure Simple_Averager
                (Previous_Density_Matrix : in tCharge_Density_Matrix;
                 Density_Matrix      : in out tCharge_Density_Matrix) is
      Averaged_Matrix : tCharge_Density_Matrix := Density_Matrix;
   begin
      Density_Matrix := alpha * tLong_Float_Matrix(Density_Matrix)
              + (1.0 - alpha) * tLong_Float_Matrix (Previous_Density_Matrix);
   end Simple_Averager;


end Convergence;
