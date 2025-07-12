with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Exceptions; use Ada.Exceptions;

with Nuclear_Attraction_Integral;
with Electron_Repulsion_Integral;
with Tools;

-- From PyQuant
-- Basic operations over primitive gaussian basis functions based on
--    'Gaussian Expansion Methods for Molecular Orbitals.' H. Taketa,
--    S. Huzinaga, and K. O-ohata. H. Phys. Soc. Japan, 21, 2313, 1966.
--   [THO paper].
-- A gaussian is defined as:
--    g(x,y,z) = A*(x^i)*(y^j)*(z^k)*exp{-a*(r-ro)^2}
-- overlap(g') computes the overlap matrix element of g with g': Int(g*g')
-- kinetic(g') computes the kinetic energy matrix element
--    between g and g' = Int(G*lapl(G')), where lapl is the Laplacian.
-- nuclear(g',r) computes the nuclear attraction integral
--    Int(g*(1/r)*g'). Only programmed for 1s gaussians.
-- coulomb(g,g',g'',g''') computes the two-electron coulombic repulsion
--    integral Int(g(1)g'(1)(1/r12)g''(2)g'''(2)).

package body Integrals is
   Subtype tOverlap_Weight is long_float;
   Function Dual_Binomial_Sum (Pos_A   : in pfloat;  Pos_B   : in pfloat;
                               Power_A : in integer; Power_B : in integer;
                               gamma   : in long_float)
                               return long_float;

   function Form_PGBF_Integral (Matrix_Type : in tMatrix_Type;
                                PGBF_1      : in tPGBF_Data;
                                PGBF_2      : in tPGBF_Data;
                                Molecule    : in tMolecule := Null_Molecule)
                                return long_float;
   Function Overlap_Weight (PGBF_A : in tPGBF_Data;
                            PGBF_B : in tPGBF_Data)
                            return long_Float;
   Function Product_Centre (PGBF_1 : tPGBF_Data;
                            PGBF_2 : tPGBF_Data)
                         return tCartesian_Coordinates;

-- ------------------------------------------------------------

   -- Dual_Binomial_Sum corresponds to the summatiomn terms of
   -- Taketa, Huzinaga and O-ohata equation (2.12)
   Function Dual_Binomial_Sum (Pos_A   : in pfloat;  Pos_B   : in pfloat;
                               Power_A : in integer; Power_B : in integer;
                               gamma   : in long_float)
                               return long_float is
      sum     : long_float := 0.0;
      gamma_2 : long_float := 2.0* gamma;
   begin
      For Index in 0..floor (0.5 * float(Power_A + Power_B)) loop
         sum := sum + fFact_2 (2 * Index - 1) *
                Dual_Binomial(long_float(Pos_A), long_float(Pos_B),
                              Power_A, Power_B, 2 * Index) / (gamma_2 ** Index);
      end loop;
   --   If sum = 0.0 then
   --      Put_Line ("Integrals.Dual_Binomial_Sum, "
   --                & "Pos_A, Pos_B, Power_A, Power_B : ");
   --      Put_Line (float'image (float (Pos_A)) & ",  "
   --               & float'image (float(Pos_B)) & ",  "
   --               & integer'image (Power_A) & ",  " & integer'image (Power_B));
   --   end if;
      return sum;
   exception
      when anError :  others =>
        Put_Line("An exception occurred in Integrals.Dual_Binomial_Sum.");
        Put_Line (Exception_Information (anError));
        return 0.0;
   end Dual_Binomial_Sum;

-- ------------------------------------------------------------

   function Form_CGBF_Integral (Matrix_Type : in tMatrix_Type;
                                CGBF_1      : in tCGBF_Orbital;
                                CGBF_2      : in tCGBF_Orbital;
                                Molecule    : in tMolecule := Null_Molecule)
                                return long_float is
      -- <CGBF_1|Op|CGBF_2> CGBF_Integral
      -- Integral = Sum_i(CGBF_1.PGBF(i)Sum_j(CGBF_2.PGBF(j)))
      Procedure Do_CGBF_1 (CGBF_1_Index : in Contraction_Vector.Cursor);
      CGBF_Set_1    : tPGBF_Contraction_Set := CGBF_1.Contraction_Set;
      CGBF_Set_2    : tPGBF_Contraction_Set := CGBF_2.Contraction_Set;
      PGBF_Data_1   : tPGBF_Data;
      PGBF_Data_2   : tPGBF_Data;
      integral      : long_float := 0.0;
      Row           : Integer := 1;  -- For debug
      Col           : Integer := 1;  -- For debug
      Procedure Do_CGBF_1 (CGBF_1_Index : in Contraction_Vector.Cursor)is
         -- CGBF_Set_1 iteration
         use Contraction_Vector;
         Procedure Do_CGBF_2 (CGBF_2_Index : in Cursor);
         Sum  : long_float;
         Procedure Do_CGBF_2 (CGBF_2_Index : in Cursor)is
            -- CGBF_Set_2 iterationx
            PGBF_Integral : long_float;
         begin
            PGBF_Data_2.PGBF := Element (CGBF_2_Index);
            PGBF_Integral := Form_PGBF_Integral (Matrix_Type,
                                  PGBF_Data_1, PGBF_Data_2, Molecule);
   --         If Matrix_Type = Nuclear_Attraction then
   --            Put_Line ("Integrals.Do_CGBF_2, Coeffs 1 and 2, PGBF_Integral:");
   --            Put_Line (float'image (PGBF_Data_1.PGBF.Coeff) & ",  "
   --                    & float'image (PGBF_Data_2.PGBF.Coeff));
   --            Put_Line (float'image (PGBF_Data_2.PGBF.Coeff * PGBF_Integral));
   --         end if;
            Sum := Sum + long_float(PGBF_Data_2.PGBF.Coeff) * PGBF_Integral;
            Col := Col + 1;
          end Do_CGBF_2;
      begin  -- Do_CGBF_1
         PGBF_Data_1.PGBF := Element (CGBF_1_Index);
         Sum := 0.0;
         Iterate (CGBF_Set_2, Do_CGBF_2'access);
         integral := integral + long_float(PGBF_Data_1.PGBF.Coeff) * Sum;
         Row := Row + 1;
         Col := 1;
        end Do_CGBF_1;
   begin
      Initialize_PGBF_Data (CGBF_1, PGBF_Data_1);
      Initialize_PGBF_Data (CGBF_2, PGBF_Data_2);
      Iterate (CGBF_Set_1, Do_CGBF_1'access);
      return integral;
   exception
      when anError :  others =>
        Put_Line("An exception occurred in Integrals.Form_CGBF_Integral.");
        Put_Line (Exception_Information (anError));
        return 0.0;
   end Form_CGBF_Integral;

-- ------------------------------------------------------------

   function Form_Kinetic_Energy_Integral (PGBF_1 : in tPGBF_Data;
                                          PGBF_2 : in tPGBF_Data)
                                          return tKinetic_Energy is
      Alpha_2       : long_float := long_float(PGBF_2.PGBF.Exponent);
      Order_2       : tPower := PGBF_2.Order;
      Normalization : long_float := long_float (PGBF_1.PGBF.Normalization
                                              * PGBF_2.PGBF.Normalization);
      term1         : long_float;
      term2         : long_float;
      term3         : long_float;
      l2            : integer := Order_2.x_Power;
      m2            : integer := Order_2.y_Power;
      n2            : integer := Order_2.z_Power;
      PGBF_21       : tPGBF_Data := PGBF_2;
      PGBF_22       : tPGBF_Data := PGBF_2;
      PGBF_23       : tPGBF_Data := PGBF_2;
      Result        : long_float;
   begin
      -- Taketa, Huzinaga and O-ohata, 1, equation (2.14)
      term1 := Alpha_2*long_float(2*(l2 + m2 + n2)+3)
             * Form_PGBF_Overlap_Integral (PGBF_1, PGBF_2);
      PGBF_21.Order := Order_2 + (2, 0, 0);
      PGBF_22.Order := Order_2 + (0, 2, 0);
      PGBF_23.Order := Order_2 + (0, 0, 2);
      term2 := -2.0*Alpha_2**2
               * (Form_PGBF_Overlap_Integral (PGBF_1, PGBF_21)
                + Form_PGBF_Overlap_Integral (PGBF_1, PGBF_22)
                + Form_PGBF_Overlap_Integral (PGBF_1, PGBF_23));

      PGBF_21.Order := Order_2 - (2, 0, 0);
      PGBF_22.Order := Order_2 - (0, 2, 0);
      PGBF_23.Order := Order_2 - (0, 0, 2);
      term3 := -0.5 * (long_float(l2 * (l2 - 1))
                    * Form_PGBF_Overlap_Integral (PGBF_1, PGBF_21)
              + long_float(m2 * (m2 - 1))
                    * Form_PGBF_Overlap_Integral (PGBF_1, PGBF_22)
              + long_float(n2 * (n2 - 1))
                       * Form_PGBF_Overlap_Integral (PGBF_1, PGBF_23));
      Result := Normalization * (term1 + term2 + term3);
      return Result;
   exception
      when anError :  others =>
        Put_Line("An exception occurred in Integrals.Form_Kinetic_Energy_Integral.");
        Put_Line (Exception_Information (anError));
        return 0.0;
   end Form_Kinetic_Energy_Integral;

   -- ------------------------------------------------------------

   function Form_PGBF_Integral (Matrix_Type : in tMatrix_Type;
                                PGBF_1      : in tPGBF_Data;
                                PGBF_2      : in tPGBF_Data;
                                Molecule    : in tMolecule := Null_Molecule)
                                return long_float is
      use Nuclear_Attraction_Integral;
      integral : long_float := 0.0;
   begin
      case Matrix_Type is
         when Core_Hamiltonian => Null;
         when Kinetic => integral := Form_Kinetic_Energy_Integral(PGBF_1, PGBF_2);
         when Nuclear_Attraction => integral := Form_Nuclear_Attraction_Integral
                                                       (PGBF_1, PGBF_2, Molecule);
         when Overlap => integral := Form_PGBF_Normalized_Overlap_Integral
                                                       (PGBF_1, PGBF_2);
         when Transformation => Null;
         when Two_Electron => Null;
      end case;
      return integral;
   exception
      when anError :  others =>
        Put_Line("An exception occurred in Integrals.Form_PGBF_Integral.");
        Put_Line (Exception_Information (anError));
        return 0.0;
   end Form_PGBF_Integral;

-- ------------------------------------------------------------

   Function Check_Normalization (PGBF : in tPGBF; Order: in tPower)
                                 return long_float is
   use tFloat_Elementary_Functions;
      gamma        : long_float := 2.0*PGBF.Exponent;
      gamma2       : long_float := 2.0*gamma;
      l            : integer := Order.x_Power;
      m            : integer := Order.y_Power;
      n            : integer := Order.z_Power;
      Normalization : long_float := long_float(PGBF.Normalization**2);
      integral      : long_float;
   begin
      For i in 0 .. l loop
         integral := fFact_2(2*i-1) /(gamma2**i);
      end loop;
      For j in 0 .. m loop
         integral := integral * fFact_2(2*j-1) /(gamma2**j);
      end loop;
      For k in 0 .. n loop
         integral := integral * fFact_2(2*k-1) /(gamma2**k);
      end loop;
      integral := (long_float(pi)/gamma)**1.5 *integral;
     -- Put_Line ("Integrals.Check_Normalization, Integral, 1/Normalization: "
     --          & float'image(float(integral)) & ",  "
     --          & float'image(float(1.0/Normalization)));
      return Normalization * integral;
   end Check_Normalization;

-- ------------------------------------------------------------

   Function Form_PGBF_Overlap_Integral (PGBF_1 : in tPGBF_Data;
                                        PGBF_2 : in tPGBF_Data)
                                        return long_float is
   use tFloat_Elementary_Functions;
      Exp_1         : tExponent := PGBF_1.PGBF.Exponent;
      Exp_2         : tExponent := PGBF_2.PGBF.Exponent;
      SeparationSq  : long_float;
      gamma         : tExponent;
      Pre           : long_float;
      Weight        : long_float;
      -- Taketa, Huzinaga and O-otage equation (2.12)
   begin
      SeparationSq := DistanceSq (PGBF_1.Position, PGBF_2.Position);
      gamma := Exp_1 + Exp_2;
      -- Pre is Szabo and Ostlund equation A.9
      Pre := (long_float(pi)/gamma)**1.5 *
            exp (-Exp_1 * Exp_2 * SeparationSq / gamma);
      Weight :=  Overlap_Weight (PGBF_1, PGBF_2);
      return Weight * Pre;
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in "
                 & "Integrals.Form_PGBF_Overlap_Integral!");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end Form_PGBF_Overlap_Integral;

-- ------------------------------------------------------------

-- PyQuant self._normalization * other._normalization *
--         overlap(self.exponent, self.powers,self.origin,
--                 other.exponent,other.powers,other.origin
   Function Form_PGBF_Normalized_Overlap_Integral (PGBF_1 : in tPGBF_Data;
                                                   PGBF_2 : in tPGBF_Data)
                                                   return long_float is
      Normalization : long_float := long_float (PGBF_1.PGBF.Normalization
                                              * PGBF_2.PGBF.Normalization);
      result        : long_float;
   begin
      result := Chop (Normalization * Form_PGBF_Overlap_Integral (PGBF_1, PGBF_2));
      if result > 1.00000001 then
         Put_Line ("Integrals.Form_PGBF_Normalized_Overlap_Integral > 1: "
                  & float'image(float(result)));
      end if;
      return result;
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in "
                 & "Integrals.Form_PGBF_Normalized_Overlap_Integral!");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end Form_PGBF_Normalized_Overlap_Integral;

-- ------------------------------------------------------------

   -- Initialize_PGBF_Data initializes the Nuclear_Charge, Position
   -- and Order fields of PGBF in preparation for later
   -- processing the individual PGBF's (exponent and coefficient)
   -- of Orbital's PGBF contraction set.
   Procedure Initialize_PGBF_Data (Orbital : in tCGBF_Orbital;
                                   PGBF    : in out tPGBF_Data) is
   begin
      PGBF.Nuclear_Charge := Orbital.Nuclear_Charge;
      PGBF.Position := Orbital.Position;
      PGBF.Order := Orbital.Order;
   exception
      when anError :  others =>
        Put_Line("An exception occurred in Integrals.Initialize_PGBF_Data.");
        Put_Line (Exception_Information (anError));
   end Initialize_PGBF_Data;

-- ------------------------------------------------------------

     -- x, y and z weightings of Taketa, Huzinaga and O-otage equation (2.12)
     Function Overlap_Weight (PGBF_A : in tPGBF_Data;
                              PGBF_B : in tPGBF_Data)
                              return long_Float is
     Pos_A   : tCartesian_Coordinates := PGBF_A.Position;
     Pos_B   : tCartesian_Coordinates := PGBF_B.Position;
     Power_A : tPower := PGBF_A.Order;
     Power_B : tPower := PGBF_B.Order;
     Pos_P   : tCartesian_Coordinates;
     gamma   : tExponent;
     PA_Dist : tCartesian_Coordinates;
     PB_Dist : tCartesian_Coordinates;
     wx      : tOverlap_Weight;
     wy      : tOverlap_Weight;
     wz      : tOverlap_Weight;
   begin
      gamma := PGBF_A.PGBF.Exponent + PGBF_B.PGBF.Exponent;
      Pos_P := Product_Centre(PGBF_A, PGBF_B);
      PA_Dist := Pos_A - Pos_P;
      PB_Dist := Pos_B - Pos_P;
      wx := Dual_Binomial_Sum
                    (PA_Dist.x, PB_Dist.x, Power_A.x_Power, Power_B.x_Power, gamma);
      wy := Dual_Binomial_Sum
                    (PA_Dist.y, PB_Dist.y, Power_A.y_Power, Power_B.y_Power, gamma);
      wz := Dual_Binomial_Sum
                    (PA_Dist.z, PB_Dist.z, Power_A.z_Power, Power_B.z_Power, gamma);
     --  If Chop (DistanceSq (PGBF_A.Position, PGBF_B.Position)) = 0.0
     --    And Chop(wx * wy * wz) /= 1.0 then
     --    Print_Coordinates("PA_Dist: ", PA_Dist);
     --    Print_Coordinates ("PB_Dist: ", PB_Dist);
     --    Put_Line ("x, y, z weights: "
     --            & (float'image (float (wx)) & ",  "  & float'image (float (wy))
     --            & ",  "  & float'image (float (wz))));
     -- end if;
      return wx * wy * wz;
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Integrals.Overlap_Weight!");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end Overlap_Weight;

-- ------------------------------------------------------------

   Function Product_Centre(PGBF_1 : in tPGBF_Data;
                           PGBF_2 : in tPGBF_Data)
                           return tCartesian_Coordinates is
      Pos_1 : tCartesian_Coordinates := PGBF_1.Position;
      Pos_2 : tCartesian_Coordinates := PGBF_2.Position;
      Exp_1 : tExponent := PGBF_1.PGBF.Exponent;
      Exp_2 : tExponent := PGBF_2.PGBF.Exponent;
      scale_1 : pFloat;
      scale_2 : pFloat;
   begin
      -- Szabo and Ostlund equation (A.4)
      scale_1 := pFloat(Exp_1/(Exp_1 + Exp_2));
      scale_2 := pFloat(Exp_2/(Exp_1 + Exp_2));
      return
        (scale_1 * Pos_1.x + scale_2 * Pos_2.x,
         scale_1 * Pos_1.y + scale_2 * Pos_2.y,
         scale_1 * Pos_1.z + scale_2 * Pos_2.z);
   end Product_Centre;

-- ------------------------------------------------------------

end Integrals;

