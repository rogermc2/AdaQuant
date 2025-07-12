with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Exceptions; use Ada.Exceptions;

with Types;

with AdaQuant_Types; use AdaQuant_Types;
with Cartesian; use Cartesian;
with Maths; use Maths;
with Gamma;
with Shell_Basis_Set; use Shell_Basis_Set;

--    From Gaussian Expansion Methods for Molecular Orbitals by Taketa,
--    Huzinaga, and O-ohata. H. Phys. Soc. Japan, 21, 2313, 1966.

Package body Electron_Repulsion_Integral is
   use tFloat_Elementary_Functions;

  Type tReal_five_Dim_Array is
     Array (Natural range <>, Natural range <>, Natural range <>,
            Natural range <>, Natural range <>) of Long_Float;

   Function Form_B_Array (Order_1 : in Natural;      Order_2 : in Natural;
                          Order_3 : in Natural;      Order_4 : in Natural;
                          Pos_A   : in pFloat;       Pos_B   : in pFloat;
                          Pos_C   : in pFloat;       Pos_D   : in pFloat;
                          Pos_P   : in pFloat;       Pos_Q   : in pFloat;
                          gamma1  : in Long_Float;   gamma2  : in Long_Float)
                          return tReal_five_Dim_Array;
   Function Form_Normalized_PGBF_Repulsion_Integral
                        (PGBF_Data_A : tPGBF_Data;
                         PGBF_Data_B : tPGBF_Data;
                         PGBF_Data_C : tPGBF_Data;
                         PGBF_Data_D : tPGBF_Data)
                         return Long_Float;

-- -----------------------------------------------------------------

--    From Gaussian Expansion Methods for Molecular Orbitals by Taketa,
--    Huzinaga and O-ohata equation (2.22)
   Function Form_B_Array (Order_1 : in Natural; Order_2 : in Natural;
                          Order_3 : in Natural; Order_4 : in Natural;
                          Pos_A   : in pFloat;  Pos_B   : in pFloat;
                          Pos_C   : in pFloat;  Pos_D   : in pFloat;
                          Pos_P   : in pFloat;  Pos_Q   : in pFloat;
                          gamma1  : in Long_Float;   gamma2  : in Long_Float)
                          return tReal_five_Dim_Array is
      Dist_PA       : pFloat;
      Dist_PB       : pFloat;
      Dist_QC       : pFloat;
      Dist_QD       : pFloat;
      Dim_i1        : Natural := Order_1 + Order_2;
      Dim_i2        : Natural := Order_3 + Order_4;
      Dim_r1        : Natural := Natural(Floor(float(Dim_i1/2)));
      Dim_r2        : Natural := Natural(Floor(float(Dim_i2/2)));
      Dim_u         : Natural := Natural (Floor(float ((Dim_i1 + Dim_i2) / 2)));
      B_Array       : tReal_five_Dim_Array (0 .. Dim_i1, 0 .. Dim_i2,
                                            0 .. Dim_r1, 0 .. Dim_r2, 0 .. Dim_u);
      -- Taketa, Huzinaga and O-ohata define delta as 1/(4 gamma1) + 1/(4 gamma2)
      -- but the 4 can be cancelled out in equation (2.22).
      Four_delta       : Long_Float := 1.0/gamma1 + 1.0/gamma2;
      i1_Term, i2_Term : Long_Float;
      r1_Fact          : Long_Float;
      r12_Term         : Long_Float;
      part_1, part_2   : Long_Float;
      part_3, part_4   : Long_Float;
      ii1, ii2         : integer;
      rr1, rr2         : integer;
      uu               : integer;
   begin
      Dist_PA := Pos_P - Pos_A;
      Dist_PB := Pos_P - Pos_B;
      Dist_QC := Pos_Q - Pos_C;
      Dist_QD := Pos_Q - Pos_D;
      For i1 in 0 .. Dim_i1 loop
         ii1 := i1;
         i1_Term := Long_Float(Factorial(i1)) *
                    Dual_Binomial (Long_Float (Dist_PA), Long_Float (Dist_PB),
                                   Order_1, Order_2, i1);
         For i2 in 0 ..Dim_i2 loop
            ii2 := i2;
            i2_Term := Long_Float(Factorial(i2)) *
                        Dual_Binomial (Long_Float (Dist_QC), Long_Float (Dist_QD),
                                       Order_3, Order_4, i2);
            For r1 in 0 .. Floor(float(Dim_i1/2)) loop
               rr1 := r1;
               r1_Fact := Long_Float(Factorial(r1));
               For r2 in 0 .. Floor(float(Dim_i2/2))  loop
                  rr2 := r2;
                  part_1 := gamma1 ** (r1 - i1) * gamma2 ** (r2 - i2);
                  part_2:= Four_delta ** (r1 + r2 - i1 - i2);
                  part_3 := r1_Fact
                    * Long_Float (Factorial (r2) * Factorial (i1 - 2 * r1)
                                            * Factorial (i2 - 2 * r2));
                  part_4 := Long_Float(Factorial (i1 + i2 - 2*(r1 + r2)));
                  r12_Term := part_1*part_2/part_3 * part_4;
                  For u in 0.. Floor(float ((Dim_i1 + Dim_i2)/2 - r1 - r2)) loop
                     uu := u;
                     part_1 := i1_Term * i2_Term * r12_Term;
                     part_2 := Long_Float (Pos_P) ** (i1 + i2 - 2 * (r1 + r2 + u));
                     part_3 := (Four_delta/4.0) ** u;
                     part_4 := Long_Float (Factorial (u) *
                                    Factorial (i1+i2 - 2 * (r1+r2 + u)));
                     B_Array (i1,i2, r1,r2, u) :=
                         Long_Float((-1.0)**(i2 + u)) * part_1 * part_2  * part_3/ part_4;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

      return B_Array;
   exception
      when anError :  Constraint_Error =>
        Put_Line ("An exception occurred in " &
                   "Electron_Repulsion_Integral.Form_B_Array.");
        Put_Line ("i1_Term, i2_Term, r12_Term: " & Long_Float'image(i1_Term)
                  & "  " & Long_Float'image(i2_Term) & "  " &  Long_Float'image(r12_Term));
        Put_Line ("Dist_QC, Four_delta: " & pfloat'image (Dist_QC)
                   & "  " & Long_Float'image (Four_delta));
        Put_Line ("i1, i2, r1, r2, u: " & integer'image (ii1) & integer'image (ii2)
                    & integer'image(rr1)  & integer'image(rr2) & integer'image(uu));
       Put_Line (Exception_Information (anError));
       raise;
     when anError :  others =>
       Put_Line ("An exception occurred in " &
                 "Electron_Repulsion_Integral.Form_B_Array.");
       Put_Line (Exception_Information (anError));
       return B_Array;

   end Form_B_Array;

-- -----------------------------------------------------------------

   -- Form_PGBF_Repulsion_Integral is equivalent to Szabo and Ostlund's
   -- normalized TWOE function (page 423) of un-normalized wave-functions.
   -- Form_PGBF_Repulsion_Integral implements Taketa, Huzinaga and O-ohata
   -- Equation (2-22)
   Function Form_Normalized_PGBF_Repulsion_Integral
                        (PGBF_Data_A : tPGBF_Data;
                         PGBF_Data_B : tPGBF_Data;
                         PGBF_Data_C : tPGBF_Data;
                         PGBF_Data_D : tPGBF_Data) return Long_Float is
      use Gamma;
      use Types;
      PGBF_A        : tPGBF := PGBF_Data_A.PGBF;
      PGBF_B        : tPGBF := PGBF_Data_B.PGBF;
      PGBF_C        : tPGBF := PGBF_Data_C.PGBF;
      PGBF_D        : tPGBF := PGBF_Data_D.PGBF;
      Pos_A         : tCartesian_Coordinates := PGBF_Data_A.Position;
      Pos_B         : tCartesian_Coordinates := PGBF_Data_B.Position;
      Pos_C         : tCartesian_Coordinates := PGBF_Data_C.Position;
      Pos_D         : tCartesian_Coordinates := PGBF_Data_D.Position;
      Pos_P         : tCartesian_Coordinates;
      Pos_Q         : tCartesian_Coordinates;
      Exp_1         : Long_Float := Long_Float(PGBF_A.Exponent);
      Exp_2         : Long_Float := Long_Float (PGBF_B.Exponent);
      Exp_3         : Long_Float := Long_Float(PGBF_C.Exponent);
      Exp_4         : Long_Float := Long_Float (PGBF_D.Exponent);
      Order_1       : tPower := PGBF_Data_A.Order;
      Order_2       : tPower := PGBF_Data_B.Order;
      Order_3       : tPower := PGBF_Data_C.Order;
      Order_4       : tPower := PGBF_Data_D.Order;
      Normalization : Long_Float;
      gamma_1       : Long_Float := Exp_1 + Exp_2;
      gamma_2       : Long_Float := Exp_3 + Exp_4;
      -- Taketa, Huzinaga and O-ohata define delta as 1/(4 gamma1) + 1/(4 gamma2)
      -- but the 4 can be cancelled out in equation (2.22).
      Four_delta    : Long_Float := 1.0/gamma_1 + 1.0/gamma_2;
      AB_Sq, CD_Sq  : Long_Float;
      PQ_Sq         : Long_Float;
      Dim_i1        : Natural := Order_1.x_Power + Order_2.x_Power;
      Dim_i2        : Natural := Order_3.x_Power + Order_4.x_Power;
      Dim_j1        : Natural := Order_1.y_Power + Order_2.y_Power;
      Dim_j2        : Natural := Order_3.y_Power + Order_4.y_Power;
      Dim_k1        : Natural := Order_1.z_Power + Order_2.z_Power;
      Dim_k2        : Natural := Order_3.z_Power + Order_4.z_Power;
      Dim_r1        : Natural := Natural(float'Floor(float(Dim_i1/2)));
      Dim_r2        : Natural := Natural(float'Floor(float(Dim_i2/2)));
      Dim_s1        : Natural := Natural(float'Floor(float(Dim_j1/2)));
      Dim_s2        : Natural := Natural(float'Floor(float(Dim_j2/2)));
      Dim_t1        : Natural := Natural(float'Floor(float(Dim_k1/2)));
      Dim_t2        : Natural := Natural(float'Floor(float(Dim_k2/2)));
      Dim_u         : Natural := Natural (float'Floor
                                (float ((Dim_i1 + Dim_i2) / 2)));
      Dim_v         : Natural := Natural(float'Floor
                                (float ((Dim_j1 + Dim_j2) / 2)));
      Dim_w         : Natural := Natural (float'Floor
                                (float ((Dim_k1 + Dim_k2) / 2)));
      iru_Array     : tReal_five_Dim_Array (0 .. Dim_i1, 0 .. Dim_i2, 0 .. Dim_r1,
                                            0 .. Dim_r2, 0 .. Dim_u);
      jsv_Array     : tReal_five_Dim_Array (0 .. Dim_j1, 0 .. Dim_j2, 0 .. Dim_s1,
                                            0 .. Dim_s2, 0 .. Dim_v);
      ktw_Array     : tReal_five_Dim_Array (0 .. Dim_k1, 0 .. Dim_k2, 0 .. Dim_t1,
                                            0 .. Dim_t2, 0 .. Dim_w);
      iru_Sum       : Long_Float;
      jsv_Sum       : Long_Float;
      ktw_Sum       : Long_Float;
   begin
   --   Put_Line ("Electron_Repulsion_Integral.Form_PGBF_Repulsion_Integral, Normalization: "
   --           & float'image(Normalization));
      AB_Sq :=  Long_Float(Pos_B.x - Pos_A.x)**2 + Long_Float(Pos_B.y - Pos_A.y)**2
              + Long_Float(Pos_B.z - Pos_A.z) ** 2;
      CD_Sq :=  Long_Float(Pos_D.x - Pos_C.x)**2 + Long_Float(Pos_D.y - Pos_C.y)**2
              + Long_Float(Pos_D.z - Pos_C.z) ** 2;
      Pos_P := (Exp_1 * Pos_A + Exp_2 * Pos_B) / gamma_1;
      Pos_Q := (Exp_3 * Pos_C + Exp_4 * Pos_D) / gamma_2;
      PQ_Sq :=  Long_Float(Pos_Q.x - Pos_P.x)**2 + Long_Float(Pos_Q.y - Pos_P.y)**2
              + Long_Float(Pos_Q.z - Pos_P.z) ** 2;
      iru_Array := Form_B_Array (Order_1.x_Power, Order_2.x_Power,
                                 Order_3.x_Power, Order_4.x_Power,
                                 Pos_A.x, Pos_B.x, Pos_C.x, Pos_D.x,
                                 Pos_P.x, Pos_Q.x, gamma_1, gamma_2);
      jsv_Array := Form_B_Array (Order_1.y_Power, Order_2.y_Power,
                                 Order_3.y_Power, Order_4.y_Power,
                                 Pos_A.y, Pos_B.y, Pos_C.y, Pos_D.y,
                                 Pos_C.y, Pos_D.y, gamma_1, gamma_2);
      ktw_Array := Form_B_Array (Order_1.z_Power, Order_2.z_Power,
                                 Order_3.z_Power, Order_4.z_Power,
                                 Pos_A.z, Pos_B.z, Pos_C.z, Pos_D.z,
                                 Pos_C.z, Pos_D.z, gamma_1, gamma_2);
      iru_Sum := 0.0;
      For i1 in 0.. Dim_i1 loop
      For i2 in 0.. Dim_i2 loop
         For r1 in 0 .. Natural(Floor(float(i1/2))) loop
         For r2 in 0 .. Natural(Floor(float(i2/2))) loop
            For u in 0 .. Natural(Floor(float((i1+i2)/2-r1-r2))) loop
               jsv_Sum := 0.0;
               For j1 in 0.. Dim_j1 loop
               For j2 in 0.. Dim_j2 loop
                  For s1 in 0 .. Natural(Floor(float(j1/2))) loop
                  For s2 in 0 .. Natural(Floor(float(j2/2))) loop
                     For v in 0 .. Natural(Floor(float((j1+j2)/2-s1-s2))) loop
                       ktw_Sum := 0.0;
                       For k1 in 0.. Dim_k1 loop
                       For k2 in 0.. Dim_k2 loop
                          For t1 in 0 .. Natural(Floor(float(k1/2))) loop
                          For t2 in 0 .. Natural(Floor(float(k2/2))) loop
                              For w in 0 .. Natural
                                 (Floor (float ((k1 + k2) / 2 - t1 - t2))) loop
                                ktw_Sum := ktw_Sum +
                                    ktw_Array (k1, k2, t1, t2, w)
                                  * fGamma (i1 + i2 + j1 + j2 + k1 + k2
                                     - 2 * (r1+r2 + s1+s2 + t1+t2) -u - v - w,
                                            PQ_Sq / Four_delta);

                                 If i1 + i2 + j1 + j2 + k1 + k2
                                    - 2 * (r1 + r2 + s1 + s2 + t1 + t2)
                                    -u - v - w < 0 then
               Put_Line("Form_PGBF_Repulsion_Integral, index sum less than zero!");
               Put ("i1, i2, j1, j2, k1, k2, u ,v, w: ");
               Put (integer'image (i1) & "  " & integer'image (i2));
               Put("  " & integer'image(j1) & "  "& integer'image(j2));
               Put("  " & integer'image(k1) & "  "& integer'image(k2));
               Put ("  " & integer'image (u) & "  " & integer'image (v));
               Put_Line ("  " & integer'image (w));
                                 end if;

                              end loop; --w
                           end loop; -- t2
                           end loop; -- t1
                        end loop; -- k2
                        end loop; -- k1
                        jsv_Sum := jsv_Sum + jsv_Array (j1, j2, s1, s2, v)*ktw_Sum;
                      end loop; -- v
                   end loop; -- s2
                   end loop; -- s1
               end loop; -- j2
               end loop; -- j1
               iru_Sum := iru_Sum + iru_Array (i1, i2, r1, r2, u)*jsv_Sum;
            end loop; -- u
         end loop; -- r2
         end loop; -- r1
      end loop; -- i2
      end loop; -- i1
      Normalization := Long_Float (PGBF_A.Normalization * PGBF_B.Normalization
                                   * PGBF_C.Normalization * PGBF_D.Normalization);
      -- Taketa, Huzinaga and O-ohata Equation (2-22) normalized
      return Normalization * 2.0 * (Pi ** 2) / (gamma_1 * gamma_2)
        * SqRt (Pi / (gamma_1 + gamma_2))
        * Exp (-Exp_1 * Exp_2 * AB_Sq / gamma_1 - Exp_3 * Exp_4 * CD_Sq / gamma_2)
        * iru_Sum;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in " &
                   "Electron_Repulsion_Integral.Form_PGBF_Repulsion_Integral.");
        Put_Line (Exception_Information (anError));
        return 0.0;
   end Form_Normalized_PGBF_Repulsion_Integral;

-- -----------------------------------------------------------------------

   function Form_Two_Electron_CGBF_Integral (CGBF_A : tCGBF_Orbital;
                                             CGBF_B : tCGBF_Orbital;
                                             CGBF_C : tCGBF_Orbital;
                                             CGBF_D : tCGBF_Orbital)
                                             return Long_Float is
   use Types;
   use Contraction_Vector;
      -- <CGBF_A, CGBF_B|Op|CGBF_C, CGBF_D> CGBF_Integral
      -- Integral = Sum (CGBF_A.PGBF (i))Sum (CGBF_B.PGBF (j))
      --            Sum (CGBF_C.PGBF (k))Sum (CGBF_D.PGBF (l))
      Contraction_Set_A : tPGBF_Contraction_Set := CGBF_A.Contraction_Set;
      Contraction_Set_B : tPGBF_Contraction_Set := CGBF_B.Contraction_Set;
      Contraction_Set_C : tPGBF_Contraction_Set := CGBF_C.Contraction_Set;
      Contraction_Set_D : tPGBF_Contraction_Set := CGBF_D.Contraction_Set;
      PGBF_Data_A       : tPGBF_Data;
      PGBF_Data_B       : tPGBF_Data;
      PGBF_Data_C       : tPGBF_Data;
      PGBF_Data_D       : tPGBF_Data;
      CGBF_integral     : Long_Float := 0.0;

      Procedure Do_CGBF_A (CGBF_A_Index : in Cursor)is
         -- CGBF_Set_A iteration
         Procedure Do_CGBF_B (CGBF_B_Index : in Cursor)is
            -- CGBF_Set_B iteration
            Procedure Do_CGBF_C (CGBF_C_Index : in Cursor)is
               -- CGBF_Set_C iteration
               Procedure Do_CGBF_D (CGBF_D_Index : in Cursor)is
                  -- CGBF_Set_D iteration
               begin
                  PGBF_Data_D.PGBF := Element(CGBF_D_Index);
                  -- Form_PGBF_Repulsion_Integral is equivalent to
                  -- Szabo and Ostlund's TWOE function (page 423)
                  -- of un-normalized wave-functions.
                  CGBF_integral := CGBF_integral + PGBF_Data_A.PGBF.Coeff *
                                                   PGBF_Data_B.PGBF.Coeff *
                                                   PGBF_Data_C.PGBF.Coeff *
                                                   PGBF_Data_D.PGBF.Coeff *
                                        Form_Normalized_PGBF_Repulsion_Integral
                                                  (PGBF_Data_A, PGBF_Data_B,
                                                   PGBF_Data_C, PGBF_Data_D);
               end Do_CGBF_D;
            begin  --  Do_CGBF_C
               PGBF_Data_C.PGBF := Element (CGBF_C_Index);
               Iterate (Contraction_Set_D, Do_CGBF_D'access);
            end Do_CGBF_C;
         begin  --  Do_CGBF_B
            PGBF_Data_B.PGBF := Element(CGBF_B_Index);
            Iterate (Contraction_Set_C, Do_CGBF_C'access);
         end Do_CGBF_B;
      begin  --  Do_CGBF_A
         PGBF_Data_A.PGBF := Element(CGBF_A_Index);
         Iterate (Contraction_Set_B, Do_CGBF_B'access);
      end Do_CGBF_A;
   begin
      -- Initialize_PGBF_Data sets Charge, Position and Order
      -- for each PGBF_Data leaving PGBF to be set for each iteration
      -- over each contraction set (CGBF).
      Initialize_PGBF_Data (CGBF_A, PGBF_Data_A);
      Initialize_PGBF_Data (CGBF_B, PGBF_Data_B);
      Initialize_PGBF_Data (CGBF_C, PGBF_Data_C);
      Initialize_PGBF_Data (CGBF_D, PGBF_Data_D);
      Iterate (Contraction_Set_A, Do_CGBF_A'access);
      return CGBF_integral;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in " &
                   "Electron_Repulsion_Integral.Form_Two_Electron_CGBF_Integral.");
        Put_Line (Exception_Information (anError));
        return 0.0;

   end Form_Two_Electron_CGBF_Integral;
End Electron_Repulsion_Integral;
