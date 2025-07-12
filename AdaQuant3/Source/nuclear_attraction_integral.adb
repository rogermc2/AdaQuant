with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Exceptions; use Ada.Exceptions;

with Types;
with Shell_Basis_Set; use Shell_Basis_Set;

with Adaquant_Types; use Adaquant_Types;
with Elements;
with Maths; use Maths;
with Gamma;

--    From Gaussian Expansion Methods for Molecular Orbitals by Taketa,
--    Huzinaga, and O-ohata. H. Phys. Soc. Japan, 21, 2313, 1966.

Package body Nuclear_Attraction_Integral is
   use tFloat_Elementary_Functions;

   Function Form_A_Array (Order_1 : in Natural; Order_2 : in Natural;
                          Dist_PA : in pFloat;  Dist_PB : in pFloat;
                          Dist_PC : in pFloat;  gamma   : in long_float)
                          return tReal_Cubic_Array;
   Function JSV_Sum (PC_Sq     : in long_float; Dim_j, Dim_k : in Natural;
                     i, r, u   : in Natural;
                     jsv_Array : in tReal_Cubic_Array;
                     ktw_Array : in tReal_Cubic_Array;
                     gamma     : in long_float)
                     return long_float;
   Function KTW_Sum (PC_Sq : in long_float; Dim_k : in Natural;
                     i, j, r, s, u, v : in Natural;
                     ktw_Array        : in tReal_Cubic_Array;
                     alpha12          : in long_float) return long_float;
   Function Form_Nuclear_Attraction_Component
                        (PGBF_Data_1 : in tPGBF_Data;
                         PGBF_Data_2 : in tPGBF_Data;
                         Pos_3       : in tCartesian_Coordinates)
                         return long_float;

-- -----------------------------------------------------------------

--    From Gaussian Expansion Methods for Molecular Orbitals by Taketa,
--    Huzinaga, and O-ohata equation (2.18)
   Function Form_A_Array (Order_1 : in Natural; Order_2 : in Natural;
                          Dist_PA : in pFloat;  Dist_PB : in pFloat;
                          Dist_PC : in pFloat;  gamma   : in long_float)
                          return tReal_Cubic_Array is
      Dim_i   : Natural := Order_1 + Order_2;
      Dim_ru  : Natural := Natural(float'Floor(float(Dim_i/2)));
      A_Array : tReal_Cubic_Array (0 .. Dim_i, 0 .. Dim_ru, 0 .. Dim_ru)
                             := (others => (others => (others => 0.0)));
      i_Term  : long_float;
      r_Fact  : long_float;
   begin
      For i in 0.. Dim_i loop
         i_Term := long_float(Factorial(i)) *
           Dual_Binomial (long_float (Dist_PA), long_float (Dist_PB),
                          Order_1, Order_2, i);
         declare
            rmax : Natural := Natural (float'Floor (float (i) / 2.0));
         begin
            For r in 0 .. rmax loop
               r_Fact := long_float(Factorial(r));
            declare
               umax : Natural := Natural (float'Floor (float (i-2*r) / 2.0));
            begin
               For u in 0.. umax loop
                  A_Array (i, r, u) := (-1.0) ** (i + u) * i_Term *
                            long_float(Dist_PC) ** (i - 2 * (r + u)) /
                            (r_Fact * long_float (Factorial (u)) *
                            long_float (Factorial (i - 2 * (r + u))) *
                             (4.0 * gamma) ** (r + u));
               end loop;
            end; -- declare
            end loop;
         end; -- declare
      end loop;

      return A_Array;
   end Form_A_Array;

-- -----------------------------------------------------------------

   Function JSV_Sum (PC_Sq     : in long_float; Dim_j, Dim_k : in Natural;
                     i, r, u   : in Natural;
                     jsv_Array : in tReal_Cubic_Array;
                     ktw_Array : in tReal_Cubic_Array;
                     gamma     : in long_float)
                     return long_float is
      jj : Natural;
      ss : Natural;
      vv : Natural;
      sum : long_float := 0.0;
   begin
      For j in 0 .. Dim_j loop
         jj := j;
         For s in 0 .. Natural(Floor(float(j/2))) loop
            ss := s;
            For v in 0 .. Natural(Floor(float(j/2 - s))) loop
               vv := v;
               sum := sum + jsv_Array (j, s, v)
                          * KTW_Sum (PC_Sq, Dim_k, i, j, r, s, u, v,
                                     ktw_Array, gamma);
            end loop;
         end loop;
      end loop;
      return sum;
  exception
      when anError :  Constraint_Error =>
         Put_Line ("An exception occurred in Nuclear_Attraction_Integral.JSV_Sum.");
         Put_Line ("i, j, Dim_j, Dim k: " & Integer'image (i)
             & Integer'image (jj) & Integer'image (Dim_j) & Integer'image (Dim_k));
         Put_Line ("r, s: " & Integer'image (r) & Integer'image (ss) );
         Put_Line ("u, v: " & Integer'image (u) & Integer'image (vv));
         Put_Line (Exception_Information (anError));
         return 0.0;
      when anError :  others =>
         Put_Line ("An exception occurred in Nuclear_Attraction_Integral.JSV_Sum.");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end JSV_Sum;

-- -----------------------------------------------------------------

   Function KTW_Sum (PC_Sq : in long_float; Dim_k : in Natural;
                     i, j, r, s, u, v : in Natural;
                     ktw_Array        : in tReal_Cubic_Array;
                     alpha12          : in long_float) return long_float is
   use Gamma;
      kk           : Natural;
      tt           : Natural;
      ww           : Natural;
      Level        : Integer;
      Continue     : Boolean := True;
      Show_Message : Boolean := True;
      sum          : long_float := 0.0;
   begin
       For k in 0 .. Dim_k loop
          kk := k;
          For t in 0 .. Natural(Floor(float(k/2))) loop
             tt := t;
             For w in 0 .. Natural(Floor(float(k/2 - t))) loop
                ww := w;
                Level :=  i + j + k - 2 * (r + s + t) -u - v - w;
                Continue := Level >= 0;
                If Continue then
                   sum := sum + ktw_Array (k, t, w) *
                                fGamma (Level, alpha12 * PC_Sq);
   --      Put_Line ("Nuclear_Attraction_Integral.KTW_Sum, Level, PC_Sq, fGamma: ");
    --     Put_Line (Integer'image (Level) & ",  " & float'image (PC_Sq) & ",  "
   --              & float'image (fGamma (Level, alpha12 * PC_Sq)));
                elsif Show_Message then
                   Put_Line ("Nuclear_Attraction_Integral.KTW_Sum");
                   Put_Line ("Invalid Fgamma index: " & Integer'image (Level));
                   Put_Line ("i, j, k: " & Integer'image (i)
                           & Integer'image (j) & Integer'image (k));
                   Put_Line ("r, s, t: " & Integer'image (r)
                           & Integer'image (s) & Integer'image (t));
                   Put_Line ("u, v, w: " & Integer'image (u)
                           & Integer'image (v) & Integer'image (w));
                   Show_Message := False;
                end if;
             end loop;
          end loop;
      end loop;
      return sum;
   exception
      when anError :  Constraint_Error =>
         Put_Line ("An exception occurred in Nuclear_Attraction_Integral.KTW_Sum.");
         Put_Line ("i, j, k, Dim k: " & Integer'image (i)
                 & Integer'image (j) & Integer'image (kk) & Integer'image (Dim_k));
         Put_Line ("r, s, t: " & Integer'image (r) & Integer'image (s)
                 & Integer'image (tt));
         Put_Line ("u, v, w: " & Integer'image (u) & Integer'image (v)
                 & Integer'image (ww));
         Put_Line (Exception_Information (anError));
         return 0.0;
      when anError :  others =>
         Put_Line ("An exception occurred in Nuclear_Attraction_Integral.KTW_Sum.");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end KTW_Sum;

-- -----------------------------------------------------------------

Function Form_Nuclear_Attraction_Integral
                        (PGBF_Data_1 : in tPGBF_Data;
                         PGBF_Data_2 : in tPGBF_Data;
                         Molecule    : in tMolecule)
                         return long_float is
      use Types;
      use Elements;
      procedure Integrate (Atom_Index : in Atom_List_Package.Cursor);
      The_Atoms     : tAtom_List := Atom_List(Molecule);
      integral      : long_float := 0.0;
      Normalization : long_float;
      procedure Integrate (Atom_Index : in Atom_List_Package.Cursor) is
         Atom   : tAtom  := Atom_List_Package.Element (Atom_Index);
         Charge : long_float := long_float (Element_Data (Atom.Symbol).Z);
         PGBF_Integral : long_float;
      begin
         PGBF_Integral := Charge * Form_Nuclear_Attraction_Component
                            (PGBF_Data_1, PGBF_Data_2, Atom.Position);
         integral := integral - PGBF_Integral;
      exception
        when anError :  others =>
          Put_Line ("An exception occurred in Nuclear_Attraction_Integral." &
                    "Form_Nuclear_Attraction_Integral.Integrate.");
          Put_Line (Exception_Information (anError));
      end Integrate;
   begin
      Normalization := long_float (PGBF_Data_1.PGBF.Normalization
                                 * PGBF_Data_2.PGBF.Normalization);
      Iterate (The_Atoms, Integrate'access);
      return Normalization*integral;
   exception
      when anError :  others =>
         Put_Line ("An exception occurred in " &
             "Nuclear_Attraction_Integral.Form_Nuclear_Attraction_Integral.");
        Put_Line (Exception_Information (anError));
        return 0.0;
   end Form_Nuclear_Attraction_Integral;

-- -----------------------------------------------------------------

   Function Form_Nuclear_Attraction_Component
                        (PGBF_Data_1 : in tPGBF_Data;
                         PGBF_Data_2 : in tPGBF_Data;
                         Pos_3  : in tCartesian_Coordinates) return long_float is
      PGBF_1  : tPGBF := PGBF_Data_1.PGBF;
      PGBF_2  : tPGBF := PGBF_Data_2.PGBF;
      Pos_1   : tCartesian_Coordinates := PGBF_Data_1.Position;
      Pos_2   : tCartesian_Coordinates := PGBF_Data_2.Position;
      Pos_P   : tCartesian_Coordinates;
      Exp_1   : long_float := long_float(PGBF_1.Exponent);
      Exp_2   : long_float := long_float (PGBF_2.Exponent);
      Order_1 : tPower := PGBF_Data_1.Order;
      Order_2 : tPower := PGBF_Data_2.Order;
      gamma   : long_float := Exp_1 + Exp_2;
      AB_Sq   : long_float;
      PC_Sq   : long_float;
      Dim_i   : Natural := Order_1.x_Power + Order_2.x_Power;
      Dim_j   : Natural := Order_1.y_Power + Order_2.y_Power;
      Dim_k   : Natural := Order_1.z_Power + Order_2.z_Power;
      Dim_ru  : Natural := Natural(Floor(float(Dim_i/2)));
      Dim_sv  : Natural := Natural(Floor(float(Dim_j/2)));
      Dim_tw  : Natural := Natural(Floor(float(Dim_k/2)));
      ii      : Natural;
      rr      : Natural;
      uu      : Natural;
      iru_Array : tReal_Cubic_Array (0 .. Dim_i, 0 .. Dim_ru, 0 .. Dim_ru);
      jsv_Array : tReal_Cubic_Array (0 .. Dim_j, 0 .. Dim_sv, 0 .. Dim_sv);
      ktw_Array : tReal_Cubic_Array (0 .. Dim_k, 0 .. Dim_tw, 0 .. Dim_tw);
      iru_Sum   : long_float := 0.0;
      Dist_PA   : tCartesian_Coordinates;
      Dist_PB   : tCartesian_Coordinates;
      Dist_PC   : tCartesian_Coordinates;
   begin
      AB_Sq :=  long_float(Pos_2.x - Pos_1.x)**2 + long_float(Pos_2.y - Pos_1.y)**2
              + long_float(Pos_2.z - Pos_1.z) ** 2;
      Pos_P := (Exp_1 * Pos_1 + Exp_2 * Pos_2) / gamma;
      Dist_PA := Pos_P - Pos_1;
      Dist_PB := Pos_P - Pos_2;
      Dist_PC := Pos_P - Pos_3;
      PC_Sq :=  long_float(Pos_3.x - Pos_P.x)**2 + long_float(Pos_3.y - Pos_P.y)**2
        + long_float (Pos_3.z - Pos_P.z) ** 2;
      iru_Array := Form_A_Array (Order_1.x_Power, Order_2.x_Power,
                                 Dist_PA.x, Dist_PB.x, Dist_PC.x, gamma);
      jsv_Array := Form_A_Array (Order_1.y_Power, Order_2.y_Power,
                                 Dist_PA.y, Dist_PB.y, Dist_PC.y, gamma);
      ktw_Array := Form_A_Array (Order_1.z_Power, Order_2.z_Power,
                                 Dist_PA.z, Dist_PB.z, Dist_PC.z, gamma);
      For i in 0 .. Dim_i loop
         ii := i;
         For r in 0 .. Natural(Floor(float(i/2))) loop
            rr := r;
            For u in 0 .. Natural(Floor(float(i/2 - r))) loop
               uu := u;
               iru_Sum := iru_Sum + iru_Array (i, r, u)
                                  * JSV_Sum (PC_Sq, Dim_j, Dim_k,
                                             i, r, u, jsv_Array, ktw_Array, gamma);
            end loop;
         end loop;
      end loop;

      return (2.0 * Pi) / gamma * Exp (-Exp_1 * Exp_2 * AB_Sq / gamma)
        * iru_Sum;
   exception
      when anError :  Constraint_Error =>
         Put_Line ("An exception occurred in Nuclear_Attraction_Integral." &
                   "Form_Nuclear_Attraction_Component.");
         Put_Line ("i, Dim i, Dim j, Dim k: " & Integer'image (ii)
                 & Integer'image (Dim_i) & Integer'image (Dim_j)
                 & Integer'image (Dim_k));
         Put_Line ("r, Dim r, Dim s, Dim t: " & Integer'image (rr)
                 & Integer'image (Dim_ru) & Integer'image (Dim_sv)
                 & Integer'image (Dim_tw));
         Put_Line ("u, Dim u, Dim v, Dim w: " & Integer'image (uu)
                 & Integer'image (Dim_ru) & Integer'image (Dim_sv)
                 & Integer'image (Dim_tw));
         Put_Line (Exception_Information (anError));
         return 0.0;
      when anError :  others =>
         Put_Line ("An exception occurred in Nuclear_Attraction_Integral." &
                   "Form_Nuclear_Attraction_Component.");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end Form_Nuclear_Attraction_Component;

End Nuclear_Attraction_Integral;
