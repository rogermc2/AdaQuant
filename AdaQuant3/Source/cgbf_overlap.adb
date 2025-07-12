    Function CGBF_Overlap (CGBF_1 : in tCGBF_Orbital;
                          CGBF_2 : in tCGBF_Orbital) return float;

Function CGBF_Overlap (CGBF_1 : in tCGBF_Orbital;
                          CGBF_2 : in tCGBF_Orbital) return float is
      PGBF_Data_1 : tPGBF_Data;
      PGBF_Data_2 : tPGBF_Data;
      integral    : float := 0.0;
      Procedure Do_CGBF_1 (Index_1 : in Contraction_Vector.Cursor) is
         Procedure Do_CGBF_2 (Index_2 : in Contraction_Vector.Cursor) is
         use Contraction_Vector;
         begin
            PGBF_Data_2.PGBF := Contraction_Vector.Element(Index_2);
            integral := integral +
               PGBF_Data_1.PGBF.Coefficient * PGBF_Data_2.PGBF.Coefficient *
               Form_PGBF_Overlap_Integral (PGBF_Data_1, PGBF_Data_2);
         end Do_CGBF_2;
      begin
         PGBF_Data_1.PGBF := Contraction_Vector.Element(Index_1);
         Iterate(CGBF_2.Contraction_Set, Do_CGBF_2'Access);
      end Do_CGBF_1;
   begin
      Initialize_PGBF_Data (CGBF_1, PGBF_Data_1);
      Initialize_PGBF_Data (CGBF_2, PGBF_Data_2);
      Iterate (CGBF_1.Contraction_Set, Do_CGBF_1'Access);
      return integral;
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Integrals.CGBF_Overlap!");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end CGBF_Overlap;

-- ------------------------------------------------------------
