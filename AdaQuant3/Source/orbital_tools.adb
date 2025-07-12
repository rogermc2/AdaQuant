with Molecule; use Molecule;
with Shell_Basis_Set; use Shell_Basis_Set;

Package body Orbital_Tools is

   function Orbital_Overlap (Orbital_1  : in tPGBF_Data;
                             Orbital_2  : in tPGBF_Data)
                              return float;

-- --------------------------------------------------------------------

   function Normalize(Orbital : in tPGBF_Data)
                      return float is
      overlap  : float;
      Result   : float;
   begin
      overlap := Orbital_Overlap (Orbital, Orbital);
      return Result;
   end Normalize;

-- --------------------------------------------------------------------

   function Orbital_Overlap (Orbital_1  : in tPGBF_Data;
                             Orbital_2  : in tPGBF_Data)
                             return float is
      overlap  : float := 0.0;
      PGBF_1   : tPGBF := Orbital_1.PGBF;
      PGBF_2   : tPGBF := Orbital_2.PGBF;
   begin
      overlap := PGBF_1.Coefficient * PGBF_2.Coefficient
               * Form_Overlap_Integral(Orbital_1, Orbital_2);

      return overlap;  -- un-normalized
   end Orbital_Overlap;

end Orbital_Tools;
