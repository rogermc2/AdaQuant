
with Cartesian; use Cartesian;
with Molecule; use Molecule;
with Integrals; use Integrals;

Package Nuclear_Attraction_Integral is
   Function Form_Nuclear_Attraction_Integral
                        (PGBF_Data_1 : in tPGBF_Data;
                         PGBF_Data_2 : in tPGBF_Data;
                         Molecule    : in tMolecule)
                         return long_float;
End Nuclear_Attraction_Integral;
