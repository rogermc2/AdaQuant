with Ada.Numerics.Generic_Real_Arrays; use Ada.Numerics;

with Types; use Types;

with AdaQuant_Types; use AdaQuant_Types;
with Cartesian; use Cartesian;
with Maths; use Maths;
with Molecule; use Molecule;
with Shell_Basis_Set; use Shell_Basis_Set;

package Integrals is

   Subtype tOverlap is long_float;
   Subtype tKinetic_Energy is long_float;
   Subtype tNuclear_Attraction is long_float;
   Subtype tTwo_Electrons is long_float;

   Type tPGBF_Data is record
      Nuclear_Charge  : tNuclear_Charge;
      Position        : TCartesian_Coordinates;
      Order           : tPower;
      PGBF            : tPGBF;
   end record;

   Function Check_Normalization (PGBF : in tPGBF; Order: in tPower)
                                 return long_float;
   function Form_CGBF_Integral (Matrix_Type : in tMatrix_Type;
                                CGBF_1   : in tCGBF_Orbital;
                                CGBF_2   : in tCGBF_Orbital;
                                Molecule : in tMolecule := Null_Molecule)
                                return long_float;
   function Form_Kinetic_Energy_Integral
                          (PGBF_1 : in tPGBF_Data;
                           PGBF_2 : in tPGBF_Data)
                           return tKinetic_Energy;
   Function Form_PGBF_Overlap_Integral (PGBF_1 : in tPGBF_Data;
                                        PGBF_2 : in tPGBF_Data)
                                        return long_float;
   Function Form_PGBF_Normalized_Overlap_Integral (PGBF_1 : tPGBF_Data;
                                                   PGBF_2 : in tPGBF_Data)
                                        return long_float;
   Procedure Initialize_PGBF_Data (Orbital : in tCGBF_Orbital;
                                   PGBF    : in out tPGBF_Data);

end Integrals;
