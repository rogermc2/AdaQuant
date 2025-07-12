with Ada.Text_IO; use Ada.Text_IO;

with Types; use Types;

with Cartesian; use Cartesian;

Package AdaQuant_Types is
   Type tUnits is (Bohr, Angstrom);
   Type tCharge is new Integer;
   Type tNuclear_Charge is new Positive;
   Type tMultiplicity is new Integer;
   Type tCalculation_Method is (Undefined, RHF, UHF, DFT, MINDO3, UMINDO3);
   Type tOrthog_Method is (Canonical, Symmetric);
   Type tMatrix_Type is (Core_Hamiltonian, Kinetic, Overlap,
                         Nuclear_Attraction, Transformation, Two_Electron);
   Type tCharge_Density is (eZero, eFock);
   Type tAverager is (eNone, eSimple_Average, eAnderson);

   Subtype tString_7 is String(1..7);
   Seven_Blanks : constant tString_7 := "       ";
   Subtype tString_13 is String(1..13);
   Thirteen_Blanks : constant tString_13 := "             ";

   Type tOptions is record
      Calculation_Method       : tCalculation_Method := RHF;
      Orthogonalization_Method : tOrthog_Method := Symmetric;
   end record;

   Type tAtom is record
      Symbol   : tElement;
      Position : tCartesian_Coordinates;
   end record;

   Package Text_Float_IO is new Ada.Text_IO.Float_IO (Float);

end AdaQuant_Types;
