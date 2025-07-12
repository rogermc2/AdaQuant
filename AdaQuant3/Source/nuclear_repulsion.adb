with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Types; use Types;

with AdaQuant_Types; use AdaQuant_Types;
with Cartesian;
with Elements;

Package body Nuclear_Repulsion is

   Function Nuclear_Repulsion_Energy (Molecule : in tMolecule) return long_float is
      Procedure Do_Atom (Index_1 : in Atom_List_Package.Cursor);
      theAtom_List : tAtom_List := Atom_List (Molecule);
      sum          : long_float := 0.0;
      Procedure Do_Atom (Index_1 : in Atom_List_Package.Cursor) is
         use Atom_List_Package;
         use Elements;
         use Cartesian;
         Index_2   : Atom_List_Package.Cursor;
         Element_1 : tElement;
         Element_2 : tElement;
         Pos_1     : tCartesian_Coordinates;
         Pos_2     : tCartesian_Coordinates;
         Dist_12   : long_float;
      begin
         Element_1 := Atom_List_Package.Element (Index_1).Symbol;
         Pos_1 := Atom_List_Package.Element (Index_1).Position;
         Index_2 := Index_1;
         while Index_2 /= Last (theAtom_List) loop
            Index_2 := Next (Index_2);
            Element_2 := Atom_List_Package.Element (Index_2).Symbol;
            Pos_2 := Atom_List_Package.Element (Index_2).Position;
            Dist_12 := long_float (Distance(Pos_1, Pos_2));
            Sum := Sum + long_float (Element_Data (Element_1).Z
                              * Element_Data (Element_2).Z)
                              / Dist_12;
         end loop;
      end Do_Atom;
   begin
      Iterate(theAtom_List, Do_Atom'access);
      return sum;
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Nuclear_Repulsion_Energy!");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end Nuclear_Repulsion_Energy;

-- --------------------------------------------------------------------------

end Nuclear_Repulsion;
