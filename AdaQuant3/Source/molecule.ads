with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers;

with Types; use Types;
with AdaQuant_Types; use AdaQuant_Types;
with Basis_Set; use Basis_Set;
with Shell_Basis_Set; use Shell_Basis_Set;
with Maths; use Maths;
with Cartesian; use Cartesian;

Package Molecule is
   package Atom_List_Package is
     new Doubly_Linked_Lists (tAtom);
   Type tAtom_List is new Atom_List_Package.List with Null record;
   Type tMolecule is private;
   Type tPowers_Map is private;
   Type tCGBF_Orbital is record
      Nuclear_Charge  : tNuclear_Charge;
      Position        : TCartesian_Coordinates;
      Order           : tPower;
      Contraction_Set : tPGBF_Contraction_Set;
   end record;
   Package CGBF_Orbital_List is
     new Doubly_Linked_Lists (tCGBF_Orbital);
   Type tCGBF_Orbital_Set is new CGBF_Orbital_List.List with null record;

   procedure Define_Molecule (Name             : in Unbounded_String;
                              Atoms            : in tAtom_List;
                              Units            : in tUnits;
                              Charge           : in tCharge;
                              Multiplicity     : in tMultiplicity;
                              Molecule         : out tMolecule;
                              Error            : out tSource_Data_Error);

   Procedure Form_CGBF_Orbitals (Basis_Set        : in tBasis_Set_Map;
                                 Molecule         : in tMolecule;
                                 CGBF_Orbitals    : out tCGBF_Orbital_Set;
                                 Error            : out tSource_Data_Error);
   function Null_Molecule return tMolecule;
   function Atom_List (Molecule : in tMolecule)
                       return tAtom_List;
   procedure Make_Atom_List (Descriptor_File : in tFile_Type;
                             Atom_List       : out tAtom_List;
                             Error           : out tSource_Data_Error);
   function get_Contraction_Set(CGBF : in tCGBF_Orbital)
                                return tPGBF_Contraction_Set;

private

   Type tMolecule is record
       Name                : Unbounded_String;
       Atom_List           : tAtom_List;
       Units               : tUnits;
       Charge              : tCharge;
       Multiplicity        : tMultiplicity;
       Number_Of_Electrons : tCharge := 0;
   end record;

  package Powers_List_Package is
                  new Doubly_Linked_Lists (tPower);
   Type tPowers_List is new Powers_List_Package.List with Null record;

  Package Powers_Map_Package is
     new Ada.Containers.Ordered_Maps
         (Key_Type => tElectron_Shell, Element_Type => tPowers_List);
  Type tPowers_Map is new Powers_Map_Package.Map with null record;

end Molecule;
