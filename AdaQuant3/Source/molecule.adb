with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Numerics.Elementary_Functions;
  use Ada.Numerics;
  use Ada.Numerics.Elementary_Functions;
with Ada.Exceptions; use Ada.Exceptions;

with Basis_Name_Map; use Basis_Name_Map;
with Element_Shell_Symbol_Maps; use Element_Shell_Symbol_Maps;
with Element_Basis_Set; use Element_Basis_Set;

with Elements;
with Integrals;

Package body Molecule is

  sPowers_List : tPowers_List;
  pPowers_List : tPowers_List;
  dPowers_List : tPowers_List;
  fPowers_List : tPowers_List;
  Powers_Map   : tPowers_Map;

  Procedure Add_Level_Orbitals (Level_Basis_Set : in tEnergy_Level_Basis_Set;
                                Atom            : tAtom;
                                CGBF_Orbitals   : in out tCGBF_Orbital_Set);
  Procedure Add_Power_Orbitals (Contraction_Set : in tPGBF_Contraction_Set;
                                Powers_List     : in tPowers_List;
                                Atom            : tAtom;
                                CGBF_Orbitals   : in out tCGBF_Orbital_Set);
  Procedure Add_Shell_Orbitals (Shell_Basis_Set : in tShell_Basis_Set;
                               Atom            : tAtom;
                               CGBF_Orbitals   : in out tCGBF_Orbital_Set);
  Procedure Normalize_PGBF (Order : in tPower; PGBF : in out tPGBF);

-- --------------------------------------------------------------------------

   Procedure Add_Level_Orbitals (Level_Basis_Set   : in tEnergy_Level_Basis_Set;
                                Atom              : tAtom;
                                CGBF_Orbitals     : in out tCGBF_Orbital_Set) is
      Procedure Add_Shells (Index : in Element_Energy_Level.Cursor)is
         Energy_Level    : tEnergy_Level;
         Shell_Basis_Set : tShell_Basis_Set;
      begin
         Energy_Level := Element_Energy_Level.Key (Index);
--         Put_Line ("Molecule.Add_Level_Orbitals.Add_Shell_Orbitals, Level: "
--                    & tEnergy_Level'image(Energy_Level));
         Shell_Basis_Set := Level_Basis_Set.Element (Energy_Level);
         Add_Shell_Orbitals(Shell_Basis_Set, Atom, CGBF_Orbitals);
       end Add_Shells;

   begin
      Iterate (Level_Basis_Set, Add_Shells'access);
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Molecule.Add_Level_Orbitals!");
         Put_Line(Exception_Information(anError));
   end Add_Level_Orbitals;

-- --------------------------------------------------------------------------

   Procedure Add_Power_Orbitals (Contraction_Set   : in tPGBF_Contraction_Set;
                                Powers_List       : in tPowers_List;
                                Atom              : tAtom;
                                CGBF_Orbitals     : in out tCGBF_Orbital_Set) is
      -- The contraction set is provided by the basis set manager and
      -- conisists of a list of exponent, coefficient pairs
      -- each pair describing one PGBF.
      Procedure Add_Orbital_To_Molecule (Index : in Powers_List_Package.Cursor)is
         Orbital      : tCGBF_Orbital;
         Contractions : tPGBF_Contraction_Set := Contraction_Set;
         Procedure Normalize (Index : in Contraction_Vector.Cursor) is
         use integrals;
            PGBF : tPGBF;
            Pos  : tCartesian_Coordinates := Atom.Position;
            check : long_float;
         begin
            PGBF := Contraction_Vector.Element (Index);
            Normalize_PGBF (Orbital.Order, PGBF);
            check := Integrals.Check_Normalization (PGBF, Orbital.Order);
            If Abs(check) > 1.0+1.0E-12 then
               Put_Line ("Molecule.Add_Power_Orbitals, Normalization check: "
                        & long_float'image (check));
            End if;
            Replace_Element (Contractions, Index, PGBF);
          --  Put_Line (float'image (PGBF.Exponent) & ", " & float'image (PGBF.Coeff)
          --            & ", " & tNormalization'image (PGBF.Normalization));
         exception
            when  anError : others =>
               Put_Line ("An exception occurred in "
                       & "Molecule.Add_Power_Orbitals.Normalize!");
               Put_Line(Exception_Information(anError));
         end Normalize;

      begin
         Orbital.Nuclear_Charge := Elements.Element_Data(Atom.Symbol).Z;
         Orbital.Position := Atom.Position;
         Orbital.Order := Powers_List_Package.Element (Index);
        -- Put_Line ("Molecule.Add_Power_Orbitals.Add_Orbital_To_Molecule, x:"
         --         & Integer'image(Orbital.Order.x_Power) & "  y:"
         --         & Integer'image(Orbital.Order.y_Power) & "  z:"
         --         & Integer'image(Orbital.Order.z_Power));
         -- Normalize each atomic orbital of the contraction set.
         Contraction_Vector.Iterate (Contraction_Vector.Vector (Contractions),
                                     Normalize'access);
         Orbital.Contraction_Set := Contractions;
         CGBF_Orbitals.Append (Orbital);
      end Add_Orbital_To_Molecule;

   begin
     --  Put_Line("Add_Power_Orbitals.Add_Orbital_To_Molecule, PGBF Exp, Coeff and Norm: ");
       Iterate (Powers_List, Add_Orbital_To_Molecule'access);
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Molecule.Add_Power_Orbitals!");
         Put_Line(Exception_Information(anError));
   end Add_Power_Orbitals;

-- --------------------------------------------------------------------------

   Procedure Add_Shell_Orbitals (Shell_Basis_Set   : in tShell_Basis_Set;
                                Atom              : tAtom;
                                CGBF_Orbitals : in out tCGBF_Orbital_Set) is
      Procedure Add_Shell_Basis_Set (Index : in Shell_Basis_Set_Map.Cursor)is
         Shell_Symbol    : tElectron_Shell;
         Contraction_Set : tPGBF_Contraction_Set;
         Powers_List     : tPowers_List;
      begin
        Shell_Symbol := Shell_Basis_Set_Map.Key(Index);
        --Put_Line ("Molecule.Add_Shell_Orbitals.Add_Shell_Basis_Set, Shell_Symbol: "
        --        & tElectron_Shell'image(Shell_Symbol));
        Contraction_Set := Shell_Basis_Set.Element (Shell_Symbol);
      -- The Contraction_Set consists of a list of exponent, coefficient pairs
      -- each pair describing one PGBF.
        Powers_List := Powers_Map.Element (Shell_Symbol);
        Add_Power_Orbitals(Contraction_Set, Powers_List, Atom, CGBF_Orbitals);
      end Add_Shell_Basis_Set;

   begin
       Iterate (Shell_Basis_Set, Add_Shell_Basis_Set'access);
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Molecule.Add_Shell_Orbitals!");
         Put_Line(Exception_Information(anError));
   end Add_Shell_Orbitals;

-- --------------------------------------------------------------------------

   function Atom_List (Molecule : in tMolecule)
                       return tAtom_List is
  begin
     return Molecule.Atom_List;
  end Atom_List;

-- --------------------------------------------------------------------------

procedure Define_Molecule (Name            : in Unbounded_String;
                           Atoms            : in tAtom_List;
                           Units            : in tUnits;
                           Charge           : in tCharge;
                           Multiplicity     : in tMultiplicity;
                           Molecule         : out tMolecule;
                           Error            : out tSource_Data_Error) is
      Electrons    : tCharge := 0;

      Procedure Count_Electrons (Position : in Atom_List_Package.Cursor)is
      use Elements;
         Element_Symbol  : tElement;
      begin
         if Error = no_Error then
           Element_Symbol := Atom_List_Package.Element(Position).Symbol;
--      Put_Line ("Molecule.Define_Molecule.Count_Electrons, Element_Symbol: "
--               & tElement'image(Element_Symbol));
           if Element_Data_Contains (Element_Symbol) then
               Electrons := Electrons +
                            tCharge (Elements.Element_Data (Element_Symbol).Z);
           else
             Error := eElement_Symbol_Error;
               Put_line ("Molecule.Define_Molecule,"
                       & "the element data map does not contain "
                       & tElement'image(Element_Symbol));
            end if;
         end if;
      end Count_Electrons;

   begin
      Error := no_Error;
      Iterate (Atoms, Count_Electrons'access);
      Molecule.Name := Name;
      Molecule.Atom_List := Atoms;
      Molecule.Units := Units;
      Molecule.Charge := Charge;
      Molecule.Multiplicity := Multiplicity;
      Molecule.Number_Of_Electrons := Electrons - Charge;
      Put_Line ("Molecule.Define_Molecule, Number of electrons: "
               & tCharge'image( Molecule.Number_Of_Electrons));
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Molecule.Define_Molecule!");
         Put_Line(Exception_Information(anError));
   end Define_Molecule;

-- --------------------------------------------------------------------------

   Procedure Form_CGBF_Orbitals (Basis_Set        : in tBasis_Set_Map;
                                 Molecule         : in tMolecule;
                                 CGBF_Orbitals    : out tCGBF_Orbital_Set;
                                 Error            : out tSource_Data_Error) is
      Procedure Add_Atomic_Orbitals (Index : in Atom_List_Package.Cursor)is
         Atom             : tAtom;
         Level_Basis_Set  : tEnergy_Level_Basis_Set;
      begin
         if Error = no_Error then
           Atom := Atom_List_Package.Element(Index);
           if Basis_Set.Contains (Atom.Symbol) then
               Level_Basis_Set := Basis_Set.Element (Atom.Symbol);
               Add_Level_Orbitals(Level_Basis_Set, Atom, CGBF_Orbitals);
           else
              Error := eElement_Symbol_Error;
               Put_line ("Molecule.Form_CGBF_Orbitals, "
                        & "the basis set does not contain "
                        & tElement'image(Atom.Symbol));
           end if;
         end if;
       end Add_Atomic_Orbitals;

   begin
      Error := no_Error;
      Iterate (Molecule.Atom_List, Add_Atomic_Orbitals'access);
      Put_Line ("Molecule.Form_CGBF_Orbitals, Number of CGBF orbitals: " &
                 count_type'image (CGBF_Orbitals.Length));
  exception
      when  anError : others =>
         Put_Line ("An exception occurred in Molecule.Form_CGBF_Orbitals!");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Form_CGBF_Orbitals;

-- --------------------------------------------------------------------------

   function get_Contraction_Set (CGBF  : in tCGBF_Orbital)
                                 return tPGBF_Contraction_Set is
   begin
     return CGBF.Contraction_Set;
   end get_Contraction_Set;

-- --------------------------------------------------------------------------

   procedure Make_Atom_List (Descriptor_File : in tFile_Type;
                             Atom_List       : out tAtom_List;
                             Error           : out tSource_Data_Error) is
     aLine           : Unbounded_String;
     Line_Length     : Natural;
     element_string  : String(1..2);
     Continue        : Boolean := True;
     Atom            : tAtom;
     Last            : Positive;
   begin
     Error := No_Error;
     while Continue And not End_Of_File(Descriptor_File) loop
        aLine := To_Unbounded_String (Get_Line (Descriptor_File));
        Continue := To_Upper(Slice(aLine, 1, 3)) /= "END";
        If Continue then
           element_string := To_Upper(Slice (aLine, 1, 2));
--           Put_Line("Molecule.Make_Atom_List, element_string: " & element_string);
           If Element_Symbol_Map_Contains (element_string) then
--           Put_Line("Element_string found in Element_Symbol_Map.");
              Line_Length := Length (aLine);
              Atom.Symbol := Element_Symbol (element_string);
              Text_Float_IO.Get(Slice(aLine, 16, 26), float(Atom.Position.x), Last);
              Text_Float_IO.Get(Slice (aLine, 28, 38), float(Atom.Position.y), Last);
              If Line_Length < 50 then
                 Text_Float_IO.Get(Slice (aLine, 40, Line_Length), float(Atom.Position.z), Last);
              else
                Text_Float_IO.Get(Slice (aLine, 40, 50), float(Atom.Position.z), Last);
              end if;
               Atom_List.Append (Atom);
--           else
--              Put_Line("Element_string NOT found in Element_Symbol_Map.");
           end if;
       end if;
      end loop;

   exception
     when  anError : others =>
       Put_Line ("An exception occurred in Make_Atom_List!");
       Put_Line(Exception_Information(anError));
       Error := eError;

   end Make_Atom_List;

-- --------------------------------------------------------------------------

   Procedure Normalize_PGBF (Order : in tPower; PGBF : in out tPGBF) is
      alpha     : long_float := long_float(PGBF.exponent);
      l         : integer := Order.x_Power;
      m         : integer := Order.y_Power;
      n         : integer := Order.z_Power;
      lmn       : long_float := long_float (l + m + n);
      Numerator : long_float;
      Denom     : long_float;
   use tFloat_Elementary_Functions;
   --  Taketa, Huzinaga and O-otage equation (2.2)
   begin
      Numerator := 2.0 ** (2.0 * lmn + 1.5) * alpha ** (lmn + 1.5);
      Denom := Pi**1.5 * fFact_2(2*l - 1) * fFact_2(2*m - 1) * fFact_2(2*n - 1);
      PGBF.Normalization := tNormalization (SqRt (Numerator / Denom));
   --   Put_Line ("Normalize_PGBF.Normalize_PGBF alpha, Normalization: "
   --             & texponent'image (PGBF.exponent) & ",  "
   --             & tNormalization'image (PGBF.Normalization));
   exception
      when anError :  others =>
         Put_Line("An exception occurred in Integrals.Normalize_PGBF.");
         Put_Line (Exception_Information (anError));
   end Normalize_PGBF;

-- ------------------------------------------------------------
   function Null_Molecule return tMolecule is
      Dummy : tMolecule;
   begin
      return Dummy;
   end Null_Molecule;

-- --------------------------------------------------------------------------

begin
   sPowers_List.Append ((0, 0, 0));

   pPowers_List.Append ((1, 0, 0));
   pPowers_List.Append ((0, 1, 0));
   pPowers_List.Append ((0, 0, 1));

   dPowers_List.Append ((2, 0, 0));
   dPowers_List.Append ((0, 2, 0));
   dPowers_List.Append ((0, 0, 2));
   dPowers_List.Append ((1, 1, 0));
   dPowers_List.Append ((0, 1, 1));
   dPowers_List.Append ((1, 0, 1));

   fPowers_List.Append ((3, 0, 0));
   fPowers_List.Append ((2, 1, 0));
   fPowers_List.Append ((2, 0, 1));
   fPowers_List.Append ((1, 2, 0));
   fPowers_List.Append ((1, 1, 1));
   fPowers_List.Append ((1, 0, 2));
   fPowers_List.Append ((0, 3, 0));
   fPowers_List.Append ((0, 2, 1));
   fPowers_List.Append ((0, 1, 2));
   fPowers_List.Append ((0, 0, 3));

    Powers_Map.Include(s, sPowers_List);
    Powers_Map.Include(p, pPowers_List);
    Powers_Map.Include(d, dPowers_List);
   Powers_Map.Include (f, fPowers_List);

end Molecule;
