with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Ordered_Maps; use Ada.Containers;
with Ada.Exceptions; use  Ada.Exceptions;

Package body Basis_Utilities is
   function Basis_Set_Element(aBasis_Set : in tBasis_Set_Map;
                          anElement : in tElement) return tEnergy_Level_Basis_Set;
   function Energy_Level_Element(Level_Basis_Set : in tEnergy_Level_Basis_Set;
                          Energy_Level : in tEnergy_Level) return tShell_Basis_Set;
   procedure Get_Energy_Level_Map (aBasis_Set : in tBasis_Set_Map;
                        anElement        : in tElement;
                        Energy_Level     : in tEnergy_Level;
                        Energy_Level_Map : in out tEnergy_Level_Basis_Set;
                        Error            : out tSource_Data_Error);
   procedure Get_Shell_Map (Basis_Set : in tEnergy_Level_Basis_Set;
                        Energy_Level  : in tEnergy_Level;
                        Shell_Basis   : in out tShell_Basis_Set;
                            Error         : out tSource_Data_Error);
   function Shell_Element(Shell_Basis_Set : in tShell_Basis_Set;
                          Shell : in tElectron_Shell) return tPGBF_Contraction_Set;
-- -----------------------------------------------------------------------------

   function Basis_Set_Element (aBasis_Set : in tBasis_Set_Map;
                          anElement : in tElement) return tEnergy_Level_Basis_Set is
   begin
      return aBasis_Set.Element(anElement);
   end Basis_Set_Element;

-- -----------------------------------------------------------------------------

   function Energy_Level_Element(Level_Basis_Set : in tEnergy_Level_Basis_Set;
                          Energy_Level : in tEnergy_Level) return tShell_Basis_Set is
   begin
      return Level_Basis_Set.Element(Energy_Level);
   end Energy_Level_Element;

   -- -----------------------------------------------------------------------------

   -- Basis_Set contains a set of element basis sets
   -- Each element basis set contains a set of energy level basis sets
   -- Each energy level basis set contains a set of shell basis sets
   -- Each shell basis set contains a contraction set of primitive basis records
   -- Each primitive basis record contains a pair of exponent and coefficient values
   procedure Get_Basis (aBasis_Set   : in tBasis_Set_Map;
                        Element      : in tElement;
                        Energy_Level : in tEnergy_Level;
                        Shell        : in tElectron_Shell;
                        Basis        : in out tPGBF_Contraction_Set;
                        Error        : out tSource_Data_Error) is
      Energy_Level_Map : tEnergy_Level_Basis_Set; -- of shell basis sets
                                                  -- e.g an S and a P shell
      Shell_Map        : tShell_Basis_Set; -- of one contraction set per shell
      Contraction_Set  : tPGBF_Contraction_Set;
   begin
      Get_Energy_Level_Map (aBasis_Set, Element, Energy_Level, Energy_Level_Map, Error);
      If Error /= no_Error then
        Put_Line ("Get_Energy_Level_Map returned, Error: "
                  & tSource_Data_Error'image (Error));
      else
         Get_Shell_Map (Energy_Level_Map, Energy_Level, Shell_Map, Error);
      --   Put_Line ("Get_Basis, the Basis Set_Map has element: "
      --             & tElement'image (Element));
         If Error = no_Error then
            If Shell_Contains (Shell_Map, Shell) then
              Basis := Shell_Element(Shell_Map, Shell);
      --        Put_Line ("Get_Basis, the Shell_Map has: "
      --                   & tElectron_Shell'image (Shell));
            else
               Error := eShell_Symbol_Error;
               Put_Line ("Get_Basis, the Shell_Map doesn't contain the shell: "
                         & tElectron_Shell'image (Shell));
            end if;
         else
           Put_Line ("Get_Basis, the Basis Set_Map doesn't contain the element: "
                         & tElement'image (Element));
         end if;
      end if;
   exception
      when anError : Constraint_Error =>
         Put("Basis_Utilities.Get_Basis returned ");
         Put_Line(Exception_Information(anError));
         Error := eConstraint_Error;
      when anError :  others =>
         Put_Line("An exceptiom occurred in Basis_Utilities.Get_Basis.");
         Put_Line(Exception_Information(anError));
         Error := eError;
   end Get_Basis;

-- -----------------------------------------------------------------------------

    procedure Get_Energy_Level_Map (aBasis_Set : in tBasis_Set_Map;
                        anElement        : in tElement;
                        Energy_Level     : in tEnergy_Level;
                        Energy_Level_Map : in out tEnergy_Level_Basis_Set;
                        Error            : out tSource_Data_Error) is
   begin
        If aBasis_Set.Basis_Set_Contains(anElement) then
          Energy_Level_Map := Basis_Set_Element(aBasis_Set, anElement);
        else
          Error := eElement_Symbol_Error;
               Put_Line ("Get_Energy_Level_Map, the Element_Map doesn't contain the element: "
                         & tElement'image (anElement));
        end if;
   end Get_Energy_Level_Map;

-- -----------------------------------------------------------------------------

   procedure Get_Shell_Map (Basis_Set   : in tEnergy_Level_Basis_Set;
                           Energy_Level : in tEnergy_Level;
                           Shell_Basis  : in out tShell_Basis_Set;
                            Error       : out tSource_Data_Error) is
      Energy_Level_Basis : tEnergy_Level_Basis_Set;
   begin
        If Basis_Set.Contains(Energy_Level) then
          Shell_Basis := Basis_Set.Element(Energy_Level);
        else
          Error := eShell_Symbol_Error;
        end if;
   end Get_Shell_Map;

-- -----------------------------------------------------------------------------

   function Shell_Element (Shell_Basis_Set : in tShell_Basis_Set;
                           Shell           : in tElectron_Shell)
                           return tPGBF_Contraction_Set is
   begin
      return Shell_Basis_Set.Element(Shell);
   end Shell_Element;

end Basis_Utilities;
