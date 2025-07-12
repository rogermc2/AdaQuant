with Ada.Containers.Ordered_Maps;

Package body Element_Shell_Symbol_Maps is

  Package Element_Map is new  Ada.Containers.Ordered_Maps
                       (Key_Type=>tString_2, Element_Type=>tElement);

   Package Basis_Shell_Maps is new  Ada.Containers.Ordered_Maps
     (Key_Type => tString_2, Element_Type => tBasis_Shell_Code);

   Element_Name_Map :  Element_Map.Map := Element_Map.Empty_Map;
   Basis_Shell_Map :  Basis_Shell_Maps.Map := Basis_Shell_Maps.Empty_Map;

   function Element_Symbol_Map_Contains (Element : in tString_2) return Boolean is
     begin
        return Element_Name_Map.Contains (Element);
   end Element_Symbol_Map_Contains;

   function Shell_Symbol_Map_Contains (Shells : in tString_2) return Boolean is
     begin
        return Basis_Shell_Map.Contains (Shells);
   end Shell_Symbol_Map_Contains;

   function Element_Symbol (Element : in tString_2) return tElement is
      Begin
         return Element_Name_Map.Element (Element);
      end Element_Symbol;

   function Shells_Code (Shells : in tString_2) return tBasis_Shell_Code is
      Begin
         return Basis_Shell_Map.Element (Shells);
      end Shells_Code;

   begin
    Element_Name_Map.Include("H ", H);
    Element_Name_Map.Include("HE", He);
    Element_Name_Map.Include("LI", Li);
    Element_Name_Map.Include("B ", B);
    Element_Name_Map.Include("C ", C);
    Element_Name_Map.Include("N ", N);
    Element_Name_Map.Include("O ", O);
    Element_Name_Map.Include("F ", F);
    Element_Name_Map.Include("NE", Ne);
    Element_Name_Map.Include("NA", Na);
    Element_Name_Map.Include("MG", Mg);
    Element_Name_Map.Include("AL", Al);
    Element_Name_Map.Include("SI", Si);
    Element_Name_Map.Include("P ", P);
    Element_Name_Map.Include("S ", S);
    Element_Name_Map.Include("CL", Cl);
    Element_Name_Map.Include("AR", Ar);
    Element_Name_Map.Include("K ", K);
    Element_Name_Map.Include("CA", Ca);
    Element_Name_Map.Include("SC", Sc);
    Element_Name_Map.Include("TI", Ti);
    Element_Name_Map.Include("V ", V);
    Element_Name_Map.Include("CR", Cr);
    Element_Name_Map.Include("MN", Mn);
    Element_Name_Map.Include("FE", Fe);
    Element_Name_Map.Include("CO", Co);
    Element_Name_Map.Include("NI", Ni);
    Element_Name_Map.Include("CU", Cu);
    Element_Name_Map.Include("ZN", Zn);
    Element_Name_Map.Include("GA", Ga);
    Element_Name_Map.Include("GE", Ge);
    Element_Name_Map.Include("AS", As);
    Element_Name_Map.Include("SE", Se);
    Element_Name_Map.Include("BR", Br);
    Element_Name_Map.Include ("KR", Kr);

    Basis_Shell_Map.Include("S ", s);
    Basis_Shell_Map.Include(" S", s);
    Basis_Shell_Map.Include("SP", sp);
    Basis_Shell_Map.Include("D ", d);
    Basis_Shell_Map.Include(" D", d);

 end Element_Shell_Symbol_Maps;
