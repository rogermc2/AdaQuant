with Ada.Containers.Ordered_Maps;

Package body Elements is

  Package Element_Data_Package is new  Ada.Containers.Ordered_Maps
                       (Key_Type=>tElement, Element_Type=>tElement_Data);

   Element_Data_Map :  Element_Data_Package.Map := Element_Data_Package.Empty_Map;

   function Element_Data_Contains (Element : in tElement) return Boolean is
     begin
        return Element_Data_Map.Contains (Element);
   end Element_Data_Contains;


   function Element_Data (Element : in tElement) return tElement_Data is
      Begin
         return Element_Data_Map.Element (Element);
      end Element_Data;

begin
    Element_Data_Map.Include(H,(Z=>1));
    Element_Data_Map.Include(He,(Z=>2));
    Element_Data_Map.Include(Li,(Z=>3));
    Element_Data_Map.Include(Be,(Z=>4));
    Element_Data_Map.Include(B,(Z=>5));
    Element_Data_Map.Include(C,(Z=>6));
    Element_Data_Map.Include(N,(Z=>7));
    Element_Data_Map.Include(O,(Z=>8));
    Element_Data_Map.Include(F,(Z=>9));
    Element_Data_Map.Include(Ne,(Z=>10));
    Element_Data_Map.Include(Na,(Z=>11));
    Element_Data_Map.Include(Mg,(Z=>12));
    Element_Data_Map.Include(Al,(Z=>13));
    Element_Data_Map.Include(Si,(Z=>14));
    Element_Data_Map.Include(P,(Z=>15));
    Element_Data_Map.Include(S,(Z=>16));
    Element_Data_Map.Include(Cl,(Z=>17));
    Element_Data_Map.Include(Ar,(Z=>18));
    Element_Data_Map.Include(K,(Z=>19));
    Element_Data_Map.Include(Ca,(Z=>20));
    Element_Data_Map.Include(Sc,(Z=>21));
    Element_Data_Map.Include(Ti,(Z=>22));
    Element_Data_Map.Include(V,(Z=>23));
    Element_Data_Map.Include(Cr,(Z=>24));
    Element_Data_Map.Include(Mn,(Z=>25));
    Element_Data_Map.Include(Fe,(Z=>26));
    Element_Data_Map.Include(Co,(Z=>27));
    Element_Data_Map.Include(Ni,(Z=>28));
    Element_Data_Map.Include(Cu,(Z=>29));
    Element_Data_Map.Include(Zn,(Z=>30));
    Element_Data_Map.Include(Ga,(Z=>31));
    Element_Data_Map.Include(Ge,(Z=>32));
    Element_Data_Map.Include(As,(Z=>33));
    Element_Data_Map.Include(Se,(Z=>34));
    Element_Data_Map.Include(Br,(Z=>35));
    Element_Data_Map.Include (Kr,(Z=>36));

 end Elements;
