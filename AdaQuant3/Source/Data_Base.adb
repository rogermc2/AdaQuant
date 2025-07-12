with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Element_Data_Base;

Package body Data_Base  is
  Package Data_Base is new  Ada.Containers.Ordered_Maps
                 (Key_Type => tBasis_Name, Element_Type => tBasis_Shell_Data_Base);

  Type tBasis_Data_Base is new Data_Base.Map with Null record;
  Basis_Data_Base  :  Data_Base.Map := Data_Base.Empty_Map;

   function Add_Basis(Source_file : File_Type; aLine : Unbounded_String) return  Unbounded_String;
   function find_Next_Element(Source_file : File_Type) return  Unbounded_String;


  Function Add_Basis_Set(Basis_Name : tBasis_Name;Source_file : File_Type) return Boolean is
    aLine : Unbounded_String;
    Data_Base_Element : tBasis_Shell_Data_Base;
    Result : Boolean := False;
  begin
    If not  End_Of_File(Source_file) then
      If Basis_Data_Base.Contains(Basis_Name) then
        -- it should, need assertion?
        while not End_Of_File(Source_file) loop
          aLine := find_Next_Element(Source_file);
        end loop;
      else
         Null;
       end If;
     end If;
     return Result;
   end Add_Basis_Set;

   function find_Next_Element(Source_file : File_Type) return  Unbounded_String is
    aLine : Unbounded_String := To_Unbounded_String("  ");
    two_Chars :  String := ("  ");
    Continue : Boolean := True;

  begin
    two_Chars := Slice(aLine,1,2);
     while not End_Of_File(Source_file) And Continue loop
      Continue := not Element_Map.Contains(two_Chars);
       If Continue then
         aLine := To_Unbounded_String(Get_Line(Source_file));
       End if;
     end loop;
     return aLine;
  end find_Next_Element;

end Data_Base;
