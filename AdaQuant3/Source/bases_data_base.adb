with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Float_Text_IO;

with Tools; use Tools;

Package body Bases_Data_Base is

  Type tBasis_Element_Shell is record
    element : tElement;
    shell : tBasisShell;
   end record;

-- Data Base Structure
-- Basis_Data_Base contains
-- {Element_Map} each of which contains
-- {Basis_Shell_Map} each of which contains
-- Coefficient + Exponent

  Basis_Shell_Map :  BasisShellMaps.Map := BasisShellMaps.Empty_Map;
  -- the_Bases_Data_Base : Data_Base.List := Data_Base.Empty_List;
  the_Bases_Data_Base : Data_Base.Map := Data_Base.Empty_Map;

  Function Add_Basis_Set(Basis_Name : tBasis_Name;Source_file : File_Type) return Boolean;
  function Form_Basis(Basis_Name : Unbounded_String; Source_file : File_Type)) return  tBasisData
  function Get_Basis_Name(Source_file : File_Type) return Unbounded_String ;

  function addToDataBase(File_Name : tFile_Name) return Boolean is
    Source_file : File_Type;
    Result : tFile_Result := OK;
    aLine : Unbounded_String := To_Unbounded_String("  ");
    Basis_Name : Unbounded_String;
    File_Name : Unbounded_String;
    aNew_Basis : tBasisData;
    begin
     Open (Source_file, In_File, To_String(File_Name));
     -- Get the basis name string from the file name (e.g. STO-3G)
     Basis_Name := Get_Basis_Name(Source_file);

     If Tools.Contains (Basis_Name) then
     	-- Convert it to its enumeration form
        -- Should do this in Get_Basis_Name?
       Basis_Name := Basis_Map.Element(Basis_Name);
       while  End_Of_File(Source_file) loop
         aNew_Basis := Form_Basis(Basis_Name,Source_file);
         -- if Basis exists then
         -- Replace with new basis
         --else
         -- Add new basis to data base
        Find_Next_Shell;
        Continue := not End_Of_File(Source_file) And not  Element_Map.Contains(Slice(next_Line,1,2));
       end loop;
     end If;

     Close (Source_file);
      return Result;

     exception
       when Event: others =>
         Result := Error;
        return Result;

    end addToDataBase;

    function Form_Basis(Basis_Name : Unbounded_String; Source_file : File_Type) return  tBasisData is
    BasisData : tBasisData;
    BasisDataCoeff : tCoefficient;
    BasisDataExponent : tExponent;
    basisName : Unbounded_String;
    Continue : Boolean := True;
    last : Positive;
    next_Line : Unbounded_String := aLine;
    Result : tFile_Result := OK;
  begin
    Continue := Element_Map.Contains(Slice(aLine,1,2));
    If Continue then
        BasisData.element := Element_Map.Element(Slice(aLine,1,2));
        BasisData.shell := Basis_Shell_Map.Element(Slice(aLine,6,7));
        next_Line := To_Unbounded_String(Get_Line(Source_file));
        Continue := not End_Of_File(Source_file) And Slice(next_Line,1,2) = "  ";
      If not End_Of_File(Source_file) And Slice(next_Line,1,2) = "  " then
        Ada.Float_Text_IO.Get(Slice(next_Line,6,15), BasisDataExponent, last);
        Ada.Float_Text_IO.Get(Slice(next_Line,31,41), BasisDataCoeff, last);
        BasisData.exponent := BasisDataExponent;
        BasisData.Coefficient  := BasisDataCoeff;
      end If;
    else
       next_Line := find_Next_Element(Source_file);
    end if;
    return BasisData;
  end Add_Basis;


  function Get_Basis_Name(Source_file : File_Type) return Unbounded_String is
  Basis_Name : Unbounded_String;
  Name_Length : Integer;
  begin
    Basis_Name := To_Unbounded_String(Name(Source_file));
    Name_Length := Ada.Strings.Unbounded.Length(Basis_Name) - 4;
    Basis_Name := Head(Basis_Name,Length);
    return Basis_Name;
  end Get_Basis_Name;

  function get_basis(Basis_Name :tBasis_Name) return tBasis is
  theBasis : tBasis;
  begin
    return theBasis;
  end get_basis;

   begin
    Element_Map.Include("H ", H);
    Element_Map.Include("He", He);
    Element_Map.Include("Li", Li);
    Element_Map.Include("Be", Be);
    Element_Map.Include("B ", B);
    Element_Map.Include("C ", C);
    Element_Map.Include("N ", N);
    Element_Map.Include("O ", O);
    Element_Map.Include("F ", F);
    Element_Map.Include("Ne", Ne);
    Element_Map.Include("Na", Na);
    Element_Map.Include("Mg", Mg);
    Element_Map.Include("Al", Al);
    Element_Map.Include("Si", Si);
    Element_Map.Include("Ti", Ti);
    Element_Map.Include("V ", V);
    Element_Map.Include("Cr", Cr);
    Element_Map.Include("Mn", Mn);
    Element_Map.Include("Fe", Fe);
    Element_Map.Include("Co", Co);

    Basis_Shell_Map.Include("s ", s);
    Basis_Shell_Map.Include("sp", sp);
    Basis_Shell_Map.Include("d ", d);

end Bases_Data_Base;
