with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions; use  Ada.Exceptions;

Package body Tools is
   Basis_Map          :  BasisMaps.Map := BasisMaps.Empty_Map;
   Molecule_Directory : String := "../MoleculeDescriptors";

   function Contains(name : String)  return Boolean is
   begin
     return Basis_Map.Contains (To_Unbounded_String(name)) ;
   end Contains;

   Function Diagonal_Matrix (aVector : in tLong_Float_Vector)
                             return tLong_Float_Matrix is
      the_Matrix : tLong_Float_Matrix := Unit_Matrix(aVector'Length);
   begin
      For Index in aVector'First .. aVector'Last loop
         the_Matrix(Index,Index) := aVector(Index);
      end loop;
      return the_Matrix;
   end Diagonal_Matrix;

   Function Get_Molecule_Directory return String is
   begin
      return Molecule_Directory;
   end Get_Molecule_Directory;

   Function get_basis_data (name : String) return tBasis_Data is
     lc_Name :  String := To_Lower(name);
     Basis_Dimension : Integer :=1;
     basis_Name : Unbounded_String;
     basis_Data : tBasis_Data;
   begin
     If Basis_Map.Contains (To_Unbounded_String(lc_Name)) then
         basis_Name := Basis_Map.Element (To_Unbounded_String (lc_Name));
         basis_Data := Get_Basis_Data(To_String(basis_Name));
     else
    	Put_Line ("Tools.get_basis_data, doesn't know the basis " & name);
     end If;

     return basis_Data;
   end get_basis_data;

   Procedure Print_Contraction_Item (Curs : in Contraction_Vector.Cursor) is
   use Contraction_Vector;
      Index : Extended_Index := To_Index(Curs);
      PGBF : tPGBF;
   begin
      PGBF := Contraction_Vector.Element (Curs);
      Put_Line ("Contraction.Element Index, Exponent, Coefficient, Normalization: ");
      Put_Line (Extended_Index'image (Index) & ",  "
              & Float'image (float(PGBF.Exponent)) & ",  "
              & Float'image (float(PGBF.Coeff)) & ",  "
              & tNormalization'image (PGBF.Normalization));
   end Print_Contraction_Item;

   Procedure Print_Matrix_Type (Matrix_Type : in tMatrix_Type) is
   begin
      case Matrix_Type is
         when Core_Hamiltonian => Put_Line("Core_Hamiltonian.");
         when Kinetic => Put_Line("Kinetic.");
         when Overlap => Put_Line("Overlap.");
         when Nuclear_Attraction => Put_Line("Nuclear_Attraction.");
         when Transformation => Put_Line("Transformation.");
         when Two_Electron => Put_Line("Two_Electron.");
      end case;
   exception
      when anError : others =>
         Put_Line("Tools.Print_Matrix_Type returned ");
         Put_Line(Exception_Information(anError));
   end Print_Matrix_Type;

   Procedure Print_Square_Matrix (theMatrix : in tLong_Float_Matrix) is
      Size  : integer := theMatrix'Length;
      Count : Natural;
   begin
      Put_Line("Matrix size: " & integer'image(Size));
      For Row in 1 .. Size loop
         Count := 0;
         For Col in 1 .. Size loop
            Count := Count + 1;
            If Count > 5 then
               New_Line;
               Put("  ");
               Count := 1;
            end if;
            Put(float'image(float((theMatrix(row, col)))) & "    ");
         end loop;
         New_Line;
      end loop;
   exception
      when anError : others =>
         Put_Line("Tools.Print_Square_Matrix returned ");
         Put_Line(Exception_Information(anError));
   end Print_Square_Matrix;

   Procedure Print_Square_Matrix (theMatrix : in tLong_Float_Matrix;
                                  start : in Positive; last : in Positive) is
      Size  : Positive := theMatrix'Length;
      first : Positive := start;
      fin   : Positive := last;
   begin
      if last > Size then
         fin := Size;
      end if;
      if first > fin then
         fin := start;
         first := last;
      end if;
      Put_Line("Matrix size: " & integer'image(Size));
      For Row in first..fin loop
         For Col in first..fin loop
            Put(float'image(float(theMatrix(row, col))) & "    ");
         end loop;
         New_Line;
      end loop;
   exception
      when anError : others =>
         Put_Line("Tools.Print_Square_Matrix returned ");
         Put_Line(Exception_Information(anError));
   end Print_Square_Matrix;

   Procedure Print_Vector(theVector : in tLong_Float_Vector) is
      Size  : Positive := theVector'Length;
      Count : Natural := 0;
   begin
      For Col in 1 .. Size loop
         Count := Count + 1;
         If Count > 5 then
            New_Line;
            Count := 1;
         end if;
         Put(float'image(float(theVector(col))) & "   ");
      end loop;
      New_Line;
   exception
      when anError : others =>
         Put_Line("Tools.Print_Vector returned ");
         Put_Line(Exception_Information(anError));
   end Print_Vector;


   Procedure Swap_Cols (Matrix : in out tLong_Float_Matrix; Col_A, Col_B : in integer) is
      Size  : Natural := Matrix'Length;
      temp  : long_Float;
   begin
      For Row in 1 .. Size loop
         Temp := Matrix (Row, Col_A);
         Matrix (Row, Col_A) := Matrix (Row, Col_B);
         Matrix(Row, Col_B) := temp;
      end loop;
   end Swap_Cols;

   Procedure Symmetrize_Matrix (theMatrix : in out tLong_Float_Matrix) is
      Size : Positive := theMatrix'Length;
   begin
      For Row in 2..Size loop
         For Col in 1 .. Size loop
            If Col < Row then
               theMatrix (row, col) := theMatrix (col, row);
            end if;
         end loop;
      end loop;
   exception
      when anError : others =>
         Put_Line("Tools.Symmetrize_Matrix returned ");
         Put_Line(Exception_Information(anError));
   end Symmetrize_Matrix;

begin
    Basis_Map.Include(To_Unbounded_String("6-31g**"),To_Unbounded_String("p631ss"));
    Basis_Map.Include(To_Unbounded_String("6-31g(d,p)"),To_Unbounded_String("p631ss"));
    Basis_Map.Include(To_Unbounded_String("6-31g**++"),To_Unbounded_String("p631ppss"));
    Basis_Map.Include(To_Unbounded_String("6-31g++**"),To_Unbounded_String("p631ppss"));
    Basis_Map.Include(To_Unbounded_String("6-311g**"),To_Unbounded_String("p6311ss"));
    Basis_Map.Include(To_Unbounded_String("6-311g++(2d,2p)"),To_Unbounded_String("p6311pp_2d_2p"));
    Basis_Map.Include(To_Unbounded_String("6-311g++(3d,3p)"),To_Unbounded_String("p6311pp_3d_3p"));
    Basis_Map.Include(To_Unbounded_String("6-311g++(3df,3pd)"),To_Unbounded_String("p6311pp_3df_3pd"));
    Basis_Map.Include(To_Unbounded_String("3-21g"),To_Unbounded_String("p321"));
    Basis_Map.Include(To_Unbounded_String("sto3g"),To_Unbounded_String("sto3g"));
    Basis_Map.Include(To_Unbounded_String("sto-3g"),To_Unbounded_String("sto3g"));
    Basis_Map.Include(To_Unbounded_String("sto-6g"),To_Unbounded_String("sto6g"));
    Basis_Map.Include(To_Unbounded_String("lacvp"),To_Unbounded_String("lacvp"));
    Basis_Map.Include(To_Unbounded_String("ccpvdz"),To_Unbounded_String("ccpvdz"));
    Basis_Map.Include(To_Unbounded_String("cc-pvdz"),To_Unbounded_String("ccpvdz"));
    Basis_Map.Include(To_Unbounded_String("ccpvtz"),To_Unbounded_String("ccpvtz"));
    Basis_Map.Include(To_Unbounded_String("cc-pvtz"),To_Unbounded_String("ccpvtz"));
    Basis_Map.Include(To_Unbounded_String("dzvp"),To_Unbounded_String("dzvp"));
end Tools;
