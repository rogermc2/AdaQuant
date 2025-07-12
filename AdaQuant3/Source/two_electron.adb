with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.IO_Exceptions; use Ada.IO_Exceptions;

with Matrices; use Matrices;
with Electron_Repulsion_Integral;
with Tools; use Tools;

Package body Two_Electron is

   Two_Electron_File_Name  : String := "Two_Electron_Matrix";

-- -------------------------------------------------------------------------

   procedure Form_Two_Electron_Matrix
                          (Basis_Set           : in tCGBF_Orbital_Set;
                           Two_Electron_Matrix : in out tTwo_Electron_Matrix;
                           Symmetric           : in Boolean := False) is
   use CGBF_Orbital_List; -- for Cursor
   use Electron_Repulsion_Integral;
      Size    : Natural := Natural(Basis_Set.Length)**2;
      Row     : Positive range 1..Size := 1;
      Col     : Positive range 1 .. Size := 1;
      A_Count : Natural := 0;
      Procedure Do_CG_A (CG_A_Index : in CGBF_Orbital_List.Cursor)is
         CG_A_Element : tCGBF_Orbital;
         Procedure Do_CG_B (CG_B_Index : in CGBF_Orbital_List.Cursor)is
            CG_B_Element : tCGBF_Orbital;
            Procedure Do_CG_C (CG_C_Index : in CGBF_Orbital_List.Cursor)is
               CG_C_Element : tCGBF_Orbital;
               Procedure Do_CG_D (CG_D_Index : in CGBF_Orbital_List.Cursor)is
                  CG_D_Element : tCGBF_Orbital;
               begin
                  -- <CGBFA, CGBFB (Row)|Op|CGBFC, CGBFD (Col)>
    --              Put_Line ("Form_Two_Electrons_Matrix.Do_CG_D");
    --              Put ("Row, Col: " & Positive'image(Row));
    --              Put_Line ("   " & Positive'image(Col));
                  CG_D_Element := Element(CG_D_Index);
                  If not Symmetric or Col >= Row then
                    Two_Electron_Matrix (Row, Col) :=
                      Form_Two_Electron_CGBF_Integral
                        (CG_A_Element, CG_B_Element, CG_C_Element, CG_D_Element);
                  else
                    Two_Electron_Matrix(Row, Col) := Two_Electron_Matrix(Col, Row);
                  end if;
                  If Col < Size then
                     Col := Col + 1;
                  else
                     Col := 1;
                     If Row < Size then
                        Row := Row + 1;
                     end if;
                  end if;
               exception
                  when anError :  Constraint_Error =>
                     Put_Line ("A Constraint_Error occurred in " &
                               "Two_Electron.Form_Two_Electron_Matrix.Do_CG_D.");
                     Put("Row: " & Integer'image(Row));
                     Put_Line("   Col: " & Integer'image(Col));
                     Put_Line (Exception_Information (anError));
                  when anError :  others =>
                     Put_Line ("An exception occurred in " &
                               "Two_Electron.Form_Two_Electron_Matrix.Do_CG_D.");
                     Put("Row: " & Integer'image(Row));
                     Put_Line("   Col: " & Integer'image(Col));
                     Put_Line (Exception_Information (anError));
               end Do_CG_D;

            begin  -- Do_CG_C
--               Put_Line("Two_Electron.Form_Two_Electrons_Matrix.Do_CG_C");
               CG_C_Element := Element(CG_C_Index);
               Iterate (Basis_Set, Do_CG_D'access);
            exception
               when anError :  others =>
                  Put_Line ("An exception occurred in " &
                            "Two_Electron.Form_Two_Electron_Matrix.Do_CG_C.");
                  Put_Line("Col: " & Integer'image(Col));
                  Put_Line (Exception_Information (anError));
            end Do_CG_C;

         begin  -- Do_CG_B
            Put(".");
            CG_B_Element := Element(CG_B_Index);
            Iterate (Basis_Set, Do_CG_C'access);
         exception
            when anError :  others =>
               Put_Line ("An exception occurred in " &
                          "Two_Electron.Form_One_Electron_Matrix.Do_CG_B.");
               Put_Line("Row: " & Integer'image(Row));
               Put_Line (Exception_Information (anError));
         end Do_CG_B;

      begin  -- Do_CG_A
         If A_Count = 3 then
            New_Line;
            A_Count := 0;
         end if;
         Put (":");
         A_Count := A_Count + 1;
         CG_A_Element := Element(CG_A_Index);
         Iterate (Basis_Set, Do_CG_B'access);
      exception
         when anError :  others =>
           Put_Line ("An exception occurred in " &
                     "Two_Electron.Form_Two_Electron_Matrix.Do_CG_A.");
           Put_Line("Row: " & Integer'image(Row));
           Put_Line (Exception_Information (anError));
      end Do_CG_A;
   begin
      -- iterate through through the contraction sets of CGBFs
      -- the matrix is a matrix of CGBF matrix elements (integrals)
      -- <CGBFi, CGBFj| Op |CGBFk, CGBFl> where Op depend on the matrix type
--      Put_Line ("Form_Two_Electron_Matrix, Matrix:" & Natural'image(Size));
      Iterate (Basis_Set, Do_CG_A'access);
      New_Line;
   exception
      when anError :  Constraint_Error =>
         Put_Line("A Constraint Error occurred in Two_Electron.Form_Two_Electron_Matrix.");
         Put_Line(Exception_Information(anError));
      when anError :  others =>
         Put_Line("An exceptiom occurred in Two_Electron.Form_Two_Electrons_Matrix.");
         Put_Line(Exception_Information(anError));
   end Form_Two_Electron_Matrix;

-- ------------------------------------------------------------

   procedure Get_Two_Electron_Matrix (Basis_Set_Symbol : in tBasis_Set_Symbol;
                                      Orbitals         : in tCGBF_Orbital_Set;
                                Two_Electron_Matrix : in out tTwo_Electron_Matrix;
                                Molecule_File_Name  : in tFile_Name) is
      use Ada.Strings.Unbounded;
      use Ada.Streams;
      Size              : Positive := Two_Electron_Matrix'Length;
      Stored_Size       : Positive;
      Input_File        : Stream_IO.File_Type;
      Input_Stream      : Stream_Access;
      Read_Basis_Symbol : tBasis_Set_Symbol;
      Directory_Set     : Boolean := False;
      Response          : Character;
   begin
     Set_Directory (Get_Molecule_Directory);
     --  The exception Name_Error occurs if the string
     --  given as Directory does not identify an existing directory.
      Directory_Set := True;
      If Exists (Two_Electron_File_Name) then
         Open (Input_File, In_File, Two_Electron_File_Name);
         Input_Stream := Stream(Input_File);
         Positive'Read (Input_Stream, Stored_Size);
         tBasis_Set_Symbol'Read (Input_Stream, Read_Basis_Symbol);
         If Size > 4 And Stored_Size = Size And
            Read_Basis_Symbol = Basis_Set_Symbol then
            Put_Line("Use saved two-electron matrix file? [Y/y] ");
            Get (Response);
            If Response = 'Y' Or Response = 'y' then
               tTwo_Electron_Matrix'Read (Input_Stream, Two_Electron_Matrix);
            else
               Form_Two_Electron_Matrix (Orbitals, Two_Electron_Matrix, True);
            end if;
         else
           Form_Two_Electron_Matrix (Orbitals, Two_Electron_Matrix, True);
         end if;
         Close (Input_File);
      else
         Form_Two_Electron_Matrix (Orbitals, Two_Electron_Matrix, True);
      end if;
      Save_Two_Electron_Matrix (Two_Electron_Matrix, Basis_Set_Symbol, Molecule_File_Name);
      -- Put_Line ("Get_Two_Electron_Matrix, Two electron matrix:");
      -- Print_Square_Matrix (Two_Electron_Matrix);
   exception
      when anError : Ada.IO_Exceptions.Name_Error  =>
         Put_Line ("Get_Two_Electron_Matrix: Probably there is no directory "
                   & Get_Molecule_Directory);
         Put_Line (Exception_Information (anError));
      when anError : Ada.IO_Exceptions.End_Error  =>
         Put_Line ("Get_Two_Electron_Matrix EOF Exception.");
         Put_Line (Exception_Information (anError));
      when  anError : Ada.Text_IO.Status_Error =>
         -- File is already open
         Put_Line ("Get_Two_Electron_Matrix: File "
                   & Two_Electron_File_Name & " is already open. ");
         Put_Line (Exception_Information (anError));
      when  anError : others =>
         Put_Line ("An exception occurred in Get_Two_Electron_Matrix!");
         Put_Line (Exception_Information (anError));
   end Get_Two_Electron_Matrix;

-- --------------------------------------------------------------------------

   procedure Save_Two_Electron_Matrix
     (Two_Electron_Matrix : in tTwo_Electron_Matrix;
      Basis_Set_Symbol    : in tBasis_Set_Symbol;
      Molecule_File_Name  : in tFile_Name) is
      use Ada.Strings.Unbounded;
      use Ada.Streams;
      Size                : Positive := Two_Electron_Matrix'Length;
      Two_Electron_File   : Stream_IO.File_Type;
      Two_Electron_Stream : Stream_Access;
      Directory_Set       : Boolean := False;
   begin
      Set_Directory (Get_Molecule_Directory);
      --  The exception Name_Error occurs if the string
      --  given as Directory does not identify an existing directory.
      Directory_Set := True;
      If Exists (Two_Electron_File_Name) then
         Open (Two_Electron_File, Out_File, Two_Electron_File_Name);
      else
         Create (Two_Electron_File, Out_File, Two_Electron_File_Name);
      end if;
      Two_Electron_Stream := Stream (Two_Electron_File);
      Positive'Write (Two_Electron_Stream, Size);
      tBasis_Set_Symbol'Write (Two_Electron_Stream, Basis_Set_Symbol);
      tTwo_Electron_Matrix'Write (Two_Electron_Stream, Two_Electron_Matrix);
      Close (Two_Electron_File);
   exception
      when anError : Ada.IO_Exceptions.Name_Error  =>
         Put_Line ("Save_Two_Electron_Matrix: Probably there is no directory "
                   & Get_Molecule_Directory);
         Put_Line (Exception_Information (anError));
      when  anError : Ada.Text_IO.Status_Error =>
         -- File is already open
         Put_Line ("Save_Two_Electron_Matrix: File "
                   & Two_Electron_File_Name & " is already open. ");
         Put_Line (Exception_Information (anError));
      when  anError : others =>
         Put_Line ("An exception occurred in Save_Two_Electron_Matrix!");
         Put_Line (Exception_Information (anError));
   end Save_Two_Electron_Matrix;

   -- --------------------------------------------------------------------------

end Two_Electron;

