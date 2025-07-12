with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;
with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;

with Elements;
with Tools;
with Electron_Repulsion_Integral;

package body Matrices is
   use tFloat_Elementary_Functions;
   Type tEigenVal_Sort is record
      eVal          : Long_Float;
      Initial_Index : Positive;
   end record;
   package EigenVal_Sort_List is
     new Doubly_Linked_Lists (tEigenVal_Sort);

   Function eval_compare (left, right : in tEigenVal_Sort) return Boolean;
   Procedure Form_One_Electron_Matrix
                       (Basis_Set      : in tCGBF_Orbital_Set;
                        Matrix_Type    : in tMatrix_Type;
                        Molecule       : in tMolecule := Null_Molecule;
                        Symmetric      : in Boolean := False;
                        Orbital_Matrix : out tLong_Float_Matrix);
   Procedure Form_Kinetic_Energy_Matrix
                      (Basis_Set             : in tCGBF_Orbital_Set;
                       Kinetic_Energy_Matrix : out tLong_Float_Matrix);
   Procedure Form_Nucleus_Electron_Attraction_Matrix
                      (Basis_Set      : in tCGBF_Orbital_Set;
                       Molecule       : in tMolecule;
                       Nuclear_Matrix : out tLong_Float_Matrix);

   Function eval_compare (left, right : in tEigenVal_Sort) return Boolean is
   begin
     return right.Eval < left.Eval;
   end eval_compare;

   Package List_Sorting is new
           EigenVal_Sort_List.Generic_Sorting (eval_compare);
   Type tEigenVal_Sort_List is new EigenVal_Sort_List.List with Null record;

-- ------------------------------------------------------------

   Procedure Create_Core_Hamiltonian_Matrix
                        (Basis_Set : in tCGBF_Orbital_Set;
                         Molecule  : in tMolecule;
                         Hamiltonian_Matrix : in out tCore_Hamiltonian_Matrix) is
      Kinetic_Matrix : tLong_Float_Matrix := (Hamiltonian_Matrix);
      Nuclear_Matrix : tLong_Float_Matrix := Kinetic_Matrix;
   begin
      Form_Kinetic_Energy_Matrix (Basis_Set, Kinetic_Matrix);
      Form_Nucleus_electron_Attraction_Matrix(Basis_Set, Molecule, Nuclear_Matrix);
      Hamiltonian_Matrix :=
                   tCore_Hamiltonian_Matrix(Kinetic_Matrix + Nuclear_Matrix);
      -- Put_Line ("Create_Core_Hamiltonian_Matrix, Core_Hamiltonian matrix:");
      -- Tools.Print_Square_Matrix(Hamiltonian_Matrix);
   end Create_Core_Hamiltonian_Matrix;

-- ------------------------------------------------------------

   Procedure Form_Kinetic_Energy_Matrix
                     (Basis_Set             : in tCGBF_Orbital_Set;
                      Kinetic_Energy_Matrix : out tLong_Float_Matrix) is
   begin
      Form_One_Electron_Matrix (Basis_Set, Kinetic, Null_Molecule, True,
                                Kinetic_Energy_Matrix);
      -- Put_Line ("Form_Kinetic_Energy_Matrix, Kinetic energy matrix");
      -- Put_Line ("(cf S&O equation (3.252)):");
      -- Tools.Print_Square_Matrix(Kinetic_Energy_Matrix);
   end Form_Kinetic_Energy_Matrix;

-- ------------------------------------------------------------

   Procedure Form_Nucleus_Electron_Attraction_Matrix
                 (Basis_Set      : in tCGBF_Orbital_Set;
                  Molecule       : in tMolecule;
                  Nuclear_Matrix : out tLong_Float_Matrix) is
   begin
      Form_One_Electron_Matrix (Basis_Set, Nuclear_Attraction, Molecule, True,
                                Nuclear_Matrix);
      -- Put_Line ("Form_Nucleus_Electron_Attraction_Matrix, "
      --         & "Nuclear attraction matrix");
      -- Put_Line ("         (cf sum of S&O equations (3.253) and (3.254):");
      -- Tools.Print_Square_Matrix(Nuclear_Matrix);
   end Form_Nucleus_electron_Attraction_Matrix;

-- ------------------------------------------------------------

-- tCGBF_Orbital_Set : {tCGBF_Orbital}
-- tCGBF_Orbital:
--   Ion Position : {x , y, z}
--   Order :  {x_power , y_power, z_power} ({l, m, n})
--   Contraction_Set : {tExponent, tCoefficient}
--   Normalization_Factor ?
-- ------------------------------------------------------------

  Procedure Form_One_Electron_Matrix(Basis_Set : in tCGBF_Orbital_Set;
                                Matrix_Type    : in tMatrix_Type;
                                Molecule       : in tMolecule := Null_Molecule;
                                Symmetric      : in Boolean := False;
                                Orbital_Matrix : out tLong_Float_Matrix) is
      Procedure Do_Row (Row_Index : in CGBF_Orbital_List.Cursor);
      use CGBF_Orbital_List; -- for Cursor
        Size   : Natural := Natural(Basis_Set.Length);
        Row    : Positive range 1..Size := 1;
        Col    : Positive range 1..Size;
      Procedure Do_Row (Row_Index : in CGBF_Orbital_List.Cursor)is
         Procedure Do_Col (Col_Index : in CGBF_Orbital_List.Cursor);
         Row_Element : tCGBF_Orbital;
         Continue  : Boolean := True;
         Procedure Do_Col (Col_Index : in CGBF_Orbital_List.Cursor)is
            Col_Element : tCGBF_Orbital;
         begin
            Col_Element := Element (Col_Index);
            -- <CGBF(Row)|Op|CGBF(Col)>
            If not Symmetric or Col >= Row then
              Orbital_Matrix(Row, Col) := Form_CGBF_Integral
                               (Matrix_Type, Row_Element, Col_Element, Molecule);
             -- Put_Line ("Row " & Positive'image (Row)
             --         & ",  Col: " & Positive'image (Col)
             --         & ": " & Float'image (Float(Orbital_Matrix(Row, Col))));

            else
              Orbital_Matrix(Row, Col) := Orbital_Matrix(Col, Row);
            end if;

           If Col < Size then
             Col := Col + 1;
           end if;
         exception
           when anError :  others =>
             Put_Line("An exception occurred in Matrices.Form_One_Electron_Matrix.Do_Col.");
             Put_Line("Col: " & Integer'image(Col));
             Put_Line (Exception_Information (anError));
         end Do_Col;

      begin
         Row_Element := Element(Row_Index);
         Col := 1;
         Iterate (Basis_Set, Do_Col'access);

         If Row < Size then
            Row := Row + 1;
         end if;
      exception
         when anError :  others =>
           Put_Line("An exception occurred in Matrices.Form_One_Electron_Matrix.Do_Row.");
           Put_Line("Row: " & Integer'image(Row));
           Put_Line (Exception_Information (anError));
      end Do_Row;
  begin
      -- iterate through through the contraction sets of CGBFs
      -- the matrix is a matrix of CGBF matrix elements <CGBFi|Op|CGBFj>
      -- or for Two Electron operators, <CGBFi, CGBFj|Op|CGBFk, CGBFl>
      -- where Op depend on the matrix type
      --Put_Line ("Matrices.Form_One_Electron_Matrix, Orbital_Matrix:");
      Iterate (Basis_Set, Do_Row'access);

  exception
      when anError :  Constraint_Error =>
         Put_Line("A Constraint Error occurred in Matrices.Form_One_Electron_Matrix.");
         Put_Line(Exception_Information(anError));
      when anError :  others =>
         Put_Line("An exceptiom occurred in Matrices.Form_One_Electron_Matrix.");
         Put_Line(Exception_Information(anError));
  end Form_One_Electron_Matrix;

-- ------------------------------------------------------------

   Procedure Form_Overlap_Matrix(Basis_Set      : in tCGBF_Orbital_Set;
                                 Overlap_Matrix : out tLong_Float_Matrix) is
   begin
      Form_One_Electron_Matrix (Basis_Set, Overlap, Null_Molecule, True,
                                Overlap_Matrix);
   end Form_Overlap_Matrix;

-- ------------------------------------------------------------

   Procedure Form_Transformation_Matrix (Overlap_Matrix : in tLong_Float_Matrix;
                                  Method : in tOrthog_Method := Symmetric;
                   Transformation_Matrix : out tTransformation_Matrix;
                                  Error  : out tSource_Data_Error) is
      Size           : Natural := Overlap_Matrix'Length;
      Eigenvals      : tLong_Float_Vector(1..Size);
      Unitary_Matrix : tLong_Float_Matrix (1 .. Size, 1 .. Size);
      diag_Matrix    : tLong_Float_Matrix (1 .. Size, 1 .. Size)
                                          := Unit_Matrix (Size);
      -- test_Matrix    : tLong_Float_Matrix (1 .. Size, 1 .. Size);
      Procedure Form_Symmetric_Transformation_Matrix is
      begin
         Put_Line("Form_Symmetric_Transformation_Matrix");
         For index in  1 .. Size loop
            diag_Matrix(index,index) := 1.0/SqRt(Eigenvals(index));
         end loop;
         -- The transformation matrix (X) is given by
         -- Szabo and Ostland equation (3.167)
         Transformation_Matrix :=
           tTransformation_Matrix (Unitary_Matrix * diag_Matrix);
         Transformation_Matrix :=
           Transformation_Matrix * Transpose (Unitary_Matrix);
      end Form_Symmetric_Transformation_Matrix;

      Procedure Form_Canonical_Transformation_Matrix is
      use EigenVal_Sort_List;
         Sort_Element      : tEigenVal_Sort;
         Sort_Vector       : tEigenVal_Sort_List;
         Sorted_Unitary    : tLong_Float_Matrix (1 .. Size, 1 .. Size);
         Sort_Cursor       : Cursor;
         Max_Col           : Natural;
         Continue          : Boolean := True;
         In_Limit          : Boolean := True;
      begin
         -- Put_Line ("Form_Canonical_Transformation_Matrix");
         For index in  1 .. Size loop
            Sort_Element.eVal := Eigenvals(index);
            Sort_Element.Initial_Index := index;
            Sort_Vector.Append(Sort_Element);
         end loop;
         List_Sorting.Sort(List(Sort_Vector));
         -- Put_line("Sorted eigenvalues of Overlap Matrix:");
         -- Sort_Cursor := Sort_Vector.First;
         -- For index in  1 .. Size loop
         --    Put(float'image(float(Element(Sort_Cursor).eval)));
         --    Sort_Cursor := Next(Sort_Cursor);
         -- end loop;
         -- New_Line;
         Sort_Cursor := Sort_Vector.First;
         Max_Col := 1;
         For index in  1 .. Size loop
            Sort_Element := Element (Sort_Cursor);
           --  Put_line("Sorted eigenvalue:" & Float'image(float(Sort_Element.eVal)));
            For row in 1 .. Size loop
               Sorted_Unitary (row, Index) :=
                          Unitary_Matrix(row, Sort_Element.Initial_Index);
            end loop;
            If In_Limit Then
               In_Limit := Sort_Element.Eval > 10.0**(-4);
               If In_Limit then
                  diag_Matrix (index, index) :=
                        1.0 / SqRt (Sort_Element.eVal);
                  Max_Col := index;
               end if;
            end if;
            Sort_Cursor := Next(Sort_Cursor);
         end loop;
         -- Put_line("Diagonal Matrix (cf S&O equation (3.260):");
         -- Tools.Print_Square_Matrix ((diag_Matrix));
         -- Put_line("Sorted_Unitary Matrix (cf S&O equation (3.259):");
         -- Tools.Print_Square_Matrix ((Sorted_Unitary));
         -- The transformation matrix (X) is given by
         -- Szabo and Ostland equation (3.167)
         If In_Limit Then
            Transformation_Matrix := Chop(Sorted_Unitary * diag_Matrix);
         else
            declare
               Reduced_Diag : tLong_Float_Matrix(1 .. Size, 1 .. Max_Col);
            begin
              Put_Line ("Matrices.Form_Canonical_Transformation_Matrix"
                      & " using reduced matrix.");
                Put_Line ("Max_Col: " & integer'image(Max_Col));
               For row in 1..Size loop
                  For Col in 1..Max_Col loop
                     Reduced_Diag (Row, Col) := diag_Matrix (Row, Col);
                  end loop;
               end loop;
               Transformation_Matrix := Sorted_Unitary * Reduced_Diag;
            end; -- declare block
         end if;
      exception
         when anError : Constraint_Error =>
            Put_Line ("A constraint error occurred in "
                     & "Matrices.Form_Canonical_Transformation_Matrix.");
           Put_Line(Exception_Information(anError));
           Error := eConstraint_Error;
         when anError : Ada.Numerics.Argument_Error =>
           Put_Line("An argument error occurred in "
                  & "Matrices.Form_Canonical_Transformation_Matrix.");
           Put_Line(Exception_Information(anError));
           Error := eArgument_Error;
         when anError :  others =>
           Put_Line("An exceptiom occurred in "
                  & "Matrices.Form_Canonical_Transformation_Matrix.");
           Put_Line(Exception_Information(anError));
           Error := eError;
      end Form_Canonical_Transformation_Matrix;
   begin
      Error := no_Error;
      Long_Float_Arrays.EigenSystem (Real_Matrix (Overlap_Matrix),
                                     Real_Vector (Eigenvals),
                                     Real_Matrix (Unitary_Matrix));
      Unitary_Matrix := Transpose (Unitary_Matrix);
      -- Put_line("Eigenvalues and eigenvectors of Overlap Matrix:");
      -- Tools.Print_Vector(Eigenvals);
      -- Tools.Print_Square_Matrix (Unitary_Matrix);
      -- Put_line ("Transpose of Unitary matrix times itself:");
      -- test_Matrix := Chop(Transpose (Unitary_Matrix) * Unitary_Matrix);
      -- Tools.Print_Square_Matrix (test_Matrix);
      -- Put_line ("H.v - E.v:");
      -- Tools.Print_Square_Matrix (Chop(Overlap_Matrix * Unitary_Matrix
      --                         - Unitary_Matrix *
      --                           Tools.Diagonal_Matrix (Eigenvals)));
      -- Columns are the eigenvectors in eigenvalue order.
      -- Szabo and Ostland equation (3.166);
      Case Method is
         when Symmetric =>
                  Form_Symmetric_Transformation_Matrix;
         when Canonical =>
                  Form_Canonical_Transformation_Matrix;
      end case;
      -- If Error = no_error then
        -- Put_Line ("Matrices.Form_Transformation_Matrix, Transformation Matrix");
        -- Put_Line ("         (cf S&O equation (3.262) for canonical):");
        -- Tools.Print_Square_Matrix (Transformation_Matrix);
        -- Reuse diag_Matrix to check Transpose(X) S X
        -- diag_Matrix := Transpose (Transformation_Matrix) * Overlap_Matrix
        --              * Transformation_Matrix;
        --  Put_Line ("Transpose(X) S X should be an identity matrix (within say 10%):");
        --  Tools.Print_Square_Matrix (tLong_Float_Matrix (diag_Matrix));
      -- end if;
   exception
      when anError : Constraint_Error =>
        Put_Line("A constraint error occurred in Matrices.Form_Transformation_Matrix.");
        Put_Line(Exception_Information(anError));
        Error := eConstraint_Error;
      when anError : Ada.Numerics.Argument_Error =>
        Put_Line("An argument error occurred in Matrices.Form_Transformation_Matrix.");
        Put_Line(Exception_Information(anError));
        Error := eArgument_Error;
      when anError :  others =>
        Put_Line("An exceptiom occurred in Matrices.Form_Transformation_Matrix.");
        Put_Line(Exception_Information(anError));
        Error := eError;
   end Form_Transformation_Matrix;

-- ------------------------------------------------------------

end Matrices;
