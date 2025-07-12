with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;

with Matrix_Types; use Matrix_Types;
with Matrices; use Matrices;
with Two_Electron; use Two_Electron;
with Nuclear_Repulsion; use Nuclear_Repulsion;
with Charge_Density; use Charge_Density;
with Fock; use Fock;
with Coefficients; use Coefficients;
with Tools;

Package body Self_Consistent_Field is

   Convergence_Criteria          : constant long_float := 10.0E-5;
   Max_Iterations                : constant Natural := 110;
   Charge_Density_Initialization : constant tCharge_Density := eFock;
   Averaging_Type                : constant tAverager := eSimple_Average;

-- --------------------------------------------------------------------------

   function Solve_For_Self_Consistent_Field
                                (Basis_Set_Symbol   : in tBasis_Set_Symbol;
                                 Orbitals           : in tCGBF_Orbital_Set;
                                 Molecule           : in tMolecule;
                                 Molecule_File_Name : in tFile_Name;
                                 Options            : in tOptions)
                                 return tSCF_Result is
      use CGBF_Orbital_List;
      Matrix_Size_1           : Natural := Natural (Orbitals.Length);
      Matrix_Size_2           : Natural := Matrix_Size_1 ** 2;
      Coefficient_Matrix      : tCoefficient_Matrix
                                 (1 .. Matrix_Size_1, 1 .. Matrix_Size_1);
      Core_Hamiltonian        : tCore_Hamiltonian_Matrix
                                 (1 .. Matrix_Size_1, 1 .. Matrix_Size_1);
      Fock_Matrix             : tFock_Matrix
                                 (1 .. Matrix_Size_1, 1 .. Matrix_Size_1);
      Overlap_Matrix          : tOverlap_Matrix
                                 (1 .. Matrix_Size_1, 1 .. Matrix_Size_1);
      Transformation_Matrix   : tTransformation_Matrix
                                 (1 .. Matrix_Size_1, 1 .. Matrix_Size_1);
      Two_Electron_Matrix     : tTwo_Electron_Matrix
                                 (1 .. Matrix_Size_2, 1 .. Matrix_Size_2);
      Charge_Density_Matrix   : tCharge_Density_Matrix
                                 (1 .. Matrix_Size_1, 1 .. Matrix_Size_1);
      Energy_Values           : tEigen_Values (1 .. Matrix_Size_1);
      Eo                      : long_float;
      Etotal                  : long_float;
      Iteration               : Natural := 0;
      Convergence             : long_float := 10.0;
      Result                  : tSCF_Result := (method => RHF);
      Error                   : tSource_Data_Error;
   begin
      -- Generate fixed matrices
      Form_Overlap_Matrix (Orbitals, Overlap_Matrix);
      Put_Line ("Overlap matrix:");
      Tools.Print_Square_Matrix (Overlap_Matrix, 1, 5);
      Form_Transformation_Matrix (Overlap_Matrix, Options.Orthogonalization_Method,
                                  Transformation_Matrix, Error);
      if Error = no_Error then
         Create_Core_Hamiltonian_Matrix (Orbitals, Molecule, Core_Hamiltonian);
      Put_Line ("Core Hamiltonian matrix:");
      Tools.Print_Square_Matrix (Core_Hamiltonian, 1, 5);
         Form_Charge_Density_Matrix (Charge_Density_Initialization,
                                     eSimple_Average, Core_Hamiltonian,
                                     Transformation_Matrix, Charge_Density_Matrix,
                                     Error);
         if Error = no_Error then
            Get_Two_Electron_Matrix (Basis_Set_Symbol, Orbitals,
                                     Two_Electron_Matrix, Molecule_File_Name);
            While Convergence > Convergence_Criteria and
                  Iteration < Max_Iterations loop
               Iteration := Iteration + 1;
               New_Line;
               Put_Line ("**** Iteration No.: " &
                          Natural'image (Iteration) & " ****");
               New_Line;
               Form_Orthogonal_Fock_Matrix (Core_Hamiltonian,
                                      Charge_Density_Matrix, Two_Electron_Matrix,
                                      Transformation_Matrix, Fock_Matrix, Eo);
               Form_Coefficient_Matrix (Fock_Matrix, Transformation_Matrix,
                                        Energy_Values, Coefficient_Matrix);
               -- Put_Line ("Transformed Coefficients matrix:");
               -- Tools.Print_Square_Matrix (Coefficient_Matrix, 1, 5);
               Update_Charge_Density_Matrix (Averaging_Type,
                                             Coefficient_Matrix,
                                             Charge_Density_Matrix, Convergence);
               Put_Line ("Convergence: " & float'image (float(Convergence)));
               Put_Line ("Electronic energy: " & float'image (float(Eo)));
            end loop;
            New_Line;
            If Convergence > Convergence_Criteria then
               Put_Line ("Evaluation failed to converge.");
               Put_Line ("Convergence: " & float'image (float(Convergence)));
            else
               Put_Line ("Evaluation converged in" & Natural'image (Iteration)
                       & " iterations.");
            End if;
            New_Line;
            Put_Line ("Orbital energy values:");
            Tools.Print_Vector (Energy_Values);
            Put_Line ("Orbital coefficients:");
            Tools.Print_Square_Matrix (Coefficient_Matrix, 1, 5);
            Etotal := Eo + Nuclear_Repulsion_Energy(Molecule);
               New_Line;
            Put_Line ("Nuclear repulsion energy: " &
                      float'image (float (Nuclear_Repulsion_Energy(Molecule))));
            Put_Line ("Electronic energy: " & float'image (float(Eo)));
            Put_Line ("Total energy: " & float'image (float (Etotal)));
            If Basis_Set_Symbol = sto3gso then
               Put_Line("       S & O Appendix B Electronic energy: -4.22753 ");
               Put_Line("       S & O Appendix B Total energy:      -2.86066 ");
            end if;
         end if;
      end if;
      New_Line;
      return Result;
   exception
      when anError :  others =>
         Put_Line("An exceptiom occurred in Solve_For_Self_Consistent_Field.");
         Put_Line(Exception_Information(anError));
         Error := eError;
         return Result;

   end Solve_For_Self_Consistent_Field;

end Self_Consistent_Field;

