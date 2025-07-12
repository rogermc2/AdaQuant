
with Matrix_Types; use Matrix_Types;

Package Fock is
    Procedure Form_Orthogonal_Fock_Matrix
                         (Hamiltonian_Matrix    : in tCore_Hamiltonian_Matrix;
                          Density_Matrix        : in tCharge_Density_Matrix;
                          Two_Electron_Matrix   : in tTwo_Electron_Matrix;
                          Transformation_Matrix : in tTransformation_Matrix;
                          Fock_Matrix           : in out tFock_Matrix;
                          Electron_Energy       : out Long_Float);
end Fock;
