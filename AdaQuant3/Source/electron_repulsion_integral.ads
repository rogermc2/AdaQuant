
with Molecule; use Molecule;
with Integrals; use Integrals;

Package Electron_Repulsion_Integral is
   function Form_Two_Electron_CGBF_Integral (CGBF_A : tCGBF_Orbital;
                                             CGBF_B : tCGBF_Orbital;
                                             CGBF_C : tCGBF_Orbital;
                                             CGBF_D : tCGBF_Orbital)
                                             return Long_Float;
End Electron_Repulsion_Integral;
