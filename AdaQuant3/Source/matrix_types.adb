Package body Matrix_Types is
   Function "*" (Left, Right : in tLong_Float_Matrix) return tLong_Float_Matrix is
      Size   : Natural := left'length;
      result : tLong_Float_Matrix(1..Size,1..Size);
   begin
      For Row in left'First .. left'Last loop
         For Col in left'First .. left'Last loop
            result (Row, Col) := 0.0;
            for i  in left'First .. left'Last loop
               result (Row, Col) := result (Row, Col) +
                                    Left (Row, i) * Right (i, Col);
            end loop;
         end loop;
      end loop;
     return result;
   end "*";

-- ------------------------------------------------------------

   Function "*" (Left : in tLong_Float_Vector; Right : in tLong_Float_Matrix)
                                              return tLong_Float_Matrix is
      Size   : Natural := left'length;
      result : tLong_Float_Matrix(1..Size,1..Size);
   begin
      For Row in left'First .. left'Last loop
         For Col in left'First .. left'Last loop
            result (Row, Col) := 0.0;
            for i  in left'First .. left'Last loop
               result (Row, Col) := result (Row, Col) +
                                    Left (i) * Right (i, Col);
            end loop;
         end loop;
      end loop;
     return result;
   end "*";

-- ------------------------------------------------------------
end Matrix_Types;
