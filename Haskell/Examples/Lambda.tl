-- Inferred: f : Int -> Int
f : _ <- \(a : Int). a + 2;
print f 2;

f' : Bool -> Bool -> Bool;
f' <- \(a : Bool) (b : Bool). a and b;

print f' True False or True;

-- Poor man's objects
obj : { getX : () -> Int, getY : () -> Int };
obj <- { getX <- \(a : ()). 2, getY <- \(a : ()). 3 };
-- No type inference for lambda args + No support for patterns = Horrible syntax

print obj.getX ();
print obj.getY ()
