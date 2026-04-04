  $ ./main.exe | sed 's/[[:blank:]]*$//'
  Untyped Lambda Calculus Parser
  ==============================
  
  ✓ x               -> x
  ✓ (x y)           -> (x y)
  ✓ (fun x . x)     -> (fun x . x)
  ✓ (fun f . (fun x . (f x))) -> (fun f . (fun x . (f x)))
  ✓ (fun f . ((fun x . ((f x) x))(fun x . ((f x) x))) ) -> (fun f . ((fun x . ((f x) x)) (fun x . ((f x) x))))
  
  Untyped Lambda Calculus Parser
  ==============================
  
  ✓ x               -> x
  ✓ (x y)           -> (x y)
  ✓ (fun x . x)     -> (fun x . x)
  ✓ (fun f . (fun x . (f x))) -> (fun f . (fun x . (f x)))
  ✓ (fun f . ((fun x . ((f x) x))(fun x . ((f x) x))) ) -> (fun f . ((fun x . ((f x) x)) (fun x . ((f x) x))))
  ✓ x y (u v)       -> ((x y) (u v))
  ✓ (fun f . (fun x . f x)) -> (fun f . (fun x . (f x)))
  ✓ (fun f . (fun x . f x x) (fun x . f x x) ) -> (fun f . ((fun x . ((f x) x)) (fun x . ((f x) x))))
