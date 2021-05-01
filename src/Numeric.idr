||| An alternative interface hierarchy for numbers
module Numeric

%default total

------------------------
-- NUMERIC INTERFACES --
------------------------

infixl 8 .+., .-., :+:, :-:
infixl 9 .*., ./., :*:, :/:

||| Additive notation for a binary operation and a constant
public export
interface Additive ty where
  constructor MkAdditive
  (+)  : ty -> ty -> ty
  zero : ty
  

||| Additive notation for a binary operation and a constant
public export
interface Additive1 ty where
  constructor MkAdditive1
  (.+.)  : ty -> ty -> ty
  zero1 : ty
  
||| Additive notation for a binary operation and a constant
public export
interface Additive2 ty where
  constructor MkAdditive2
  (:+:)  : ty -> ty -> ty
  zero2 : ty

public export  
additive1 : Additive ty => Additive1 ty
additive1 = MkAdditive1 (+) zero

public export
additive2 : Additive ty => Additive2 ty
additive2 = MkAdditive2 (+) zero

||| Additive notation for a binary operation and a constant
public export
interface Multiplicative ty where
  constructor MkMultiplicative
  (*)  : ty -> ty -> ty
  unit : ty

public export  
interface Additive ty => Negative ty where
  (-) : ty -> ty -> ty
  negate : ty -> ty
  negate = (zero -) 

public export  
interface Additive1 ty => Negative1 ty where
  constructor MkNegative1
  (.-.) : ty -> ty -> ty
  negate1 : ty -> ty
  negate1 = (zero1 .-.) 

public export  
interface Additive2 ty => Negative2 ty where
  constructor MkNegative2
  (:-:) : ty -> ty -> ty
  negate2 : ty -> ty
  negate2 = (zero2 :-:) 

public export  
negative1 : (Additive ty, Negative ty) => Negative1 ty 
negative1 
  = let _ = additive1 in 
    MkNegative1 (-) negate

public export  
negative2 : Negative ty => Negative2 ty
negative2 
  = let _ 
  = additive2 in MkNegative2 (-) negate


public export
[deriveAdditive] Num ty => Additive ty where
  (+)  = Num.(+)
  zero = 0

public export
[deriveMultiplicative] Num ty => Multiplicative ty where
  (*)  = Num.(*)
  unit = 1

public export  
[deriveNegative] Neg ty => Negative ty using deriveAdditive where
  (-) = Num.(-)
  negate = Num.negate

public export
[PairwiseAdditive] (Additive ty1, Additive ty2) => Additive (ty1, ty2) where
  (+) x y = (fst x + fst y, snd x + snd y)
  zero    = (zero, zero)

public export
[PairwiseMultiplicative] (Multiplicative ty1, Multiplicative ty2) => Multiplicative (ty1, ty2) where
  (*) x y = (fst x * fst y, snd x * snd y)
  unit    = (unit, unit)

public export
[PairwiseNegative] (Negative ty1, Negative ty2) => Negative (ty1, ty2) using PairwiseAdditive where
  (-) x y = (fst x - fst y, snd x - snd y)
  negate x   = (negate (fst x), negate (snd x))

-- That's it for now. More later.
  

{-reminders-}
{-
||| The Num interface defines basic numerical arithmetic.
public export
interface Num ty where
  (+) : ty -> ty -> ty
  (*) : ty -> ty -> ty
  ||| Conversion from Integer.
  fromInteger : Integer -> ty

%allow_overloads fromInteger

||| The `Neg` interface defines operations on numbers which can be negative.
public export
interface Num ty => Neg ty where
  ||| The underlying of unary minus. `-5` desugars to `negate (fromInteger 5)`.
  negate : ty -> ty
  (-) : ty -> ty -> ty

||| Numbers for which the absolute value is defined should implement `Abs`.
public export
interface Num ty => Abs ty where
  ||| Absolute value.
  abs : ty -> ty

public export
interface Num ty => Fractional ty where
  partial
  (/) : ty -> ty -> ty
  partial
  recip : ty -> ty

  recip x = 1 / x

public export
interface Num ty => Integral ty where
  partial
  div : ty -> ty -> ty
  partial
  mod : ty -> ty -> ty

----- Instances for primitives

-- Integer

%inline
public export
Num Integer where
  (+) = prim__add_Integer
  (*) = prim__mul_Integer
  fromInteger = id

public export
Neg Integer where
  negate x = prim__sub_Integer 0 x
  (-) = prim__sub_Integer

public export
Abs Integer where
  abs x = if x < 0 then -x else x

public export
Integral Integer where
  div x y
      = case y == 0 of
             False => prim__div_Integer x y
  mod x y
      = case y == 0 of
             False => prim__mod_Integer x y

-- This allows us to pick integer as a default at the end of elaboration if
-- all other possibilities fail. I don't plan to provide a nicer syntax for
-- this...
%defaulthint
%inline
public export
defaultInteger : Num Integer
defaultInteger = %search

-- Int

%inline
public export
Num Int where
  (+) = prim__add_Int
  (*) = prim__mul_Int
  fromInteger = prim__cast_IntegerInt

public export
Neg Int where
  negate x = prim__sub_Int 0 x
  (-) = prim__sub_Int

public export
Abs Int where
  abs x = if x < 0 then -x else x

public export
Integral Int where
  div x y
      = case y == 0 of
             False => prim__div_Int x y
  mod x y
      = case y == 0 of
             False => prim__mod_Int x y

-- Bits8

%inline
public export
Num Bits8 where
  (+) = prim__add_Bits8
  (*) = prim__mul_Bits8
  fromInteger = prim__cast_IntegerBits8

-- Bits16

%inline
public export
Num Bits16 where
  (+) = prim__add_Bits16
  (*) = prim__mul_Bits16
  fromInteger = prim__cast_IntegerBits16

-- Bits32

%inline
public export
Num Bits32 where
  (+) = prim__add_Bits32
  (*) = prim__mul_Bits32
  fromInteger = prim__cast_IntegerBits32

-- Bits64

%inline
public export
Num Bits64 where
  (+) = prim__add_Bits64
  (*) = prim__mul_Bits64
  fromInteger = prim__cast_IntegerBits64

-- Double

public export
Num Double where
  (+) = prim__add_Double
  (*) = prim__mul_Double
  fromInteger = prim__cast_IntegerDouble

public export
Neg Double where
  negate x = prim__negate_Double x
  (-) = prim__sub_Double

public export
Abs Double where
  abs x = if x < 0 then -x else x

public export
Fractional Double where
  (/) = prim__div_Double
-}
