
namespace hn

constant IO : Type -> Type
constant nil : Π a , list a
constant cons : Π a , a → list a → list a
constant list : Type -> Type
constant filter : Π a , (a → bool) -> list a -> list a

definition boolean := bool

definition hn_if (a : Type) (b : a) := bool.cases_on bool.tt b b
definition hn_or := bool.bor
definition hn_and := bool.band

definition num  := nat
definition incr := nat.succ
definition sum := nat.add
definition mul := nat.mul
definition div := nat.div
definition sub := nat.sub
definition string := string

definition const (a : Type) (b : Type) (c : a) (d : b) : a := c

definition is_zero (a : nat) : boolean := nat.cases_on a bool.tt (const bool nat bool.ff)

definition eq (a b : nat) := bool.band (is_zero (sub a b)) (is_zero (sub b a))
definition less (a b : nat) := bool.bnot (is_zero (sub b a))

definition natrec (a : Type) (f : nat → a → a) (x : a) (n : nat) : a 
   := nat.rec_on n x f
   
-- check option.none

end hn
