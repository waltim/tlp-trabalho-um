module Compiler where

import RBL as R
import Lambda as L
import StandardLib as S

translate :: R.Expression -> L.Term
translate (R.B True) = S.true
translate (R.B False) = S.false
translate (R.N 0) = S.zero
translate (R.N m) = L.eval(L.App S.scc (translate(N (m-1))))
translate (R.Add e1 e2) = L.eval(L.App(L.App S.plus (translate e1))(translate e2))
translate (R.Sub e1 e2) = L.eval(L.App(L.App S.subt (translate e1))(translate e2))
translate (R.And e1 e2) = L.eval(L.App S.ands(L.App (translate e1) (translate e2)))
translate (R.Or e1 e2) = L.eval(L.App(L.App S.ors (translate e1))(translate e2))
translate (R.Not e1) = L.eval(L.App S.nots (translate e1))
translate (R.IfThenElse c e1 e2) = L.eval(L.App(L.App(L.App S.ite (translate c))(translate e1))(translate e2))
translate (R.Let v e1 e2) = L.eval(L.App(L.Lambda v (translate e1))(translate e2))
translate (R.Ref v) = L.eval(L.Var v)
translate (R.Lambda v e) = L.eval(L.Lambda v (translate e))
translate (R.App e1 e2) = L.eval(L.App (translate e1) (translate e2))
