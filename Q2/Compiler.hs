module Compiler where

import RBL as R
import Lambda as L
import standardLib as SLib

translate :: R.Expression -> L.Term
translate (R.B True) = SLib.true
translate (R.B False) = SLib.false
translate (R.N 0) = SLib.false
translate (R.N m) = SLib.eval(SLib.succ(translate(N (m-1))))
translate (R.Add e1 e2) = undefined
translate (R.Sub e1 e2) = undefined
translate (R.And e1 e2) = undefined
translate (R.Or e1 e2) = undefined
translate (R.Not e1) = undefined
translate (R.IfThenElse c e1 e2) = undefined
translate (R.Let v e1 e2) = undefined
translate (R.Ref v) = undefined
translate (R.Lambda v e) = undefined
translate (R.App e1 e2) = undefined
