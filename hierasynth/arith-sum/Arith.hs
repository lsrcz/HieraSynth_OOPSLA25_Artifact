{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Arith (Op, ConProg, Sketch) where

import Grisette (SymInteger)
import HieraSynth.Combinator.Sum (type (:|))
import HieraSynth.Operator.OpTyping (DefaultType)
import qualified HieraSynth.Program.ComponentSketch as Component
import qualified HieraSynth.Program.Concrete as Concrete
import Grisette.Unified (EvalModeTag (C, S))
import Operator.Add (Add)
import Operator.AddImm (AddImm)
import Operator.Mul (Mul)

type Op mode = Add :| AddImm mode :| Mul

type ConProg = Concrete.Prog (Op 'C) Integer DefaultType

type Sketch = Component.Prog (Op 'S) SymInteger DefaultType
