{-# LANGUAGE DataKinds #-}

module RVV.Synthesizer.DefaultSynthType (DefaultConProg, DefaultSymProg) where

import Grisette (SymWordN, WordN)
import RVV.Synthesizer.Op (ConProg, SymProg)

type DefaultConProg = ConProg (WordN 8)

type DefaultSymProg = SymProg (WordN 8) (SymWordN 8)
