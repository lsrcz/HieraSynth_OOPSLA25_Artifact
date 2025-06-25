{-# LANGUAGE DataKinds #-}

module InferredSketch (getInferredSketch) where

import qualified Data.Text as T
import Grisette
  ( ToSym (toSym),
    WordN,
  )
import HieraSynth.Program.Choice.ChoiceTree
  ( ChoiceMeta (Split),
    ChoiceTree (Branch, Leaf),
  )
import HieraSynth.Program.Choice.ComponentBag
  ( ComponentBag (ComponentBag),
  )
import HieraSynth.Program.ProgTyping (symbolType)
import HieraSynth.Program.SymbolTable (SymbolTable (SymbolTable))
import HieraSynth.TypeSignature (TypeSignature (TypeSignature))
import RVV.Synthesizer.Feature.ToSketchOp (simplifyChoiceTree)
import RVV.Synthesizer.Op
  ( ConSymbolTable,
    SketchSpec (CompSpec),
    SketchSpecTable,
  )
import RVV.Synthesizer.Operator.Common.ImmSpec
  ( ImmSpec
      ( ArbitraryImmSpec,
        BoundedImmSpec
      ),
  )
import RVV.Synthesizer.Operator.Common.RHSSpec
  ( SketchRHSSpec (SketchImmRHS, SketchScalarRHS),
  )
import RVV.Synthesizer.Operator.ScalarOperator
  ( sketchScalarBin,
    sketchScalarUnary,
  )
import RVV.Synthesizer.Operator.Scalar (sketchScalarLongImm)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode
      ( Add,
        And,
        Andn,
        CZeroEqz,
        CZeroNez,
        Div,
        Max,
        Maxu,
        Min,
        Minu,
        Mul,
        Mulh,
        Mulhsu,
        Mulhu,
        Or,
        Orn,
        Sll,
        Slt,
        Sltu,
        Sra,
        Srl,
        Sub,
        Xnor,
        Xor
      ),
  )
import RVV.Synthesizer.Parameter.SingleWidthIntUnaryOpCode
  ( SingleWidthIntUnaryOpCode (CPop, Clz, Ctz, Neg, Not, Seqz, Snez),
  )

getInferredSketch :: Int -> Bool -> Bool -> ConSymbolTable (WordN 8) -> T.Text -> SketchSpecTable (WordN 8)
getInferredSketch num hasDiv hasMul conTable symbol =
  let Right (TypeSignature argTypes resTypes) = symbolType conTable symbol
   in SymbolTable
        [ ( symbol,
            CompSpec $
              ComponentBag
                (toSym argTypes)
                [ ( simplifyChoiceTree $
                      Branch
                        (Split 0 False)
                        [ Branch
                            (Split 2 False)
                            [ Branch
                                (Split 4 False)
                                [ Branch
                                    (Split 9 False)
                                    [ Branch
                                        (Split 15 False)
                                        [ Leaf [sketchScalarUnary [Not] (1 / 2)],
                                          Leaf [sketchScalarUnary [Seqz] (1 / 2)],
                                          Leaf [sketchScalarUnary [Snez] (1 / 2)]
                                        ],
                                      Branch
                                        (Split 16 False)
                                        [ Leaf [sketchScalarUnary [CPop] (1 / 2)],
                                          Leaf [sketchScalarUnary [Clz] (1 / 2)],
                                          Leaf [sketchScalarUnary [Ctz] (1 / 2)]
                                        ]
                                    ],
                                  Branch
                                    (Split 8 False)
                                    [ Branch
                                        (Split 17 False)
                                        [ Leaf [sketchScalarBin [Sltu] (1 / 2) SketchScalarRHS],
                                          Leaf [sketchScalarBin [Slt] (1 / 2) SketchScalarRHS]
                                        ],
                                      Branch
                                        (Split 18 False)
                                        [ Leaf [sketchScalarBin [Max] (1 / 2) SketchScalarRHS],
                                          Leaf [sketchScalarBin [Min] (1 / 2) SketchScalarRHS],
                                          Leaf [sketchScalarBin [Maxu] (1 / 2) SketchScalarRHS],
                                          Leaf [sketchScalarBin [Minu] (1 / 2) SketchScalarRHS]
                                        ]
                                    ]
                                ],
                              Branch
                                (Split 3 False)
                                [ Branch
                                    (Split 5 False)
                                    [ Branch
                                        (Split 6 False)
                                        [ Branch
                                            (Split 13 False)
                                            [ Branch
                                                (Split 20 False)
                                                [ Leaf [sketchScalarBin [Sll] (1 / 2) SketchScalarRHS],
                                                  Leaf [sketchScalarBin [Sra] (1 / 2) SketchScalarRHS],
                                                  Leaf [sketchScalarBin [Srl] (1 / 2) SketchScalarRHS]
                                                ],
                                              Branch
                                                (Split 21 False)
                                                [ Leaf [sketchScalarBin [Sll] (1 / 2) (SketchImmRHS (BoundedImmSpec False 0 31))],
                                                  Leaf [sketchScalarBin [Sra] (1 / 2) (SketchImmRHS (BoundedImmSpec False 0 31))],
                                                  Leaf [sketchScalarBin [Srl] (1 / 2) (SketchImmRHS (BoundedImmSpec False 0 31))]
                                                ]
                                            ],
                                          Branch
                                            (Split 16 False)
                                            [ Leaf [sketchScalarUnary [Neg] (1 / 2)],
                                              Leaf [sketchScalarBin [Add] (1 / 2) SketchScalarRHS],
                                              Leaf [sketchScalarBin [Sub] (1 / 2) SketchScalarRHS],
                                              Leaf [sketchScalarBin [Add] (1 / 2) (SketchImmRHS (BoundedImmSpec True (-2) 2))]
                                            ]
                                        ],
                                      Branch
                                        (Split 7 False)
                                        [ Branch
                                            (Split 14 False)
                                            [ Leaf [sketchScalarBin [CZeroEqz] (1 / 2) SketchScalarRHS],
                                              Leaf [sketchScalarBin [CZeroNez] (1 / 2) SketchScalarRHS]
                                            ],
                                          Branch
                                            (Split 11 False)
                                            [ Branch
                                                (Split 22 False)
                                                [ Leaf [sketchScalarBin [And] (1 / 2) SketchScalarRHS],
                                                  Leaf [sketchScalarBin [Xor] (1 / 2) SketchScalarRHS],
                                                  Leaf [sketchScalarBin [Or] (1 / 2) SketchScalarRHS]
                                                ],
                                              Branch
                                                (Split 23 False)
                                                [ Leaf [sketchScalarBin [Andn] (1 / 2) SketchScalarRHS],
                                                  Leaf [sketchScalarBin [Xnor] (1 / 2) SketchScalarRHS],
                                                  Leaf [sketchScalarBin [Orn] (1 / 2) SketchScalarRHS]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ],
                          if hasDiv && hasMul
                            then
                              Branch
                                (Split 1 False)
                                [ Branch
                                    (Split 17 False)
                                    [ Leaf [sketchScalarBin [Mul] (1 / 2) SketchScalarRHS],
                                      Leaf [sketchScalarBin [Mulh] (1 / 2) SketchScalarRHS],
                                      Leaf [sketchScalarBin [Mulhu] (1 / 2) SketchScalarRHS],
                                      Leaf [sketchScalarBin [Mulhsu] (1 / 2) SketchScalarRHS]
                                    ],
                                  Leaf [sketchScalarBin [Div] (1 / 2) SketchScalarRHS]
                                ]
                            else
                              if hasDiv
                                then Leaf [sketchScalarBin [Div] (1 / 2) SketchScalarRHS]
                                else
                                  if hasMul
                                    then
                                      Branch
                                        (Split 17 False)
                                        [ Leaf [sketchScalarBin [Mul] (1 / 2) SketchScalarRHS],
                                          Leaf [sketchScalarBin [Mulh] (1 / 2) SketchScalarRHS],
                                          Leaf [sketchScalarBin [Mulhu] (1 / 2) SketchScalarRHS],
                                          Leaf [sketchScalarBin [Mulhsu] (1 / 2) SketchScalarRHS]
                                        ]
                                    else Leaf [],
                          Leaf [sketchScalarLongImm (1 / 2) ArbitraryImmSpec]
                        ],
                    num
                  )
                ]
                (toSym resTypes)
          )
        ]
