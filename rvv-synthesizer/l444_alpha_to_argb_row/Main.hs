{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import RVV.App
  ( MainConfig
      ( MainConfig,
        defaultFuzzerMaxTests,
        defaultIRFile,
        defaultIRFuncName,
        defaultIRSpecScalingMethod,
        defaultImmScaleConfig,
        defaultLMulDownscaleRatio,
        defaultMachineConfigSpecs,
        defaultTemplateArgType,
        extraSketchArgsParser,
        maybeDefaultSpecification,
        overrideGenerators,
        sketchTable
      ),
    mainFunc,
  )
import RVV.Synthesizer.Feature.ToSketchOp (inferSketch)

{-
sketchTable0 :: ConSymbolTable (WordN 8) -> T.Text -> SketchSpecTable (WordN 8)
sketchTable0 conTable symbol =
  let Right (TypeSignature argTypes resTypes) = symbolType conTable symbol
      VectorType _ vtype = head argTypes
      mmul = vtypeMaskMultiplier vtype
      xmul = vtypeEewXlenMultiplier vtype
      vtypeX2 = widenVType vtype
      vtypeX4 = widenVType vtypeX2
   in SymbolTable
        [ ( symbol,
            CompSpec $
              ComponentBag
                (toSym argTypes)
                [ (Leaf [SketchVSetVL mmul [nonePolicy]], 1),
                  (Leaf [SketchScalarLong xmul []], 1),
                  ( Leaf
                      [ SketchWideningVX
                          vtypeX2
                          [WAddu, WSubu, WMulu]
                          [UseUndefinedDest]
                          [UseFullMask]
                      ],
                    1
                  ),
                  ( Leaf
                      [ SketchSingleWidthVI
                          vtypeX2
                          [Add, Sub, Mul, Mulhu]
                          [UseUndefinedDest]
                          [UseFullMask]
                          []
                      ],
                    1
                  ),
                  ( Leaf
                      [ SketchWideningVX
                          vtypeX4
                          [WAddu, WSubu, WMulu]
                          [UseUndefinedDest]
                          [UseFullMask]
                      ],
                    1
                  ),
                  ( Leaf
                      [ SketchNarrowingRightShiftWI
                          vtypeX2
                          [NSrl]
                          [UseUndefinedDest]
                          [UseFullMask]
                          []
                      ],
                    1
                  ),
                  ( Leaf
                      [ SketchWideningMultiplyAddVX
                          vtypeX2
                          [WMAccu]
                          [UseFullMask]
                      ],
                    1
                  ),
                  ( Leaf
                      [ SketchSingleWidthVX
                          vtypeX2
                          [SSubu, SAddu]
                          [UseUndefinedDest]
                          [UseFullMask]
                      ],
                    1
                  ),
                  ( Leaf
                      [ SketchFixedPointClipWI
                          vtype
                          (Just False)
                          [FixedRNU]
                          [UseUndefinedDest]
                          [UseFullMask]
                          []
                      ],
                    1
                  )
                ]
                (toSym resTypes)
          )
        ]

sketchTable1 :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
sketchTable1 conTable symbol _ =
  let Right (TypeSignature argTypes resTypes) = symbolType conTable symbol
      VectorType _ vtype = head argTypes
      mmul = vtypeMaskMultiplier vtype
      xmul = vtypeEewXlenMultiplier vtype
      xmulX2 = xmul * 2
      xmulX4 = xmul * 4
      vtypeX2 = widenVType vtype
      vtypeX4 = widenVType vtypeX2
   in SymbolTable
        [ ( symbol,
            CompSpec $
              ComponentBag
                (toSym argTypes)
                [ (Leaf [SketchVSetVL mmul [nonePolicy]], 1),
                  ( Branch
                      (Split 0 False)
                      [ Branch
                          (Split 1 False)
                          [ Leaf [SketchScalarLong xmul []],
                            Leaf [SketchScalarLong xmulX2 []],
                            Leaf [SketchScalarLong xmulX4 []]
                          ],
                        Branch
                          (Split 2 False)
                          [ Leaf
                              [ SketchWideningVX
                                  vtypeX2
                                  [WMulu]
                                  [UseUndefinedDest]
                                  [UseFullMask],
                                SketchWideningMultiplyAddVX
                                  vtypeX2
                                  [WMAccu]
                                  [UseFullMask],
                                SketchWideningVX
                                  vtypeX2
                                  [WAddu, WSubu]
                                  [UseUndefinedDest]
                                  [UseFullMask]
                              ],
                            Leaf
                              [ SketchWideningVX
                                  vtypeX4
                                  [WMulu]
                                  [UseUndefinedDest]
                                  [UseFullMask],
                                SketchWideningMultiplyAddVX
                                  vtypeX4
                                  [WMAccu]
                                  [UseFullMask],
                                SketchWideningVX
                                  vtypeX4
                                  [WAddu, WSubu]
                                  [UseUndefinedDest]
                                  [UseFullMask]
                              ]
                          ],
                        Branch
                          (Split 1 False)
                          [ Branch
                              (Split 3 False)
                              [ Leaf
                                  [ SketchSingleWidthVX
                                      vtypeX2
                                      [SSubu, SAddu, Add, Sub]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                  ],
                                Leaf
                                  [ SketchSingleWidthVI
                                      vtypeX2
                                      [Mul, Mulhu]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                      []
                                  ]
                              ],
                            Branch
                              (Split 4 False)
                              [ Leaf
                                  [ SketchSingleWidthVX
                                      vtype
                                      [SSubu, SAddu, Add, Sub]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                  ],
                                Leaf
                                  [ SketchSingleWidthVI
                                      vtype
                                      [Mul, Mulhu]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                      []
                                  ]
                              ]
                          ],
                        Branch
                          (Split 5 False)
                          [ Leaf
                              [ SketchNarrowingRightShiftWI
                                  vtype
                                  [NSrl]
                                  [UseUndefinedDest]
                                  [UseFullMask]
                                  [],
                                SketchFixedPointClipWI
                                  vtype
                                  (Just False)
                                  [FixedRNU]
                                  [UseUndefinedDest]
                                  [UseFullMask]
                                  []
                              ],
                            Leaf
                              [ SketchNarrowingRightShiftWI
                                  vtypeX2
                                  [NSrl]
                                  [UseUndefinedDest]
                                  [UseFullMask]
                                  [],
                                SketchFixedPointClipWI
                                  vtypeX2
                                  (Just False)
                                  [FixedRNU]
                                  [UseUndefinedDest]
                                  [UseFullMask]
                                  []
                              ]
                          ]
                      ],
                    8
                  )
                ]
                (toSym resTypes)
          )
        ]

sketchTable2 :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
sketchTable2 conTable symbol _ =
  let Right (TypeSignature argTypes resTypes) = symbolType conTable symbol
      VectorType _ vtype = head argTypes
      mmul = vtypeMaskMultiplier vtype
      xmul = vtypeEewXlenMultiplier vtype
      xmulX2 = xmul * 2
      xmulX4 = xmul * 4
      vtypeX2 = widenVType vtype
      vtypeX4 = widenVType vtypeX2
   in SymbolTable
        [ ( symbol,
            CompSpec $
              ComponentBag
                (toSym argTypes)
                [ (Leaf [SketchVSetVL mmul [nonePolicy]], 1),
                  ( Branch
                      (Split 0 False)
                      [ Branch
                          (Split 2 False)
                          [ Leaf [SketchScalarLong xmul []],
                            Leaf [SketchScalarLong xmulX2 []],
                            Leaf [SketchScalarLong xmulX4 []]
                          ],
                        Branch
                          (Split 1 False)
                          [ Branch
                              (Split 3 False)
                              [ Branch
                                  (Split 4 False)
                                  [ Leaf
                                      [ SketchWideningVX
                                          vtypeX2
                                          [WMulu]
                                          [UseUndefinedDest]
                                          [UseFullMask],
                                        SketchWideningMultiplyAddVX
                                          vtypeX2
                                          [WMAccu]
                                          [UseFullMask],
                                        SketchWideningVX
                                          vtypeX2
                                          [WAddu, WSubu]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                      ],
                                    Leaf
                                      [ SketchSingleWidthVX
                                          vtype
                                          [SSubu, SAddu, Add, Sub]
                                          [UseUndefinedDest]
                                          [UseFullMask],
                                        SketchSingleWidthVI
                                          vtype
                                          [Mul, Mulhu]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                          []
                                      ]
                                  ],
                                Leaf
                                  [ SketchNarrowingRightShiftWI
                                      vtype
                                      [NSrl]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                      [],
                                    SketchFixedPointClipWI
                                      vtype
                                      (Just False)
                                      [FixedRNU]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                      []
                                  ]
                              ],
                            Branch
                              (Split 3 False)
                              [ Branch
                                  (Split 4 False)
                                  [ Leaf
                                      [ SketchWideningVX
                                          vtypeX4
                                          [WMulu]
                                          [UseUndefinedDest]
                                          [UseFullMask],
                                        SketchWideningMultiplyAddVX
                                          vtypeX4
                                          [WMAccu]
                                          [UseFullMask],
                                        SketchWideningVX
                                          vtypeX4
                                          [WAddu, WSubu]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                      ],
                                    Leaf
                                      [ SketchSingleWidthVX
                                          vtypeX2
                                          [SSubu, SAddu, Add, Sub]
                                          [UseUndefinedDest]
                                          [UseFullMask],
                                        SketchSingleWidthVI
                                          vtypeX2
                                          [Mul, Mulhu]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                          []
                                      ]
                                  ],
                                Leaf
                                  [ SketchNarrowingRightShiftWI
                                      vtypeX2
                                      [NSrl]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                      [],
                                    SketchFixedPointClipWI
                                      vtypeX2
                                      (Just False)
                                      [FixedRNU]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                      []
                                  ]
                              ]
                          ]
                      ],
                    8
                  )
                ]
                (toSym resTypes)
          )
        ]

sketchTable2' :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
sketchTable2' conTable symbol _ =
  let Right (TypeSignature argTypes resTypes) = symbolType conTable symbol
      VectorType _ vtype = head argTypes
      mmul = vtypeMaskMultiplier vtype
      xmul = vtypeEewXlenMultiplier vtype
      xmulX2 = xmul * 2
      xmulX4 = xmul * 4
      vtypeX2 = widenVType vtype
      vtypeX4 = widenVType vtypeX2
      singleWidthVVLinearOps = [SSubu, Add, Sll, SAddu, Sub, Srl]
      singleWidthVXLinearOps = [SSubu, Add, Sll, SAddu, RSub, Sub, Srl]
      singleWidthMulOps = [Mul, Mulhu]
      singleWidthList vtype n =
        Branch
          (Split n False)
          [singleWidthLinearList vtype, singleWidthMulList vtype]
      dest = [UseUndefinedDest]
      singleWidthLinearList vtype =
        Leaf
          [ SketchSingleWidthVV
              vtype
              singleWidthVVLinearOps
              dest
              [UseFullMask],
            SketchSingleWidthVX
              vtype
              singleWidthVXLinearOps
              dest
              [UseFullMask],
            SketchSingleWidthVI
              vtype
              singleWidthVXLinearOps
              dest
              [UseFullMask]
              []
          ]
      singleWidthMulList vtype =
        Leaf
          [ SketchSingleWidthVV
              vtype
              singleWidthMulOps
              dest
              [UseFullMask],
            SketchSingleWidthVX
              vtype
              singleWidthMulOps
              dest
              [UseFullMask],
            SketchSingleWidthVI
              vtype
              singleWidthMulOps
              dest
              [UseFullMask]
              []
          ]
      wideningMulList vtype =
        Leaf
          [ SketchWideningVX
              vtype
              [WMulu]
              dest
              [UseFullMask],
            SketchWideningVV
              vtype
              [WMulu]
              dest
              [UseFullMask],
            SketchWideningWV
              vtype
              [WMulu]
              dest
              [UseFullMask],
            SketchWideningWX
              vtype
              [WMulu]
              dest
              [UseFullMask],
            SketchWideningMultiplyAddVV
              vtype
              [WMAccu]
              [UseFullMask],
            SketchWideningMultiplyAddVX
              vtype
              [WMAccu]
              [UseFullMask]
          ]
      wideningLinearList vtype =
        Leaf
          [ SketchWideningVX
              vtype
              [WAddu, WSubu]
              dest
              [UseFullMask],
            SketchWideningVV
              vtype
              [WAddu, WSubu]
              dest
              [UseFullMask],
            SketchWideningWV
              vtype
              [WAddu, WSubu]
              dest
              [UseFullMask],
            SketchWideningWX
              vtype
              [WAddu, WSubu]
              dest
              [UseFullMask]
          ]
      wideningList vtype n =
        Branch (Split n False) [wideningLinearList vtype, wideningMulList vtype]
      narrowingList vtype n =
        Branch
          (Split n False)
          [ Leaf
              [ SketchNarrowingRightShiftWV
                  vtype
                  [NSrl]
                  dest
                  [UseFullMask],
                SketchNarrowingRightShiftWX
                  vtype
                  [NSrl]
                  dest
                  [UseFullMask],
                SketchNarrowingRightShiftWI
                  vtype
                  [NSrl]
                  dest
                  [UseFullMask]
                  []
              ],
            Leaf
              [ SketchFixedPointClipWV
                  vtype
                  (Just False)
                  [FixedRNU]
                  dest
                  [UseFullMask],
                SketchFixedPointClipWX
                  vtype
                  (Just False)
                  [FixedRNU]
                  dest
                  [UseFullMask],
                SketchFixedPointClipWI
                  vtype
                  (Just False)
                  [FixedRNU]
                  dest
                  [UseFullMask]
                  []
              ]
          ]
   in SymbolTable
        [ ( symbol,
            CompSpec $
              ComponentBag
                (toSym argTypes)
                [ (Leaf [SketchVSetVL mmul [nonePolicy]], 1),
                  ( Branch
                      (Split 0 False)
                      [ Branch
                          (Split 6 False)
                          [ Leaf [SketchScalarLong xmul []],
                            Leaf [SketchScalarLong xmulX2 []],
                            Leaf [SketchScalarLong xmulX4 []]
                          ],
                        Branch
                          (Split 1 False)
                          [ Branch
                              (Split 2 False)
                              [ Branch
                                  (Split 5 False)
                                  [ singleWidthList vtype 8,
                                    narrowingList vtype 8
                                  ],
                                Branch
                                  (Split 4 False)
                                  [ wideningList vtypeX2 9,
                                    singleWidthList vtypeX2 9,
                                    narrowingList vtypeX2 9
                                  ]
                              ]
                          ],
                        Branch
                          (Split 3 False)
                          [ wideningList vtypeX4 7,
                            singleWidthList vtypeX4 7
                          ]
                      ],
                    7
                  )
                ]
                (toSym resTypes)
          )
        ]

sketchTable3 :: ConSymbolTable (WordN 8) -> T.Text -> [TemplateArgType] -> SketchSpecTable (WordN 8)
sketchTable3 conTable symbol _ =
  let Right (TypeSignature argTypes resTypes) = symbolType conTable symbol
      VectorType _ vtype = head argTypes
      mmul = vtypeMaskMultiplier vtype
      xmul = vtypeEewXlenMultiplier vtype
      xmulX2 = xmul * 2
      xmulX4 = xmul * 4
      vtypeX2 = widenVType vtype
      vtypeX4 = widenVType vtypeX2
   in SymbolTable
        [ ( symbol,
            CompSpec $
              ComponentBag
                (toSym argTypes)
                [ (Leaf [SketchVSetVL mmul [nonePolicy]], 1),
                  ( Branch
                      (Split 0 False)
                      [ Branch
                          (Split 2 False)
                          [ Leaf [SketchScalarLong xmul []],
                            Leaf [SketchScalarLong xmulX2 []],
                            Leaf [SketchScalarLong xmulX4 []]
                          ],
                        Branch
                          (Split 1 False)
                          [ Branch
                              (Split 3 False)
                              [ Branch
                                  (Split 6 False)
                                  [ Leaf
                                      [ SketchWideningVX
                                          vtypeX2
                                          [WMulu]
                                          [UseUndefinedDest]
                                          [UseFullMask],
                                        SketchWideningMultiplyAddVX
                                          vtypeX2
                                          [WMAccu]
                                          [UseFullMask],
                                        SketchWideningVX
                                          vtypeX2
                                          [WAddu, WSubu]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                      ],
                                    Leaf
                                      [ SketchWideningVX
                                          vtypeX4
                                          [WMulu]
                                          [UseUndefinedDest]
                                          [UseFullMask],
                                        SketchWideningMultiplyAddVX
                                          vtypeX4
                                          [WMAccu]
                                          [UseFullMask],
                                        SketchWideningVX
                                          vtypeX4
                                          [WAddu, WSubu]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                      ]
                                  ],
                                Branch
                                  (Split 5 False)
                                  [ Leaf
                                      [ SketchNarrowingRightShiftWI
                                          vtype
                                          [NSrl]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                          [],
                                        SketchFixedPointClipWI
                                          vtype
                                          (Just False)
                                          [FixedRNU]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                          []
                                      ],
                                    Leaf
                                      [ SketchNarrowingRightShiftWI
                                          vtypeX2
                                          [NSrl]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                          [],
                                        SketchFixedPointClipWI
                                          vtypeX2
                                          (Just False)
                                          [FixedRNU]
                                          [UseUndefinedDest]
                                          [UseFullMask]
                                          []
                                      ]
                                  ]
                              ],
                            Branch
                              (Split 4 False)
                              [ Leaf
                                  [ SketchSingleWidthVX
                                      vtype
                                      [SSubu, SAddu, Add, Sub]
                                      [UseUndefinedDest]
                                      [UseFullMask],
                                    SketchSingleWidthVI
                                      vtype
                                      [Mul, Mulhu]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                      []
                                  ],
                                Leaf
                                  [ SketchSingleWidthVX
                                      vtypeX2
                                      [SSubu, SAddu, Add, Sub]
                                      [UseUndefinedDest]
                                      [UseFullMask],
                                    SketchSingleWidthVI
                                      vtypeX2
                                      [Mul, Mulhu]
                                      [UseUndefinedDest]
                                      [UseFullMask]
                                      []
                                  ]
                              ]
                          ]
                      ],
                    8
                  )
                ]
                (toSym resTypes)
          )
        ]
        -}

main :: IO ()
main =
  mainFunc $
    MainConfig
      { defaultIRFile = "l444_alpha_to_argb_row/l444_alpha_to_argb_row.sir",
        defaultIRFuncName = "I444AlphaToARGBRow_RVV",
        defaultTemplateArgType = Nothing,
        defaultIRSpecScalingMethod = "no",
        defaultMachineConfigSpecs = "vlen32f2;vlen64,vlen128,vlen256,vlen512",
        defaultImmScaleConfig = Just "I444AlphaToARGBRow_RVV:[1:17~257|1:3~6|1:8~16]",
        defaultLMulDownscaleRatio = 16,
        maybeDefaultSpecification = Nothing,
        overrideGenerators = Nothing,
        defaultFuzzerMaxTests = 1000,
        sketchTable = \_ conTable sketchSymbol _ ->
          inferSketch (1 / 8) 7 conTable sketchSymbol id,
        extraSketchArgsParser = pure ()
      }
