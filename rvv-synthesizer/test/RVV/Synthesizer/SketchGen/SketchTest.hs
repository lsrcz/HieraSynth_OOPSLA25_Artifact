{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module RVV.Synthesizer.SketchGen.SketchTest (sketchTest) where

{-
import Control.Monad (forM_)
import qualified Data.Text as T
import Grisette (PPrint (pformat), Solvable (con), WordN)
import qualified HieraSynth.Program.Concrete as Concrete
import HieraSynth.Util.Pretty (renderDoc)
import RVV.Semantics.VectorConfigConstants (vtypeEF2M1)
import RVV.Synthesizer.Parameter.Destination
  ( Destination (UseProvidedDest, UseUndefinedDest),
  )
import RVV.Synthesizer.ElementOp (SingleWidthBinOp (Add, Sub))
import RVV.Synthesizer.Parameter.Masking (Masking (UseFullMask, UseProvidedMask))
import RVV.Synthesizer.NestProg (ConNestOp (ConSimpleOp), ConNestProg)
import RVV.Synthesizer.Op (Op (MaskToVector))
import RVV.Synthesizer.SketchGen.Sketch
  ( ConSketchSpec,
    SketchOp (SketchNestedConProg, SketchNestedSketch, SketchSingleWidthVV),
    SketchSpec (SketchSpec),
  )
import RVV.Synthesizer.Type (ValueType (MaskType, VectorType))

data SketchPPrintTest where
  SketchPPrintTest ::
    (PPrint prog) =>
    { testName :: String,
      sketch :: prog,
      loose :: T.Text,
      compact :: Maybe T.Text
    } ->
    SketchPPrintTest
    -}

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

sketchTest :: Test
sketchTest =
  testGroup
    "Sketch"
    []

{-
sketchTest :: Test
sketchTest =
  testGroup
    "Sketch"
    [ testGroup "PPrint" $ do
        SketchPPrintTest {..} <-
          [ SketchPPrintTest
              { testName = "simple",
                sketch =
                  SketchSpec
                    "test"
                    [VectorType (con False) vtypeEF2M1]
                    [ ( [ SketchSingleWidthVV
                            vtypeEF2M1
                            [Add, Sub]
                            [UseUndefinedDest, UseProvidedDest]
                            [UseFullMask, UseProvidedMask]
                        ],
                        1
                      )
                    ]
                    [VectorType (con False) vtypeEF2M1] ::
                    SketchSpec (WordN 8),
                loose =
                  T.intercalate
                    "\n"
                    [ "sketch test(vec<ef2m1, v0=false>) -> vec<ef2m1, v0=false>:",
                      "  reorder {1 * v<add, sub>.vv[ef2m1, <ud, pd>, <fm, pm>]}"
                    ],
                compact =
                  Just $
                    T.intercalate
                      "\n"
                      [ "sketch test(",
                        "  vec<ef2m1, v0=...>",
                        ") -> vec<ef2m1, v0=...>:",
                        "  reorder {",
                        "    1 * v<",
                        "      add,",
                        "      sub",
                        "    >.vv[",
                        "      ef2m1,",
                        "      <",
                        "        ud,",
                        "        pd",
                        "      >,",
                        "      <",
                        "        fm,",
                        "        pm",
                        "      >",
                        "    ]",
                        "  }"
                      ]
              },
            SketchPPrintTest
              { testName = "nested",
                sketch =
                  let conNestProg =
                        ( Concrete.buildProg
                            "conNest"
                            [("a", MaskType False 2)]
                            $ \[a] ->
                              let [r] =
                                    Concrete.node
                                      ( ConSimpleOp $
                                          MaskToVector 2 vtypeEF2M1 False
                                      )
                                      1
                                      [a]
                               in [(r, VectorType False vtypeEF2M1)]
                        ) ::
                          ConNestProg (WordN 8)
                      inner =
                        SketchSpec
                          "test"
                          [MaskType (con False) 2]
                          [ ( [ SketchSingleWidthVV
                                  vtypeEF2M1
                                  [Add, Sub]
                                  [UseUndefinedDest, UseProvidedDest]
                                  [UseFullMask, UseProvidedMask],
                                SketchNestedConProg conNestProg
                              ],
                              2
                            )
                          ]
                          [VectorType (con False) vtypeEF2M1] ::
                          SketchSpec (WordN 8)
                   in ( Concrete.buildProg
                          "conSketch"
                          [("a", MaskType (con False) 2)]
                          $ \[a] ->
                            let [r] =
                                  Concrete.node
                                    [ SketchSingleWidthVV
                                        vtypeEF2M1
                                        [Add, Sub]
                                        [UseUndefinedDest, UseProvidedDest]
                                        [UseFullMask, UseProvidedMask],
                                      SketchNestedConProg conNestProg,
                                      SketchNestedSketch inner
                                    ]
                                    1
                                    [a]
                             in [(r, VectorType (con False) vtypeEF2M1)]
                      ) ::
                        ConSketchSpec (WordN 8),
                loose =
                  T.intercalate
                    "\n"
                    [ "def conNest(a: mask<mmul=2, v0=False>) -> "
                        <> "vec<ef2m1, v0=False>:",
                      "  v1 = mask_to_vector[mmul=2, dest=ef2m1, "
                        <> "dest_v0=False](a)",
                      "  return v1",
                      "sketch test(mask<mmul=2, v0=false>) -> vec<ef2m1, v0=false>:",
                      "  reorder {2 * <v<add, sub>.vv[ef2m1, <ud, pd>,"
                        <> " <fm, pm>], con_prog(conNest)>}",
                      "def conSketch(a: mask<mmul=2, v0=false>) -> "
                        <> "vec<ef2m1, v0=false>:",
                      "  r1 = <",
                      "    v<add, sub>.vv[ef2m1, <ud, pd>, <fm, pm>],",
                      "    con_prog(conNest),",
                      "    sketch(test)",
                      "  >(a)",
                      "  return r1"
                    ],
                compact = Nothing
              }
            ]
        return $ testCase testName $ do
          let actual = pformat sketch
          renderDoc 80 actual @?= loose
          forM_ compact (renderDoc 1 actual @?=)
    ]
-}
