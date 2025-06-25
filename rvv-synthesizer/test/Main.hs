module Main (main) where

import RVV.Circuit.ParallelFoldableTest (parallelFoldableTest)
import RVV.Semantics.BitSizeUtilTest (bitSizeUtilTest)
import RVV.Semantics.ElementTest (elemTest)
import RVV.Semantics.PrimOp.BroadcastTest (broadcastTest)
import qualified RVV.Semantics.PrimOp.CompareTest as Semantics
import RVV.Semantics.PrimOp.ElementWiseTest (elementWiseTest)
import RVV.Semantics.PrimOp.FullMaskTest (fullMaskTest)
import RVV.Semantics.PrimOp.GetVLTest (getVLTest)
import RVV.Semantics.PrimOp.ImmToRegTest (immToRegTest)
import qualified RVV.Semantics.PrimOp.IotaTest as Semantics
import qualified RVV.Semantics.PrimOp.MaskLogicalTest as Semantics
import qualified RVV.Semantics.PrimOp.MergeTest as Semantics
import qualified RVV.Semantics.PrimOp.MiscMaskTest as Semantics
import qualified RVV.Semantics.PrimOp.MulAddTest as Semantics
import qualified RVV.Semantics.PrimOp.SlideTest as Semantics
import RVV.Semantics.PrimOp.UndefinedTest (undefinedTest)
import qualified RVV.Semantics.PrimOp.UnitStrideLoadStoreTest as Semantics
import RVV.Semantics.PrimOp.UtilTest (utilTest)
import qualified RVV.Semantics.PrimOp.VGetTest as Semantics
import qualified RVV.Semantics.PrimOp.VSetTest as Semantics
import RVV.Semantics.SizeConstraintTest (sizeConstraintTest)
import RVV.Semantics.VectorConfigTest (vectorConfigTest)
import RVV.Synthesizer.ElementOpTest (elementOpTest)
import RVV.Synthesizer.GeneratorTest (generatorTest)
import RVV.Synthesizer.InterpreterTest (interpreterTest)
import RVV.Synthesizer.MatcherTest (matcherTest)
import qualified RVV.Synthesizer.OpSemantics.CompareTest as Synthesizer
import qualified RVV.Synthesizer.OpSemantics.ConvertTest as Synthesizer
import RVV.Synthesizer.OpSemantics.FixedPointClipTest (fixedPointClipTest)
import qualified RVV.Synthesizer.OpSemantics.IotaTest as Synthesizer
import RVV.Synthesizer.OpSemantics.MaskElementWiseTest (maskElementWiseTest)
import qualified RVV.Synthesizer.OpSemantics.MaskLogicalTest as Synthesizer
import qualified RVV.Synthesizer.OpSemantics.MergeTest as Synthesizer
import qualified RVV.Synthesizer.OpSemantics.MiscMaskTest as Synthesizer
import RVV.Synthesizer.OpSemantics.MiscTest (miscTest)
import RVV.Synthesizer.OpSemantics.MoveTest (moveTest)
import qualified RVV.Synthesizer.OpSemantics.MulAddTest as Synthesizer
import RVV.Synthesizer.OpSemantics.NarrowingRightShiftTest (narrowingRightShiftTest)
import RVV.Synthesizer.OpSemantics.ReinterpretTest (reinterpretTest)
import RVV.Synthesizer.OpSemantics.ScalarTest (scalarTest)
import RVV.Synthesizer.OpSemantics.SingleWidthTest (singleWidthTest)
import qualified RVV.Synthesizer.OpSemantics.SlideTest as Synthesizer
-- import qualified RVV.Synthesizer.OpSemantics.UnitStrideLoadStoreTest as Synthesizer
import qualified RVV.Synthesizer.OpSemantics.VGetTest as Synthesizer
import qualified RVV.Synthesizer.OpSemantics.VSetTest as Synthesizer
import RVV.Synthesizer.OpSemantics.VlenbTest (vlenbTest)
import RVV.Synthesizer.OpSemantics.WideningTest (wideningTest)
import RVV.Synthesizer.PrettyPrintingTest (prettyPrintingTest)
import qualified RVV.Synthesizer.SketchGen.SketchTest as SketchGen
import RVV.Synthesizer.SketchGen.SplitChoiceTest (splitChoiceTest)
import RVV.Synthesizer.SketchTest (sketchTest)
import RVV.Synthesizer.Specification.ScaleTest (scaleTest)
import RVV.Synthesizer.TypeTest (typeTest)
import RVV.Synthesizer.ValueTest (valueTest)
import Test.Framework (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain
    [ testGroup "Circuit" [parallelFoldableTest],
      testGroup
        "Semantics"
        [ elemTest,
          sizeConstraintTest,
          vectorConfigTest,
          bitSizeUtilTest,
          testGroup
            "PrimOp"
            [ utilTest,
              getVLTest,
              fullMaskTest,
              undefinedTest,
              broadcastTest,
              elementWiseTest,
              Semantics.unitStrideLoadStoreTest,
              immToRegTest,
              Semantics.mulAddTest,
              Semantics.slideTest,
              Semantics.vsetTest,
              Semantics.vgetTest,
              Semantics.compareTest,
              Semantics.maskLogicalTest,
              Semantics.mergeTest,
              Semantics.iotaTest,
              Semantics.miscMaskTest
            ]
        ],
      testGroup
        "Synthesizer"
        [ valueTest,
          prettyPrintingTest,
          typeTest,
          sketchTest,
          matcherTest,
          interpreterTest,
          generatorTest,
          elementOpTest,
          testGroup
            "OpSemantics"
            [ Synthesizer.compareTest,
              Synthesizer.convertTest,
              maskElementWiseTest,
              Synthesizer.iotaTest,
              Synthesizer.maskLogicalTest,
              Synthesizer.mergeTest,
              miscTest,
              moveTest,
              Synthesizer.mulAddTest,
              reinterpretTest,
              scalarTest,
              singleWidthTest,
              -- Synthesizer.unitStrideLoadStoreTest,
              Synthesizer.slideTest,
              Synthesizer.vgetTest,
              Synthesizer.vsetTest,
              vlenbTest,
              wideningTest,
              Synthesizer.miscMaskTest,
              narrowingRightShiftTest,
              fixedPointClipTest
            ],
          testGroup "SketchGen" [SketchGen.sketchTest, splitChoiceTest],
          testGroup "Scale" [scaleTest]
        ]
    ]
