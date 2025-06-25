{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Util.PrettyTest (prettyTest) where

import qualified Data.Text as T
import HieraSynth.Util.Pretty
  ( encloseList,
    encloseListIfNotSingle,
    parenCommaList,
    parenCommaListIfNotSingle,
    renderDoc,
  )
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@?=))

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter (Doc, group, vsep)
#else
import Data.Text.Prettyprint.Doc (Doc, group, vsep)
#endif

enclose :: [Doc ann] -> Doc ann
enclose = encloseList "[" "]" ","

encloseIfNotSingle :: [Doc ann] -> Doc ann
encloseIfNotSingle = encloseListIfNotSingle "[" "]" ","

prettyTest :: Test
prettyTest =
  testGroup
    "HieraSynth.Util.Pretty"
    [ testGroup
        "renderDoc"
        [ testCase "1" $ renderDoc 1 (group $ vsep ["a", "b"]) @?= "a\nb",
          testCase "80" $ renderDoc 80 (group $ vsep ["a", "b"]) @?= "a b"
        ],
      testGroup
        "encloseList"
        [ testGroup
            "loose"
            [ testCase "empty" $
                renderDoc 80 (enclose []) @?= "[]",
              testCase "single" $
                renderDoc 80 (enclose ["a"]) @?= "[a]",
              testCase "multiple" $
                renderDoc 80 (enclose ["a", "b"]) @?= "[a, b]"
            ],
          testGroup
            "compact"
            [ testCase "empty" $
                renderDoc 1 (enclose []) @?= "[]",
              testCase "single" $
                renderDoc 1 (enclose ["a"]) @?= "[\n  a\n]",
              testCase "multiple" $ do
                let actual = renderDoc 1 (enclose ["a", "b"])
                actual @?= "[\n  a,\n  b\n]"
            ]
        ],
      testGroup
        "encloseListIfNotSingle"
        [ testGroup
            "loose"
            [ testCase "empty" $
                renderDoc 80 (encloseIfNotSingle []) @?= "[]",
              testCase "single" $
                renderDoc 80 (encloseIfNotSingle ["a"]) @?= "a",
              testCase "multiple" $
                renderDoc 80 (encloseIfNotSingle ["a", "b"]) @?= "[a, b]"
            ],
          testGroup
            "compact"
            [ testCase "empty" $
                renderDoc 1 (encloseIfNotSingle []) @?= "[]",
              testCase "single" $
                renderDoc 1 (encloseIfNotSingle ["a"]) @?= "a",
              testCase "multiple" $ do
                let actual = renderDoc 1 (encloseIfNotSingle ["a", "b"])
                actual @?= "[\n  a,\n  b\n]"
            ]
        ],
      testGroup
        "nested"
        [ testCase "loose" $ do
            let actual = renderDoc 80 (enclose ["a", enclose ["b", "c"], "d"])
            actual @?= "[a, [b, c], d]",
          testCase "compact" $ do
            let actual = renderDoc 1 (enclose ["a", enclose ["b", "c"], "d"])
            actual
              @?= T.intercalate
                "\n"
                [ "[",
                  "  a,",
                  "  [",
                  "    b,",
                  "    c",
                  "  ],",
                  "  d",
                  "]"
                ]
        ],
      testCase "prefix & postfix" $ do
        let actual = renderDoc 6 ("x" <> enclose ["a", "b", "c"] <> "y")
        actual
          @?= T.intercalate
            "\n"
            [ "x[",
              "  a,",
              "  b,",
              "  c",
              "]y"
            ],
      testCase "parenCommaListIfNotSingle" $ do
        renderDoc 80 (parenCommaListIfNotSingle []) @?= "()"
        renderDoc 80 (parenCommaListIfNotSingle ["a"]) @?= "a"
        renderDoc 80 (parenCommaListIfNotSingle ["a", "b", "c"])
          @?= "(a, b, c)",
      testCase "parenCommaList" $ do
        renderDoc 80 (parenCommaList []) @?= "()"
        renderDoc 80 (parenCommaList ["a"]) @?= "(a)"
        renderDoc 80 (parenCommaList ["a", "b", "c"]) @?= "(a, b, c)"
    ]
