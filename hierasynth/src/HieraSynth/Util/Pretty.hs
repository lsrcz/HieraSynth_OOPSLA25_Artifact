{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module HieraSynth.Util.Pretty
  ( encloseListIfNotSingle,
    encloseList,
    renderDoc,
    Doc,
    (<+>),
    nest,
    concatWith,
    hardline,
    parenCommaListIfNotSingle,
    parenCommaList,
    vsep,
    vcat,
  )
where

import qualified Data.Text as T

#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
  ( Doc,
    LayoutOptions (LayoutOptions, layoutPageWidth),
    PageWidth (AvailablePerLine),
    cat,
    flatAlt,
    group,
    layoutPretty,
    nest,
    unAnnotate,
    vcat,
    vsep,
    (<+>),
    concatWith,
    hardline,
  )
import Prettyprinter.Render.Text (renderStrict)
#else
import Data.Text.Prettyprint.Doc
  ( Doc,
    LayoutOptions (LayoutOptions, layoutPageWidth),
    PageWidth (AvailablePerLine),
    cat,
    flatAlt,
    group,
    layoutPretty,
    nest,
    unAnnotate,
    vcat,
    vsep,
    (<+>),
    concatWith,
    hardline,
  )
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
#endif

encloseListIfNotSingle :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseListIfNotSingle l r s ds = case ds of
  [d] -> d
  _ -> encloseList l r s ds

encloseList :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseList l r s ds = case ds of
  [] -> l <> r
  [d] -> cat [nest 2 $ vcat [l, d], r]
  _ ->
    group $
      vcat [nest 2 $ vcat [l, vcat $ map (<> sep) (init ds), last ds], r]
  where
    sep = flatAlt s (s <> " ")

parenCommaListIfNotSingle :: [Doc ann] -> Doc ann
parenCommaListIfNotSingle = encloseListIfNotSingle "(" ")" ","

parenCommaList :: [Doc ann] -> Doc ann
parenCommaList = encloseList "(" ")" ","

renderDoc :: Int -> Doc ann -> T.Text
renderDoc w doc = renderStrict $ layoutPretty layoutOptions (unAnnotate doc)
  where
    layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine w 1}
