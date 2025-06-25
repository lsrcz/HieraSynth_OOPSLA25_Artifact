{-# LANGUAGE FunctionalDependencies #-}

module RVV.Synthesizer.Lens
  ( -- ** vectorConfig
    HasVectorConfig (..),
    HasVectorConfigWide (..),
    HasVectorConfigNarrow (..),
    HasVectorConfigPart (..),
    HasVectorConfigDest (..),
    HasVectorConfigSrc (..),

    -- ** mmul
    HasMaskMul (..),
    HasMaskMulDest (..),
    HasMaskMulSrc (..),
    HasMaskMulPart (..),

    -- ** widthMul
    HasWidthMulDelegated (..),
    HasWidthMul (..),
    HasWidthMulDest (..),
    HasWidthMulSrc (..),

    -- ** misc
    HasImm (..),
    HasPolicy (..),
    HasDestination (..),
    HasMasking (..),
    HasBlockId (..),
    HasRhs (..),

    -- ** Operator specific
    HasSingleWidthIntBinaryOpCode (..),
    HasIndex (..),
    HasSetMaskMethod (..),
    HasWideningIntBinaryOpCode (..),
    HasNarrowingRightShiftOpCode (..),
    HasFixedPointRoundingMode (..),
    HasSingleWidthMulAddOpCode (..),
    HasWideningMulAddOpCode (..),
    HasSlideDirection (..),
    HasIntCompareOpCode (..),
    HasSigned (..),
    HasSingleWidthIntUnaryOpCode (..),
    HasMaskLogicalOpCode (..),
    HasHasWideLhs (..),
  )
where

import Control.Lens (Traversal')

class HasVectorConfig s a | s -> a where
  vectorConfig :: Traversal' s a

class HasVectorConfigWide s a | s -> a where
  vectorConfigWide :: Traversal' s a

class HasVectorConfigNarrow s a | s -> a where
  vectorConfigNarrow :: Traversal' s a

class HasVectorConfigPart s a | s -> a where
  vectorConfigPart :: Traversal' s a

class HasVectorConfigDest s a | s -> a where
  vectorConfigDest :: Traversal' s a

class HasVectorConfigSrc s a | s -> a where
  vectorConfigSrc :: Traversal' s a

class HasMaskMul s a | s -> a where
  maskMul :: Traversal' s a

class HasMaskMulDest s a | s -> a where
  maskMulDest :: Traversal' s a

class HasMaskMulSrc s a | s -> a where
  maskMulSrc :: Traversal' s a

class HasMaskMulPart s a | s -> a where
  maskMulPart :: Traversal' s a

class HasWidthMulDelegated s a | s -> a where
  widthMulDelegated :: Traversal' s a

class HasWidthMul s a | s -> a where
  widthMul :: Traversal' s a

class HasWidthMulDest s a | s -> a where
  widthMulDest :: Traversal' s a

class HasWidthMulSrc s a | s -> a where
  widthMulSrc :: Traversal' s a

class HasImm s a | s -> a where
  imm :: Traversal' s a

class HasPolicy s a | s -> a where
  policy :: Traversal' s a

class HasDestination s a | s -> a where
  destination :: Traversal' s a

class HasMasking s a | s -> a where
  masking :: Traversal' s a

class HasBlockId s a | s -> a where
  blockId :: Traversal' s a

class HasSingleWidthIntBinaryOpCode s a | s -> a where
  singleWidthIntBinaryOpCode :: Traversal' s a

class HasIndex s a | s -> a where
  index :: Traversal' s a

class HasSetMaskMethod s a | s -> a where
  setMaskMethod :: Traversal' s a

class HasWideningIntBinaryOpCode s a | s -> a where
  wideningIntBinaryOpCode :: Traversal' s a

class HasNarrowingRightShiftOpCode s a | s -> a where
  narrowingRightShiftOpCode :: Traversal' s a

class HasFixedPointRoundingMode s a | s -> a where
  fixedPointRoundingMode :: Traversal' s a

class HasSingleWidthMulAddOpCode s a | s -> a where
  singleWidthMulAddOpCode :: Traversal' s a

class HasWideningMulAddOpCode s a | s -> a where
  wideningMulAddOpCode :: Traversal' s a

class HasSlideDirection s a | s -> a where
  slideDirection :: Traversal' s a

class HasIntCompareOpCode s a | s -> a where
  intCompareOpCode :: Traversal' s a

class HasSigned s a | s -> a where
  signed :: Traversal' s a

class HasSingleWidthIntUnaryOpCode s a | s -> a where
  singleWidthIntUnaryOpCode :: Traversal' s a

class HasRhs s a | s -> a where
  rhs :: Traversal' s a

class HasMaskLogicalOpCode s a | s -> a where
  maskLogicalOpCode :: Traversal' s a

class HasHasWideLhs s a | s -> a where
  hasWideLhs :: Traversal' s a
