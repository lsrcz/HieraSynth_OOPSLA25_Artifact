{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.CostModel.CostModel
  ( CostModel (..),
  )
where

import RVV.Semantics.MachineConfig (MachineConfig)
import RVV.Semantics.Multiplier (MaskMul, WidthMul)
import RVV.Semantics.VectorConfig (VectorConfig)
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode,
  )
import RVV.Synthesizer.Parameter.SingleWidthIntUnaryOpCode
  ( SingleWidthIntUnaryOpCode,
  )
import RVV.Synthesizer.Parameter.SlideDirection
  ( SlideDirection,
  )
import RVV.Synthesizer.Parameter.WideningIntBinaryOpCode
  ( WideningIntBinaryOpCode,
  )
import RVV.Synthesizer.Specification.ScaleLMul (ScaleLMul (scaleLMul))

data CostModel = CostModel
  { modelMachineConfig :: MachineConfig,
    singleWidthCost :: SingleWidthIntBinaryOpCode -> VectorConfig -> Int,
    scalarBinCost :: SingleWidthIntBinaryOpCode -> WidthMul -> Int,
    scalarUnaryCost :: SingleWidthIntUnaryOpCode -> WidthMul -> Int,
    wideningCost :: WideningIntBinaryOpCode -> VectorConfig -> Int,
    slideCost :: SlideDirection -> VectorConfig -> Int,
    slide1Cost :: SlideDirection -> VectorConfig -> Int,
    compareCost :: VectorConfig -> Int,
    maskOpCost :: MaskMul -> Int,
    mergeCost :: VectorConfig -> Int,
    iotaCost :: VectorConfig -> Int,
    idCost :: VectorConfig -> Int,
    broadcastCost :: VectorConfig -> Int,
    scalarMoveCost :: VectorConfig -> Int,
    narrowingCost :: VectorConfig -> Int,
    vsetCost :: VectorConfig -> VectorConfig -> Int
  }

instance ScaleLMul CostModel where
  scaleLMul ratio CostModel {..} =
    CostModel
      { modelMachineConfig,
        singleWidthCost = \op -> singleWidthCost op . scaleLMul (1 / ratio),
        wideningCost = \op -> wideningCost op . scaleLMul (1 / ratio),
        slideCost = \op -> slideCost op . scaleLMul (1 / ratio),
        slide1Cost = \op -> slide1Cost op . scaleLMul (1 / ratio),
        compareCost = compareCost . scaleLMul (1 / ratio),
        maskOpCost = maskOpCost . scaleLMul (1 / ratio),
        mergeCost = mergeCost . scaleLMul (1 / ratio),
        iotaCost = iotaCost . scaleLMul (1 / ratio),
        idCost = idCost . scaleLMul (1 / ratio),
        broadcastCost = broadcastCost . scaleLMul (1 / ratio),
        scalarMoveCost = scalarMoveCost . scaleLMul (1 / ratio),
        narrowingCost = narrowingCost . scaleLMul (1 / ratio),
        vsetCost = \vectorConfig1 vectorConfig2 ->
          vsetCost
            (scaleLMul (1 / ratio) vectorConfig1)
            (scaleLMul (1 / ratio) vectorConfig2),
        scalarBinCost = scalarBinCost,
        scalarUnaryCost = scalarUnaryCost
      }
