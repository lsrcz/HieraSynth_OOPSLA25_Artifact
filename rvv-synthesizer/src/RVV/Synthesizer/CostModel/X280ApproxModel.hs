{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module RVV.Synthesizer.CostModel.X280ApproxModel
  ( x280ApproxModel,
  )
where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Ratio (numerator)
import RVV.Semantics.MachineConfig
  ( MachineBaseConfig
      ( MachineBaseConfig,
        machineMemoryBlockIds,
        machinePointerUnit,
        machineScalarImmLength,
        machineScalarLength,
        machineVectorImmLength
      ),
    MachineConfig (MachineConfig, baseConfig, scalableConfig, useVLMask),
    MachineScalableConfig
      ( MachineScalableConfig,
        machineMemoryBlockLengths,
        machineVectorLength
      ), allowPartialVL,
  )
import RVV.Semantics.Multiplier (LengthMul (getLengthMul), MaskMul, WidthMul)
import RVV.Semantics.VectorConfig
  ( VectorConfig (lengthMul),
    widenVectorConfig,
  )
import RVV.Synthesizer.CostModel.CostModel
  ( CostModel
      ( CostModel,
        broadcastCost,
        compareCost,
        idCost,
        iotaCost,
        maskOpCost,
        mergeCost,
        modelMachineConfig,
        narrowingCost,
        scalarBinCost,
        scalarMoveCost,
        scalarUnaryCost,
        singleWidthCost,
        slide1Cost,
        slideCost,
        vsetCost,
        wideningCost
      ),
  )
import RVV.Synthesizer.Parameter.SingleWidthIntBinaryOpCode
  ( SingleWidthIntBinaryOpCode
      ( Add,
        And,
        Div,
        Divu,
        Max,
        Maxu,
        Min,
        Minu,
        Mul,
        Mulh,
        Mulhsu,
        Mulhu,
        Or,
        RSub,
        SAdd,
        SAddu,
        SSub,
        SSubu,
        Sll,
        Sra,
        Srl,
        Sub,
        Xor
      ),
  )
import RVV.Synthesizer.Parameter.SingleWidthIntUnaryOpCode
  ( SingleWidthIntUnaryOpCode,
  )
import RVV.Synthesizer.Parameter.WideningIntBinaryOpCode
  ( WideningIntBinaryOpCode (WAdd, WAddu, WMul, WMulsu, WMulu, WSub, WSubu),
  )

x280SingleCycleCost :: VectorConfig -> Int
x280SingleCycleCost config =
  numerator $
    getLengthMul $
      max (lengthMul config * 2) 1

x280ApproxSingleWidthCost :: SingleWidthIntBinaryOpCode -> VectorConfig -> Int
x280ApproxSingleWidthCost op config
  | op `elem` [Add, Sub, RSub, And, Or, Xor, Min, Minu, Max, Maxu, SAdd, SAddu, SSub, SSubu] =
      x280SingleCycleCost config
  | op `elem` [Mul, Mulh, Mulhu, Mulhsu, Sll, Srl, Sra] =
      4 * x280SingleCycleCost config
  | otherwise = error "Unimplemented"

x280ApproxWideningCost :: WideningIntBinaryOpCode -> VectorConfig -> Int
x280ApproxWideningCost op wideConfig
  | op `elem` [WAddu, WSubu, WAdd, WSub] = x280SingleCycleCost wideConfig
  | op `elem` [WMul, WMulu, WMulsu] = 4 * x280SingleCycleCost wideConfig
  | otherwise = error "Unimplemented"

x280MaskOpCost :: MaskMul -> Int
x280MaskOpCost 64 = 2
x280MaskOpCost _ = 1

x280ScalarBinCost :: SingleWidthIntBinaryOpCode -> WidthMul -> Int
x280ScalarBinCost op xmul
  | op `elem` [Div, Divu] = if xmul >= 1 then 66 else 34
  | op `elem` [Mul, Mulh, Mulhsu, Mulhu] = 3
  | otherwise = 1

x280ScalarBinOnlyDivMulCost :: SingleWidthIntBinaryOpCode -> WidthMul -> Int
x280ScalarBinOnlyDivMulCost op xmul
  | op `elem` [Div, Divu] = if xmul >= 1 then 66 else 34
  | op `elem` [Mul, Mulh, Mulhsu, Mulhu] = 3
  | otherwise = 0

x280ScalarUnaryCost :: SingleWidthIntUnaryOpCode -> WidthMul -> Int
x280ScalarUnaryCost _ _ = 1

x280ScalarUnaryOnlyDivMulCost :: SingleWidthIntUnaryOpCode -> WidthMul -> Int
x280ScalarUnaryOnlyDivMulCost _ _ = 0

x280ApproxModel :: Bool -> CostModel
x280ApproxModel considerScalar =
  CostModel
    { modelMachineConfig =
        MachineConfig
          { baseConfig =
              MachineBaseConfig
                { machineScalarLength = 64,
                  machinePointerUnit = 8,
                  machineScalarImmLength = 12,
                  machineMemoryBlockIds = HS.fromList [],
                  machineVectorImmLength = 5
                },
            scalableConfig =
              MachineScalableConfig
                { machineVectorLength = 512,
                  machineMemoryBlockLengths = HM.fromList []
                },
            useVLMask = error "Should not be used",
            allowPartialVL = error "Should not be used"
          },
      singleWidthCost = x280ApproxSingleWidthCost,
      wideningCost = x280ApproxWideningCost,
      slideCost = const x280SingleCycleCost,
      slide1Cost = const x280SingleCycleCost,
      compareCost = x280SingleCycleCost,
      maskOpCost = x280MaskOpCost,
      mergeCost = x280SingleCycleCost,
      iotaCost = x280SingleCycleCost,
      idCost = x280SingleCycleCost,
      broadcastCost = x280SingleCycleCost,
      scalarMoveCost = const 1,
      narrowingCost = x280SingleCycleCost . widenVectorConfig,
      vsetCost = \partConfig destConfig ->
        if lengthMul partConfig >= 1
          then 0
          else x280SingleCycleCost destConfig,
      scalarBinCost =
        if considerScalar
          then x280ScalarBinCost
          else x280ScalarBinOnlyDivMulCost,
      scalarUnaryCost =
        if considerScalar
          then x280ScalarUnaryCost
          else x280ScalarUnaryOnlyDivMulCost
    }
