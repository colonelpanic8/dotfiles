module TaffybarConfig.WidgetPlan
  ( EndWidgetPlan (..),
    endWidgetPlanForCapabilities,
  )
where

import TaffybarConfig.RuntimeCapabilities (RuntimeCapabilities (..))

data EndWidgetPlan = EndWidgetPlan
  { useBatteryLayout :: Bool,
    includeBacklight :: Bool,
    includeCPUPower :: Bool
  }
  deriving (Eq, Show)

endWidgetPlanForCapabilities :: RuntimeCapabilities -> EndWidgetPlan
endWidgetPlanForCapabilities capabilities =
  EndWidgetPlan
    { useBatteryLayout = runtimeHasBattery capabilities,
      includeBacklight = runtimeHasBacklight capabilities,
      includeCPUPower = runtimeHasCPUPower capabilities
    }
