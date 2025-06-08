module Machines where

newtype EnergyInWatt = EnergyInWatt Double

newtype CraftingSpeed = CraftingSpeed Double

data MachineType
  = Furnace
  | AssemblingMachine
  | Lab
  | OilRefinery
  | ChemicalPlant

data CraftingMachine = CraftingMachine
  { machineType :: MachineType,
    energyConsumption :: EnergyInWatt,
    craftingSpeed :: CraftingSpeed
  }

furnace :: EnergyInWatt -> CraftingSpeed -> CraftingMachine
furnace energy speed =
  CraftingMachine
    { machineType = Furnace,
      energyConsumption = energy,
      craftingSpeed = speed
    }

assemblingMachine :: EnergyInWatt -> CraftingSpeed -> CraftingMachine
assemblingMachine energy speed =
  CraftingMachine
    { machineType = AssemblingMachine,
      energyConsumption = energy,
      craftingSpeed = speed
    }

stoneFurnace :: CraftingMachine
stoneFurnace = furnace (EnergyInWatt 9000.0) (CraftingSpeed 1.0)

steelFurnace :: CraftingMachine
steelFurnace = furnace (EnergyInWatt 9000.0) (CraftingSpeed 2.0)

electricFurnace :: CraftingMachine
electricFurnace = furnace (EnergyInWatt 18000.0) (CraftingSpeed 2.0)

assemblingMachine1 :: CraftingMachine
assemblingMachine1 = assemblingMachine (EnergyInWatt 75000.0) (CraftingSpeed 0.5)

assemblingMachine2 :: CraftingMachine
assemblingMachine2 = assemblingMachine (EnergyInWatt 150000.0) (CraftingSpeed 0.75)

assemblingMachine3 :: CraftingMachine
assemblingMachine3 = assemblingMachine (EnergyInWatt 375000.0) (CraftingSpeed 1.25)
