module Recipes where

import qualified Data.Map as Map

type Amount = Int

type TimeInSec = Double

newtype ProductName = ProductName String deriving (Eq, Ord)

newtype Ingredient = Ingredient (ProductName, Amount)

newtype ProductRecipe = ProductRecipe [Ingredient]

data Product = Product
  { name :: ProductName,
    recipe :: ProductRecipe,
    productionTime :: TimeInSec
  }

-- TODO: Unit-Test
getAmount :: Ingredient -> Amount
getAmount (Ingredient (_, amount)) = amount

-- TODO: Unit-Test
getProductName :: Ingredient -> ProductName
getProductName (Ingredient (name, _)) = name

-- TODO: Unit-Test
unwrapRecipe :: ProductRecipe -> [Ingredient]
unwrapRecipe (ProductRecipe ingredients) = ingredients

ironPlate :: Product
ironPlate =
  Product
    { name = ProductName "Iron Plate",
      recipe = ProductRecipe [Ingredient (ProductName "Iron Ore", 1)],
      productionTime = 3.2
    }

chopperPlate :: Product
chopperPlate =
  Product
    { name = ProductName "Chopper Plate",
      recipe = ProductRecipe [Ingredient (ProductName "Chopper Ore", 1)],
      productionTime = 3.2
    }

ironGearWheel :: Product
ironGearWheel =
  Product
    { name = ProductName "Iron Gear Wheel",
      recipe = ProductRecipe [Ingredient (ProductName "Iron Plate", 2)],
      productionTime = 0.5
    }

steelPlate :: Product
steelPlate =
  Product
    { name = ProductName "Steel Plate",
      recipe = ProductRecipe [Ingredient (ProductName "Iron Plate", 5)],
      productionTime = 16.0
    }

redSciencePack :: Product
redSciencePack =
  Product
    { name = ProductName "Red Science Pack",
      recipe =
        ProductRecipe
          [ Ingredient (ProductName "Chopper Plate", 1),
            Ingredient (ProductName "Iron Gear Wheel", 1)
          ],
      productionTime = 5.0
    }

productMap :: Map.Map ProductName Product
productMap =
  Map.fromList
    [ (name ironPlate, ironPlate),
      (name chopperPlate, chopperPlate)
    ]
