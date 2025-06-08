# Fabbie – Your Haskell Factory Buddy for Factorio

**Fabbie** is a lightweight Haskell toolkit designed to assist you while playing **Factorio**. It provides modules and types to model production chains, recipes, and machines — interactively usable via `ghci`.

It's not a full automation planner — it's your **functional assistant** while building factories, calculating resource needs, and understanding crafting mechanics.

---

## 🎯 Goals

Fabbie is meant to be:

- A modular Haskell-based helper for Factorio players
- Easy to use in `ghci` for live exploration
- Focused on **products**, **machines**, and **resource calculations**

---

## 📦 Modules Overview

| Module     | Purpose                                                                                     |
| ---------- | ------------------------------------------------------------------------------------------- |
| `Machines` | Defines machine types like Furnaces and Assemblers, including energy use and crafting speed |
| `Recipes`  | Defines products, recipes, ingredients, and helper functions for access and transformation  |

---

## 🧪 Example Usage in `ghci`

```haskell
-- Start ghci and load modules:
$ ghci
> :l Recipes.hs
> :l Machines.hs

-- Inspect a product:
> name ironPlate
ProductName "Iron Plate"

-- Access its ingredients:
> unwrapRecipe (recipe ironPlate)
[Ingredient (ProductName "Iron Ore",1)]

```
