module Combat.Data(
  Combat(..),
  Actions(..),
)where

data Combat = Combat{
  isPlayerTurn :: Bool, 
  isCombat :: Bool,
  hasGuard :: Bool,
  isAttacking :: Bool

}deriving(Show, Eq)

data Actions = Potion | Attack | Block | Run deriving(Eq, Show)