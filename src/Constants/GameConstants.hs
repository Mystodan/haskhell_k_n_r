module Constants.GameConstants(
  statToHealth,
  vit,
  str,
  dex,
  res,
  storyIntro,
  storyInitPlayer,
  promptName,
  promptEnounter,
  newScreen,
  numPool,
  )where

statToHealth::Int
statToHealth = 6

maxStagePerRound::Int 
maxStagePerRound = 2 

newScreen::String 
newScreen = "\ESC[2J\ESC[2J\ESC[2J\n"

vit::[String] 
vit = ["vitality","vit"]

str::[String] 
str = ["strength","str"]

dex::[String] 
dex = ["dexterity","dex"]

res::[String] 
res = ["resilience","res"]

promptName :: String 
promptName = "There existed a knight named:"


promptEnounter :: Int -> String 
promptEnounter doors = "There Exists " ++ show [1..doors] ++ " doors, which one do you pick?"

storyIntro :: [String]
storyIntro = [
  "\n\nOnce upon a time...",
  "When knights were common..."
  ]

storyInitPlayer :: String -> [String] 
storyInitPlayer name = 
  [
    "This knight called '"++name++"' was abandoned...",
    "Without their weapon, they faced this world anew..."
    ]

numPool :: String 
numPool = "1234567890"