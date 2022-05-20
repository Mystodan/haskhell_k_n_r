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
  helpText,
  noSuchArgsText,
  inappropriateAmountOfFlags,
  intro,
  screen,
  outro
  )where


screen :: String 
screen = "\t       (                                                                      (                           \n"++
  "\t   ( /(                         )     )             (              (        )\\ )                        \n"++
  "\t   )\\())        (    (  (   ( /(   ( /(            )\\               )\\ )    (()/(      )   (  (      (   \n"++
  "\t ((_)\\   (     )\\   )\\))(  )\\())  )\\())  (     ((((_)(     (      (()/(     /(_))  ( /(   )\\))(    ))\\  \n"++
  "\t(._ ((_)  )\\ ) ((_) ((_))\\ ((_)\\  (_))/   )\\     )\\ _ )\\    )\\ )    ((_))   (_))    )(_)) ((_))\\   /((_) \n"++
  "\t\\| |/ / ._(_/(  (_)  (()(_)| ||_) | |_   ((_)    (_)_\\(_)  _(_/(    _| |    | + \\  ((_)_   (()(_) (_))   \n"++
  "\t(|   <  | ,_ \\))| | / _, | |    | |  _|  (_-<     / _ \\   | ,_\\)) / _, |    |   /  / _, | / _, |  / +_)  \n"++
  "\t |_|\\_\\ |_||_|  |_| \\__, | |_||_| \\___|  /__/    /_/ \\_\\  |_||_|  \\__,_|    |_|_\\  \\__,_| \\__, |  \\___|  \n"++
  "\t                    |___/                                                                 |___/        \n"

intro :: String
intro =  
   "\nWelcome to Knights and Rage!\n\n"
 ++">> PRESS `ENTER` TO START <<"
outro :: String -> String
outro x =  
   "\nWell Played! Knights and Rage!\n\n"
 ++ x
 ++"\n>> PRESS `ENTER` TO QUIT <<"
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

noSuchArgsText::String 
noSuchArgsText = "\nNo such args exist:"

inappropriateAmountOfFlags::Int -> String
inappropriateAmountOfFlags x = "Invalid amount of flags found:"++show x
helpText::String 
helpText = 
  "\nWelcome to the help section of Knights and Rage!"
  ++"\nThis is a roguelike game with player progression, however if you lose you die! and you lose all progression."
  ++"\nThis game will not save your progress, however such implementaion would have been added if more time was given."
  ++"\nKnights and Rage is a endless game, which means it can run on forever, with no levelcap and infinite scaling on enemies"
  ++"\n@TODO: More weapons, weapon Type and effects, potion effects, etc., proper weapon scaling, drop system on enemies, randomized scaled weapons on enemies"
  ++"\n\n"