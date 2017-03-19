data OperatingSystem =
       GnuPlusLinux
       | OpenBSDPlusNevermindJustBSDStill
       | Mac
       | Windows
       deriving (Eq, Show)

data ProgrammingLanguage =
       Haskell
       | Agda
       | Idris
       | PureScript
       deriving (Eq, Show)

data Programmer = Programmer {
        os :: OperatingSystem
      , lang :: ProgrammingLanguage
      } deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [
  GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = foldr (++) [] $ map (\y -> (map (\x -> Programmer {os = x, lang = y}) allOperatingSystems)) allLanguages
