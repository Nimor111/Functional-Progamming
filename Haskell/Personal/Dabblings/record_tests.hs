import           Control.Monad (join)

data OperatingSystem = GNUPlusLinux
                      | OpenBSD
                      | Mac
                      | Windows
                      deriving (Eq, Show)

data ProgrammingLanguage = Haskell
                          | Agda
                          | Idris
                          | PureScript
                          deriving (Eq, Show)

data Programmer =
  Programmer { os   :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GNUPlusLinux
  , OpenBSD
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

makeProgrammer :: OperatingSystem -> ProgrammingLanguage -> Programmer
makeProgrammer os lang =
  Programmer {
    os = os
  , lang = lang }

-- join -> same as programmers >>= id, a.k.a flatten
allProgrammers :: [Programmer]
allProgrammers = join programmers
  where
    programmers = map (\x -> map (makeProgrammer x) allLanguages) allOperatingSystems
