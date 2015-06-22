# Lens

* Access and manipulate nested data
  * Manipulating immutable nested data is tricky

Example:

```
data Player = Player
  { playerName :: String
  , playerSalary :: Int
  , playerStats :: PlayerStats }

data PlayerStats = PlayerStats
  { goals :: Int
  , gamesPlayed :: Int }
```
