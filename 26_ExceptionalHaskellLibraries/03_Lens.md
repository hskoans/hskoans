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

Nested query:

```
playerGoals :: Player -> Int
playerGoals player =
  goals (playerStats player)
```

This isn't so bad.

What about a simple update?

```
increasePlayerSalary :: Player -> Int -> Player
increasePlayerSalary player raise =
  player { playerSalary =
             playerSalary player + raise }

Also not so bad.

Things get ugly if we try a nested update:

```
incrementPlayerGoals :: Player -> Player
incrementPlayerGoals player = player
  { playerStats = (playerStats player)
    { goals = goals (playerStats player) + 1 }}
```

In an imperative language, this is trivial:

```
void incrementPlayerGoals(Player player) {
  player.playerGoals += 1;
}
```

So, we want to be able to do easily in Haskell.

Let's say we have this:

```
data Player = Player
  { playerName :: String
  , playerSalary :: Int
  , playerStats :: PlayerStats }

data PlayerStats = PlayerStats
  { goals :: Int
  , gamesPlayed :: Int }

data Team = Team
  { teamName :: String
  , teamPlayers :: [Player] }
```

How do we increase the value of games played for all players?

```
incrAllPlayersGamesPlayed :: Team -> Team
incrAllPlayersGamesPlayed = ?
```

## Let's get started with Lens

```
cabal install lens
```

Once installed, let's import it into our source code.

```
import Control.Lens
```

Lens' style query:

```
getPlayerName :: Player -> String
getPlayerName player =
  player ^. playerName
```

```
playerName :: Lens' Player String
```

We can think of it as:

```
(^.) :: s -> Lens' s a -> a
```

The exact implementation isn't exactly like this but we shall simplify things so we can understand what's going on.

Nested query with Lens:

```
getPlayerGoals :: Player -> Int
getPlayerGoals player =
  player ^. playerStats . goals


playerStats :: Lens' Player PlayerStats

goals :: Lens' PlayerStats Int

(playerStats . goals) :: Lens' Player Int
```
