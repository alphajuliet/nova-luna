# Nova Luna

Simulate the Nova Luna tile strategy game. Written in Clojure because it's my language of choice.

Source files:

- `gen_tiles.clj`: create PNG or SVG versions of the tiles
- `tiles.clj`: read and parse the list of tiles
- `state.clj`: manage the state of the game, including tile stack, boards, wheel, and scoring track
- `action.clj`: provide actions to update the game state, mainly playing a tile and updating the wheel.
- `game.clj`: game-level functions

