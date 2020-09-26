# platformer

This is the platformer game I am currently working on. 

## How to build

You need the following system libraries:
- sdl2
- sdl2_gfx
- sdl2_image

Then it should be as simple as:
```bash
git clone https://github.com/Simre1/platformer
cd platformer
cabal build
cabal exec game
```

The main focus right now is implementing level creation through the [OGMO level editor](https://ogmo-editor-3.github.io/). Right now, only the player und platforms are available.