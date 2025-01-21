# Lazy Physics Engine

This is an experimental physic engine written in haskell. The goal is to create a physic engine that is 100% energy conservative, then exploit this energy conservation to help with lazyness.


# How to run

You need to have stack installed. Then you can run the following command:

```bash
stack run
```

Click on the window to create a new ball. The balls will bounce around the window.
*(Note that for now, no lazyness is exploited, as we render all the balls at each frame)*


# Roadmap

- [x] Create a simple bouncing ball simulation
- [ ] Implement "perfect" energy conservation
- [ ] Implement space partitioning for collision detection
- [ ] Further optimize the engine using total energy per "chunk" of space
    (i.e. a ball will never leave a chunk if there's not enough energy locally to do so)
- [ ] Change the way we observe the world, to allow for lazyness (big world, or 3d world)
- Attempt to :
    - [ ] Have multiple scales in the engine (i.e. a ball could be a planet, or a particle)
    - [ ] Time travel (i.e. we could go back in time and change the past)


# Note on the project

I was interested into building this to see if it was possible to exploit the "pure" lazyness of haskell in an engine. In the quantum world, some things are not set until they are observed. This is the same with haskell. The lazyness of haskell allows us to create a world that is not set until it is observed. This is a very interesting concept that I wanted to explore.

Another interesting point is about the immutability of this language, to change something, you kindof create a copy of it. This might allow us to implement time bound behaviors in a very interesting way.
