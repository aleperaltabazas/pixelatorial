Pixelatorial
===

<p align="center">
  <img src="https://cdn-media-1.freecodecamp.org/images/1*7DsXxogHWKpaynmwaTfE7Q.jpeg" />
</p>

# Contents
- [Pixelatorial](#pixelatorial)
- [Contents](#contents)
  - [Introduction](#introduction)
  - [Usage](#usage)
  - [How does it work?](#how-does-it-work)
    - [Possible optimizations](#possible-optimizations)

## Introduction

In a screen, an image is composed by a bunch of pixels. We can say that a picture is simply a matrix where every cell a<sub>i</sub><sub>j</sub> holds the color of the pixel (hexcode or RGB, does not really matter). There is also a finite amount of colors that can be represented (right now, there are 16,777,216 colors with [True Color](https://en.wikipedia.org/wiki/Color_depth#True_color_(24-bit)). Thus, there is a **finite** amount of possible combinations to order pixels in any given screen size.

So, the things we need to take into account are screen size (height and width) and the amount of colors:
* For any screen size, as long as we have just one color, we will have just one combination (the whole screen with said color).
* If we fix the screen size to 1x1 and vary the amount of colors, then we will have as many combinations as colors.
* If we vary both, then things go to shit pretty quickly.

Let's take a screen size of 2x2 with 2 colors (we will use red and green, for this example). We will have 16 possible combinations:

|   |   |   |   |
|---|---|---|---|
| ![15](https://user-images.githubusercontent.com/31170385/124193042-938a4d80-da9c-11eb-832d-e0d77d3e3c1c.png) | ![14](https://user-images.githubusercontent.com/31170385/124193047-94bb7a80-da9c-11eb-8408-c40c89f9aa21.png) | ![13](https://user-images.githubusercontent.com/31170385/124193067-984f0180-da9c-11eb-96a8-051399d4e896.png) | ![12](https://user-images.githubusercontent.com/31170385/124193053-95541100-da9c-11eb-8abe-7dde4c32e94e.png) |
| ![11](https://user-images.githubusercontent.com/31170385/124193069-98e79800-da9c-11eb-9d22-2a4e3ae1f2e8.png) | ![10](https://user-images.githubusercontent.com/31170385/124193059-95eca780-da9c-11eb-8564-f5c8d9a3c9b8.png) | ![9](https://user-images.githubusercontent.com/31170385/124193062-96853e00-da9c-11eb-953e-557f8ee422d7.png) | ![8](https://user-images.githubusercontent.com/31170385/124193063-971dd480-da9c-11eb-848d-ac3c5c264972.png) |
| ![7](https://user-images.githubusercontent.com/31170385/124193074-99802e80-da9c-11eb-9593-3de93cab751a.png) | ![6](https://user-images.githubusercontent.com/31170385/124193064-971dd480-da9c-11eb-9bc4-20f088a1fdfe.png) | ![5](https://user-images.githubusercontent.com/31170385/124193066-97b66b00-da9c-11eb-9b22-6a5ff2eaa2c5.png) | ![4](https://user-images.githubusercontent.com/31170385/124193078-9a18c500-da9c-11eb-8c36-0cca57782928.png) |
| ![3](https://user-images.githubusercontent.com/31170385/124193073-99802e80-da9c-11eb-82e0-a13178e01b4b.png) | ![2](https://user-images.githubusercontent.com/31170385/124193080-9ab15b80-da9c-11eb-85dd-b7ea6eec5d36.png) | ![1](https://user-images.githubusercontent.com/31170385/124193081-9b49f200-da9c-11eb-812f-955ab23d1106.png) | ![0](https://user-images.githubusercontent.com/31170385/124193044-94bb7a80-da9c-11eb-9abb-7f21ed1d24ef.png) |

<span style="font-size: 12px;font-style:italic;">Note: these picture here have been resized to 32x32 for the sake of them being visible</span>

Indeed, the growth of the amount of combinations responds to the following function:
```
combinations(height, width, colors) = colors ^ (height * width)
```

For a screen size of 1920x1080, the number of combinations has 14,970,606 digits, in case it wasn't obvious how stupid big it gets. Even a tiny 800x600 allows for more than 3 million unique combinations.

## Usage

Pixelatorial is provided as a command line tool, which can be installed by cloning this repo and compiling the binaries with `stack install`. I'm planning on uploading it soon to Hackage and Stackage for easier installation, and the possibility to use it as a library.

So, once installed, the pixelatorial program is available for you to use:

```
Pixelatorial - Exhaustive image painter

Usage: pixelatorial [-c|--cycles CYCLES] --color-set COLOR-SET 
                    [-s|--skip OFFSET] [-o|--output OUTPUT] --height HEIGHT
                    --width WIDTH [--pixel-size ARG]
  Draw every image for any canvas with any amount of colors

Available options:
  -c,--cycles CYCLES       Amount of iterations to perform. If not specified, it
                           will iterate until the end. Applied after offset.
  --color-set COLOR-SET    Path to color set. Should be a list of hex colors
                           separated by newline.
  -s,--skip OFFSET         Skip OFFSET combinations before drawing.
  -o,--output OUTPUT       Folder to put the drawings. Default is svg/
  --height HEIGHT          Canvas' height
  --width WIDTH            Canvas' width
  --pixel-size ARG         'Size' for pixels (height and width of the node on
                           the SVG). Default is 1.
  -h,--help                Show this help text
```

The required arguments are `width`, `height` and `color-set`. Of course, since this program grows exponentially, you may decide to not run all of the possible iterations. In that case, you can use `cycles` to generate a set amount of combinations. Furthermore, you can use the `skip` option to drop the first `n` combinations, without generating them. In case you decide to combine them, the `skip` option is applied before the `cycles`.

Also, there's the `pixel-size` option. Since this program deals with _pixels_, images will be small. This option allows you to create images with a "bigger" pixel size. Bigger as in, the size of the squares in the SVG will be of said size. It is recommended that both height and width are divisible by the pixel size (I have no fucking idea what happens if it isn't, but I assume it will look kind of ugly in the borders).

## How does it work?

As stated earlier, pixelatorial generates all possible combinations of colors in a given screen size. And since Haskell is lazy, we can also do this in (almost) constant memory! Indeed, one combination is generated in every iteration and is then converted to SVG. The magic is given by the following piece of code:

```hs
combinatorial width height colors = combinate $ [ (x, y) | x <- [1 .. width], y <- [1 .. height] ]
  where combinate list = map (zipWith (\(x, y) c -> (x, y, c)) list) (replicateM (length xs) colors)
```

How the fuck does this algorithm work, I have no fucking clue, since Haskell does all this shit for me.

Anyways.

We now have a list of triads `(Int, Int, String)` holding the color of each x and y position. Thus, this is then converted into an SVG, and written to a file. For the file names, I wrote a dumb [`BigInt`](src/Data/BigInt.hs), which isn't really useful aside from using it as a String. While Hakell _does_ have an unbound `Integer`, this starts to eat memory once it starts growing. `BigInt` evades this problem by using a simple list of `Int`. In case of the biggest number, the one with 14 million digits, we have a list of 14 million entries, which is trivial for Hakell. I believe this could be further optimized by using [Vector](https://hackage.haskell.org/package/vector-0.12.3.0) instead of Haskell's plain lists.

I run a couple of tests with htop on, and even for small stuff like a 5x3 screen with 3 colors, the CPU was on fire, while memory was really chill, so it seems like the most limiting factor is processing power. It does make sense, since most of the work is converting a list of items, which can get quite big (almost 3 million items, for a 1920x1080, for instance), into a big string.

### Possible optimizations

Encoding a 800x600 image takes around a minute and a half. To encode every possible image with true color on this resolution (7,959,060,000,000 combinations), it would take around 2800 years. However, we need to take into account that this process is not necessarily sequential: the first combination does _not_ need to be created for the second one to exist. That's right, we could parallelize this work and reduce this time by a great degree.

Since the algorithm does rely on the order of the colors list, if we flipped them we could then simply process half the combinations on two machines with reverse sorts, and we now reduced the time to compute every 800x600 image to 1400 years. Which is still a lot, but the point is, we can change the order of the list, and this will produce different results. We can repeat this process again: we reverse the list once, and then we split the list in half and reverse each half. We will now have 4 lists:
```hs
[1, 2, 3, 4]
[4, 3, 2, 1]
[2, 1, 4, 3]
[3, 4, 2, 1]
```

And each will yield different results

```hs
> take 4 $ combinatorial [1] [1, 2] [1, 2, 3, 4]
[[(1,1,1),(1,2,1)],[(1,1,1),(1,2,2)],[(1,1,1),(1,2,3)],[(1,1,1),(1,2,4)]]
> take 4 $ combinatorial [1] [1, 2] [4, 3, 2, 1]
[[(1,1,4),(1,2,4)],[(1,1,4),(1,2,3)],[(1,1,4),(1,2,2)],[(1,1,4),(1,2,1)]]
> take 4 $ combinatorial [1] [1, 2] [2, 1, 4, 3]
[[(1,1,2),(1,2,2)],[(1,1,2),(1,2,1)],[(1,1,2),(1,2,4)],[(1,1,2),(1,2,3)]]
> take 4 $ combinatorial [1] [1, 2] [3, 4, 1, 2]
[[(1,1,3),(1,2,3)],[(1,1,3),(1,2,4)],[(1,1,3),(1,2,1)],[(1,1,3),(1,2,2)]]
```

This is a trivial example, but we can see that this generates four different results, with no intersection whatsoever between them. If we scaled this to the previous example, we have now reduced the time it takes to process it by 4 (700 years, right now). We can reduce this time as much as color permutations are (as long as there are more permutations than combinations, past that, patterns start repeating).
