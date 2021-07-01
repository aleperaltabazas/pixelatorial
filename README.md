# Pixelatorial

In a screen, an image is composed by a bunch of pixels. We can say that a picture is simply a matrix where every cell a<sub>i</sub><sub>j</sub> holds the color of the pixel (hexcode or RGB, does not really matter). There is also a finite amount of colors that can be represented (right now, there are 16,777,216 colors with [True Color](https://en.wikipedia.org/wiki/Color_depth#True_color_(24-bit)). Thus, there is a **finite** amount of possible combinations to order pixels in any given screen size.

Let's take a screen size of 2x2 with only 1 color (let's say `#ffffff`, for the sake of the example). The amount of combinations possible will be 1: every pixel painted with the same color.

```
       1        2
1 [ #ffffff, #ffffff ]
2 [ #ffffff, #ffffff ]
```

However, as soon as we increase the number of colors to 2 (we add black), we now get 16 different combinations:

```
       1        2
1 [ #ffffff, #ffffff ]
2 [ #ffffff, #ffffff ]

       1        2
1 [ #ffffff, #ffffff ]
2 [ #ffffff, #000000 ]

       1        2
1 [ #ffffff, #ffffff ]
2 [ #000000, #ffffff ]

       1        2
1 [ #ffffff, #ffffff ]
2 [ #000000, #000000 ]

       1        2
1 [ #ffffff, #000000 ]
2 [ #ffffff, #ffffff ]

       1        2
1 [ #ffffff, #000000 ]
2 [ #ffffff, #000000 ]

       1        2
1 [ #ffffff, #000000 ]
2 [ #000000, #ffffff ]

       1        2
1 [ #ffffff, #000000 ]
2 [ #000000, #000000 ]

       1        2
1 [ #000000, #ffffff ]
2 [ #ffffff, #ffffff ]

       1        2
1 [ #000000, #ffffff ]
2 [ #ffffff, #000000 ]

       1        2
1 [ #000000, #ffffff ]
2 [ #000000, #ffffff ]

       1        2
1 [ #000000, #ffffff ]
2 [ #000000, #000000 ]

       1        2
1 [ #000000, #000000 ]
2 [ #ffffff, #ffffff ]

       1        2
1 [ #000000, #000000 ]
2 [ #ffffff, #000000 ]

       1        2
1 [ #000000, #000000 ]
2 [ #000000, #ffffff ]

       1        2
1 [ #000000, #000000 ]
2 [ #000000, #000000 ]
```

It seems to be growing exponentially. Indeed, it responds to the following formula:

```
combinations(width, height, amount_of_colors) = amount_of_colors ^ (width * height)
```
