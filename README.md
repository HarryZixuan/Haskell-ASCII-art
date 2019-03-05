# Haskell-ASCII-art
Zixuan Zhang
COMP 3007 assignment


The convertBmpToASCII.hs will transfor the Bmp image into the ASCII art.
install the Bmp Package: cabal install bmp-1.2.6.3
usage: showAsASCIIArt (question1 "greyScale" True (loadBitmap "yourImageFile.bmp"))

The whereIsWaldo.hs will load two bitmap images, convert them to ASCII art using the convertBmpToASCII.hs code, and then determine whether or not the second image appears in the first one and,if it does, provide the co-ordinates.
usage: 
question2 (question1 "greyScale" True (loadBitmap "yourFirstImageFile.bmp")) (question1 "greyScale" True (loadBitmap "yourImageFile"))

