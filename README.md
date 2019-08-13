* This is an incomplete copy of [Threes](https://play.google.com/store/apps/details?id=vo.threes.exclaim&hl=en_US), a game similar to but more interesting than [2048](https://play.google.com/store/apps/details?id=apps.r.twothousandfortyeight&hl=en_US) in Android.

* The game is implemented by Haskell with the [gloss](https://hackage.haskell.org/package/gloss), an excellent graphics library.

* There are some more works to do:

  * The numbers shown is too ugly, since  [gloss](https://hackage.haskell.org/package/gloss) can't set the font of text. Maybe numbers can be generated in the **BMP** format by code and drawn by *bitmapDataOfByteString* in [Graphics.Gloss.Data.Bitmap](https://hackage.haskell.org/package/gloss-1.13.0.1/docs/Graphics-Gloss-Data-Bitmap.html);

  * Show the score and the score history, but [gloss](https://hackage.haskell.org/package/gloss) doesn't support pretty text show.

* The following is a demo show:

  ![threes](https://github.com/chenyulue/threes/blob/master/demo.gifC:\Users\chenyulue\hs\threes\demo.gif)
