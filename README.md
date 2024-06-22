# Retome

A skin focused on glow and shadow effects.

## Forum posts

https://osu.ppy.sh/community/forums/topics/630525?start=6195149

## Building

```bash
git submodule update --init --recursive --progress --depth=1 --checkout
racket mkosuskin.rkt -m ja -m external -r dev
```

to render the Japanese version with external assets as the "dev" release.

### Requirements

#### Programs

- inkscape
- lmms
- blender
- bash
- sed
- gm (graphicsmagick)
- magick (imagemagick)
- pngquant
- vips
- racket

#### Fonts

Download and move fonts to:

- [GenJyuu Gothic Light](http://jikasei.me/font/genjyuu/) → `_common/fonts/GenJyuuGothic-Light.ttf`
- [KikaiChokokuJIS](http://font.kim/) → `_common/fonts/KikaiChokokuJIS-Md.otf`
- [JK Gothic M](http://font.cutegirl.jp/jk-font-medium.html) → `_common/fonts/JKG-M_3.ttf`
- [JK Gothic L](http://font.cutegirl.jp/jk-font-light.html) → `_common/fonts/JKG-L_3.ttf`
- [Noto Sans CJK Light (OTC)](https://www.google.com/get/noto/help/cjk/) → `_common/fonts/NotoSansCJK-Light.ttc`
- [Noto Sans CJK Medium (OTC)](https://www.google.com/get/noto/help/cjk/) → `_common/fonts/NotoSansCJK-Medium.ttc`

## License

[![image](https://i.creativecommons.org/l/by-nc/4.0/88x31.png)](http://creativecommons.org/licenses/by-nc/4.0/)

This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>.

### Credits

- Hitsounds are taken from [osu-resources](//github.com/ppy/osu-resources), which is under CC-BY-NC 4.0.
