#### 06 PROCESS DAGs ###########################################################

library(magick)

magick::image_read("data/dagitty_FREH.png") |> 
  image_crop("1600x1300+790+130") |> 
  image_write("output/figure_4a.png")

magick::image_read("data/dagitty_rev.png") |> 
  image_crop("1600x1300+790+130") |> 
  image_write("output/figure_4b.png")

magick::image_read("data/dagitty_price.png") |> 
  image_crop("1600x1300+790+130") |> 
  image_write("output/figure_4c.png")

magick::image_read("data/dagitty_FREH_change.png") |> 
  image_crop("1600x1300+790+130") |> 
  image_write("output/figure_4d.png")

magick::image_read("data/dagitty_rev_change.png") |> 
  image_crop("1600x1300+790+130") |> 
  image_write("output/figure_4e.png")

magick::image_read("data/dagitty_price_change.png") |> 
  image_crop("1600x1300+790+130") |> 
  image_write("output/figure_4f.png")
