#### 06 PROCESS DAGs ###########################################################

library(magick)

magick::image_read("data/dagitty_FREH.png") |> 
  image_crop("1800x1370+294+100") |> 
  image_write("output/figure_4a.png")

magick::image_read("data/dagitty_rev.png") |> 
  image_crop("1800x1370+294+100") |> 
  image_write("output/figure_4b.png")

magick::image_read("data/dagitty_price.png") |> 
  image_crop("1800x1370+294+100") |> 
  image_write("output/figure_4c.png")
