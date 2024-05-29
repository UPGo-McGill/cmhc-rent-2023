#### 06 PROCESS DAGs ###########################################################

library(magick)

image_read("data/dagitty_FREH.png") |> 
  image_crop("2250x1040+50+380") |> 
  image_write("output/figure_4a.png")

image_read("data/dagitty_rev.png") |> 
  image_crop("2250x1040+50+380") |> 
  image_write("output/figure_4b.png")

image_read("data/dagitty_price.png") |> 
  image_crop("2250x1040+50+380") |> 
  image_write("output/figure_4c.png")

image_read("data/dagitty_FREH_change.png") |> 
  image_crop("2250x1040+50+380") |> 
  image_write("output/figure_4d.png")

image_read("data/dagitty_rev_change.png") |> 
  image_crop("2250x1040+50+380") |> 
  image_write("output/figure_4e.png")

image_read("data/dagitty_price_change.png") |> 
  image_crop("2250x1040+50+380") |> 
  image_write("output/figure_4f.png")
