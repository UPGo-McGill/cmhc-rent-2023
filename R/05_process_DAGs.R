#### 05 PROCESS DAGs ###########################################################


# Define DAGs -------------------------------------------------------------


dag_FREH

dag_non_FREH

dag_price



# Figure 4 ----------------------------------------------------------------

image_read("data/dagitty_FREH.png") |> 
  image_crop("2250x1040+50+380") |> 
  image_write("output/figure_4a.png")

image_read("data/dagitty_non_FREH.png") |> 
  image_crop("2250x1040+50+380") |> 
  image_write("output/figure_4b.png")

image_read("data/dagitty_price.png") |> 
  image_crop("2250x1040+50+380") |> 
  image_write("output/figure_4c.png")
