library(magick)
Animation <- image_read("https://media.giphy.com/media/lVZnnXraJX440/giphy.gif") %>%
  image_scale("500")
Animation



plot = image_read("images/lollipop_plot.png")



# Combine the plot and animation
# Set the Background image
background <- image_background(image_scale(plot, "1000"), "white", flatten = TRUE)
# Combine and flatten frames
frames <- image_composite(background, Animation, offset = "+400+420")
# Turn frames into animation
animation <- image_animate(frames, fps = 10)
print(animation)



#Save gif
image_write(animation, "humor.gif")


