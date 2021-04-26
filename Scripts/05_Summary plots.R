
library(ggplot2)

# Mericarps 


ggplot(flower) +
 aes(x = galapagos_other, y = petal_length) +
 geom_point() +
   labs(x = "Galapagos - Other - Mainland", y = "Length (mm)", title = "Petal Lenght (mm)") +
 theme_minimal() + theme(axis.title = element_text(size = 13, 
    face = "bold"), axis.text = element_text(size = 10), 
    plot.title = element_text(face = "bold")) + theme(axis.line = element_line(colour = "gray20", 
    size = 0.5, linetype = "solid"), axis.text = element_text(size = 12), 
    axis.text.x = element_text(size = 12))


