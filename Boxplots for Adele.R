#This script takes Adele's UROP data and teaches her to make boxplots for her poster



#1 Install this package and run this code:----

library(ggplot2) #This package is used to make highly customizable plots with R

#2) Download your data from google sheets as a .csv file----


#3) Update this code with the file path to the .csv file in your computer and run it read in data----

zoop <- read.csv("E:/My Drive/Thesis/Adele_UROP_Data.csv")

#4) A little data formatting----

zoop$Lake <- as.factor(zoop$Lake) #This tells R that "Lake" is a categorical variable so it will format your plots properly
colnames(zoop)[colnames(zoop) == "Species"] <- "Genus"    #Rename the species column to Genus so it shows up correctly in your legend

#5) Calculate your ratio for the headpore locations----

zoop$headpore.ratio <- zoop$Length.from.side.um/zoop$Length.from.bottom.um   #Check if I did this backwards??


#6) Make a boxplot of headpore diameter by lake, separating Bosmina and Eubosmina----

diameter <- ggplot(      #Saves your plot with the name "diameter"
    data = zoop,        #Tell R where to find your data
    aes(x = Lake, y = Diameter.of.pore.um, fill = Genus)      #Tell R what your x and y variables are, and separate data by species with different fill colors
  ) +      #Close initial parenthesis and use "+" to tell R that more modifications to the plot are coming in the next line
  geom_boxplot(position = position_dodge(width = 0.82, preserve = "single")) +      #Tell R that you want it to make a boxplot with these data, and you want all boxes the same width
  theme_classic() +     #Keeps the background of your plot clean
  labs(                 #add labels to your plot
    title = "TITLE",    #we have to decide if you want to make the titles here or directly on your poster
    x = "X-axis",
    y = "y-axis (\u03bcm)"   #I put your units in for you here because greek symbols are tricky in R
  ) +
  scale_fill_manual(  #Adjusts colors - you can have fun with this as long as it is color-blind friendly
    values = c(
      "Bosmina" = "Black",
      "Eubosmina" = "White"
    )
  ) +
  theme(     #Change text size: since this is a poster, we want things BIG
    plot.title = element_text(hjust = 0.5, size = 25),  #Change title size (and center the title)
    axis.title = element_text(size = 20),  #change axis title text size
    axis.text = element_text(size = 15),   #change axis label text size
    legend.title = element_text(size = 20),   #change legend title text size
    legend.text = element_text(size = 15),    #change legend text size
  ) 
  

diameter   #This tells R to show you the plot you just made


#When you are happy with your plot, use this code to save it as a high resolution tiff file:
ggsave(filename = "E:/My Drive/Thesis/Diameter_boxplot.tiff",   #change this to the location and name you want
       plot = diameter,
       width = 6,              # Specify width in units - look at the available space on your poster to decide
       height = 4,             # Specify height in units - look at the available space on your poster to decide
       units = "in",           # Specify units for height and width
       dpi = 600               # Set a high resolution (300 or 600 are common) - I always go really high for posters because they will be big when printed
       )


#7) Copy the code above and modify it to make a second plot named "location" for your headpore location ratio:----
#your formatting should be the same, you will just change the data you are using

