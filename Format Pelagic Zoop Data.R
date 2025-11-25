

#Starts with inclusion table created in "Create Fish-Zoop Pelagic Model Inclusion Table" 
      #Includes lakes with exact match for fish/zoop sampling years, with surveys that passed my quality checks

#corrects zooplankton taxonomy issues in the selected data and calculates an accurate Shannon Diversity Index
#cleans zoop dataset based on Kylie's notes and known issues with DNR zooplankton dataset
#calculates zoop metrics (Bytho and Lepto excluded) and joins them



#LAKES THAT NEED SPECIAL ATTENTION:
      #Hill North and South need to be combined
      #Vermilion needs to be split east-west
      #Red needs to be split upper-lower


#remove Hill lake south as a row from the dataset - the zoops will get summarized appropriately in "Hill (north)" by parentdow and the fish will too
Incl.Table <- filter(Incl.Table, LakeName != "Hill (south)")