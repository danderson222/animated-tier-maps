
setwd("your_environment")

if(!require("rtweet")) install.packages("rtweet")
if(!require("tidyverse")) install.packages("tidyverse")

# api_key <- [your_api_key]
# api_secret_key <- [your_apli_secret_key]
# bearer_key <- [your_bearer_key]
# access_token <- [your_access_token]
# access_secret <- [your_access_secret]

# Authenticate twitter account via web browser
token <-
  rtweet::create_token(
    app = "TierMapAnimation",
    consumer_key = api_key,
    consumer_secret = api_secret_key,
    access_token = access_token,
    access_secret = access_secret
  )
# To make sure you can collect data, you need to have a Twitter user
# Noteworthy, we cannot just collect data without any limits. 
# In most cases, we have a limit of 18,000 observations per 15 minutes.

electionmap_tweets <- get_timeline("ElectionMapsUK", n = 2000, token)
save(electionmap_tweets, file = "tweets.rds")
em_photo_tweets <- electionmap_tweets %>% 
  subset(media_type=="photo")

# Looking at this we notice that each of the Tier maps has a similar URL in the 
# ext_media_expanded_url column, which links it back to the ElectionsMap website, 
# so let's subset for that url
# "https://electionmaps.uk/covid19-tier-map"
em_tier_tweets <- em_photo_tweets %>% 
  mutate(urls_expanded_url = sapply(urls_expanded_url, toString)) %>% 
  filter(urls_expanded_url=="https://electionmaps.uk/covid19-tier-map")

# Before downloading, I noticed through randomly clicking through that some of these 
# are not Tier maps, so we will need to filter for text containing the word Tier
em_tier_tweets <- filter(em_tier_tweets, grepl("Tier",text))

# When we create the new data frame we have to unlist the media_url column, turning 
# it from list to character 
images <- data.frame(Date = em_tier_tweets$created_at,
                     Image = unlist(em_tier_tweets$media_url))

# Cut the timing on the Date column
images$Date <- substr(images$Date, 1, 10)

# Create a folder for the images
dir.create("images")
images$Date
for (i in 1:nrow(images)) { 
  row <- images[i,]
  download.file(images[i,2], destfile = paste0('images/TierMap_',images[i,1],'.jpg'),                  
                mode="wb") 
}
# This downloads 20 images, so be aware!

# We notice that four images are not Tier Maps or have text over the maps, so let's just go ahead and delete those manually
file.remove(c("images/TierMap_2020-12-01.jpg", "images/TierMap_2020-12-15.jpg",
              "images/TierMap_2020-11-28.jpg", "images/TierMap_2020-12-22.jpg"))

# Almost there, now we have all the images, their dates and the will to make this into an animated graphic
# Next we will need the magick and magrittr packages
# Magick https://cran.r-project.org/web/packages/magick/vignettes/intro.html#Text_annotation

if(!require("magick")) install.packages("magick")
if(!require("magrittr")) install.packages("magrittr")

# Read in all the image names from your folder
# Create a vector with all the dates to be read onto each image
dates <- list.files("images") %>% 
  substr(9,18)

listOfFiles <- list.files(path = "images",
                          full.names = TRUE)
# Resize the images in bulk thanks to a function found here https://www.ben-johnston.co.uk/bulk-resizing-images-with-r/
imageResizeR <- function(z, a){
  listOfFiles <- list.files(path = z,
                            full.names = TRUE)
  imageResizing <- for(i in 1:length(listOfFiles)){
    imFile <- image_read(listOfFiles[i])
    resized <- image_scale(imFile, a)
    dated <- image_annotate(resized, dates[i], gravity = "northwest", size = 30, 
                            color = "black", boxcolor = "lightgrey")
    image_write(dated,
                paste(listOfFiles[i]))
  }
}

imageResizeR("images", "500x750")

list.files(path="images", pattern = '*.jpg', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4, delay = 200) %>% # animates, can opt for number of loops
  image_write("TierMaps.gif") # write to current dir
