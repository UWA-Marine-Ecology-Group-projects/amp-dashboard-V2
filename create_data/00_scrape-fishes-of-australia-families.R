# load libraries
library(rvest)
library(xml2)
library(httr)
library(stringr)  # Ensure str_to_title() is available

# For Genus ----
# Create a blank data frame with the columns you want
data <- data.frame(family = character(),
                   genus = character(),
                   url = character(),
                   num = numeric(),
                   stringsAsFactors = FALSE)

for(num in 1:1900) {
  print(num)
  
  # Construct the URL
  url <- paste0("https://fishesofaustralia.net.au/Home/genus/", num)
  
  # Set user agent header
  print(paste0("GET-ing ", url, " ..."))
  response <- httr::GET(url, httr::add_headers('User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36'))
  
  # Check response status
  if (http_error(response)) {
    warning(paste("Error accessing:", url))
    next
  }
  
  print("Success.")
  
  # Parse the HTML content
  html <- read_html(response)
  
  # Extract all text from the main content area
  page_text <- html %>% html_text(trim = TRUE)
  
  # Find "Genus" and extract the next word
  genus_match <- str_match(page_text, "Genus\\s+([A-Za-z]+)")
  genus_name <- ifelse(!is.na(genus_match[,2]), genus_match[,2], NA)
  
  family_match <- str_match(page_text, "Family\\s+([A-Za-z]+)")
  family_name <- ifelse(!is.na(family_match[,2]), family_match[,2], NA)
  
  # Check if genus or family is missing
  if (is.na(genus_name) | is.na(family_name)) {
    warning(paste("Genus or family not found on page:", url))
    next
  }
  
  # Print extracted values
  cat("Genus: ", str_to_title(genus_name), "\n")
  cat("Family: ", str_to_title(family_name), "\n")
  
  # Append results to the dataframe
  df <- data.frame(genus = str_to_title(genus_name),
                   family = str_to_title(family_name),
                   url = url,
                   num = num,
                   stringsAsFactors = FALSE)
  
  data <- rbind(data, df)
}

# Display the results
print(data)

saveRDS(data, "data/app/genus_foa_codes.RDS")

# write.csv(data, "FishesOfAustralia_species-names-to-codes.csv", row.names = FALSE)

# Create a blank data frame with the columns you want
data <- data.frame(family = character(),
                   genus = character(),
                   species = character(),
                   url = character(),
                   num = numeric(),
                   stringsAsFactors = FALSE)

for(num in 1:5579) {
  print(num)
  
  # Construct the URL
  url <- paste0("https://fishesofaustralia.net.au/home/species/", num)
  
  # Set a user agent header to identify our scraper
  headers <- c('User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36')
  
  # Send a GET request to the URL
  print(paste0("GET-ing ", url, " ..."))
  response <- httr::GET(url, headers = headers)
  httr::stop_for_status(response)
  print("Success.")
  
  options(xmlMaxDepth = 4000)
  
  # Parse the HTML content of the response with xml2
  html <- read_html(content(response, "text"), options = "HUGE")
  
  # Use xpath to extract the ul element
  ul_element <- html %>% html_node(xpath = '//*[@id="content"]/div[1]/div/ul')
  
  length(ul_element)
  
  # Check that the ul element is present
  # if (is.null(ul_element)) {
  if(length(ul_element) == 0) {
    print("Error: could not find ul element")
  } else {
    # Extract the last three li elements from the ul element
    li_elements <- ul_element %>% html_nodes("li") %>% tail(3)
    
    # Check that there are at least three li elements
    if (length(li_elements) < 3) {
      stop("Error: could not find three li elements")
    } else {
      # Extract the family, genus, and species from the text of the li elements, or the text of the link it contains
      family <- li_elements[[1]] %>% html_node("a") %>% html_text(trim = TRUE)
      genus <- li_elements[[2]] %>% html_node("a") %>% html_text(trim = TRUE)
      species <- li_elements[[3]] %>% html_text(trim = TRUE)
      
      # Print the results
      cat("Family: ", str_to_title(family), "\n")
      cat("Genus: ", str_to_title(genus), "\n")
      cat("Species: ", tolower(species), "\n")
      
      dat <- list(family = str_to_title(family),
                  genus = str_to_title(genus),
                  species = tolower(species),
                  url = url,
                  number = num)
      
      # Convert the list to a data frame and write to CSV
      df <- as.data.frame(dat, stringsAsFactors = FALSE)
      
      data <- rbind(data, df)
      
      # filename <- paste0(num, "_test.csv")
      # write.csv(df, file = filename, row.names = FALSE)
      # cat("Data saved to file: ", filename, "\n")
    }
  }
}

write.csv(data, "FishesOfAustralia_species-names-to-codes.csv", row.names = FALSE)


length(unique(data$number)) # 5495
5579 - 5495 # 84 numbers didn't work

# Create a dataframe with one column from 1 to 5579
all <- data.frame(number = 1:5579)

missing <- anti_join(all, data)
# I've checked a few of these manually and they return "Internal Server Error - 500 Error"