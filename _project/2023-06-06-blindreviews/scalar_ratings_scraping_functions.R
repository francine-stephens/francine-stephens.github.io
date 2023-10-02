## Scalar Review Scraping Functions Script ## 

## Overview & Getting Started

# The script contains functions to perform the following steps:

# * Find the maximum number of pages to be queried on Blind.
# * Generate all the sub-pages that make up the reviews
# * Scrape the information from each of them
# * Combine the information into one comprehensive data frame

# This script is sourced in the execute_blind_webscraper.Rmd file. 
# Parameter values for these functions are set in the first chunk of the execute file. 

################################################################################
## Step 1
# **Extract key Information from Each Field of the Review**
#   
# Rely on developer tools.
# Use the inspect option when right clicking on feature on webpage
# to find the html tag that contains the piece of the review you want to extract. 
# 
# The pieces of the review that we care about are AND can scrape: 
#   
# - Review title
# - Reviewer's job title
# - Review ratings

## Trim white space and convert extracted information to vectors
trim_and_unlist <- function(x) { 
  x %>% 
  str_trim() %>%                       
  unlist() 
  }


## Title
get_review_title <- function(html){
  html %>% 
    # Specify the relevant tag
    html_nodes('.rvtit') %>%      
    html_text() %>% 
    trim_and_unlist()
}

## Reviewer Demographics (current/former; job title)
get_reviewer_info <- function(html){
  html %>% 
    html_nodes('.auth') %>% 
    html_text() %>% 
    str_remove("Verified User") %>% 
    trim_and_unlist()
}

## Star-Ratings
get_reviews <- function(html){
  html %>% 
    # Specify the relevant tag for the node
    html_nodes('.review_item_inr') %>%      
    html_text() %>% 
    trim_and_unlist()                          
}

################################################################################

## Step 2

# Scrape Website

get_data_table <- function(html){
  
  # Extract the Basic information from the HTML
  title <- get_review_title(html) 
  review_info <- get_reviewer_info(html)
  reviews <- get_reviews(html)
  
  
  # Combine all info
  combined_data <- tibble(
    reviewer = review_info,
    title = title,
    review = reviews
  ) 
}


get_data_from_url <- function(url){
  html <- read_html(url)
  get_data_table(html)
}


scrape_write_table <- function(url){
  
  # Apply the extraction functions and bind the results into one table 
  list_of_pages %>% 
    # Apply to all URLs
    map(get_data_from_url) %>%  
    # Combine the tibbles into one tibble
    bind_rows()      
}

scraped_data <- scrape_write_table(url) 


################################################################################
## Step 3 

# Create a clean data table & Export for Analysis
clean_scraped_data <- function(x) {
  x %>%
    # Ratings
    mutate(
      overall_rating = str_extract(review,
                                   "Rating Score[[:digit:]]" ),
      career_growth_rating = str_extract(review, 
                                         "[[:digit:]]Career"),
      wlb_rating = str_extract(review,
                               "[[:digit:]]Work - Life Balance"),
      compensation_rating = str_extract(review, 
                                        "[[:digit:]]Compensation / Benefits"),
      company_culture_rating = str_extract(review,
                                           "[[:digit:]]Company"),
      management_rating = str_extract(review, 
                                      "[[:digit:]]Management")
      ) %>% 
    mutate(across(ends_with("_rating"), ~str_extract(.x, "[[:digit:]]"))) %>% 
    mutate(across(ends_with("_rating"), ~as.numeric(.x))) %>% 
    mutate(
      # Review Title
      title = str_remove(title, "“"),
      title = str_remove(title, "”"),
      title = str_trim(title, side = "both"),
      # Employment Status
      emp_status = sub("\\·.*", "", reviewer),
      # Review Date
      date = sub('.*-', '', reviewer),
      date = str_trim(date, side = "both"),
      # Reviewer Job Title
      job_title = sub(".*·", "", reviewer),
      job_title = sub('-.*', "", job_title),
      job_title = str_trim(job_title, side = "both")
    ) %>% 
    select(-review, -reviewer) %>% 
    relocate(date,
             emp_status, 
             job_title, 
             overall_rating, 
             career_growth_rating, 
             wlb_rating, 
             compensation_rating, 
             company_culture_rating, 
             management_rating,
             title)  %>% 
    separate(date,
             c("Month", "Day", "Year"),
             sep = " ",
             remove = FALSE) %>% 
    mutate(Month = match(Month, month.abb),
           Day = as.numeric(str_remove(Day, ",")),
           Year = as.numeric(Year),
           Date_obj = make_date(Year, Month, Day)
    ) %>% 
    relocate(date, Date_obj) %>% 
    arrange(desc(Date_obj))
}

