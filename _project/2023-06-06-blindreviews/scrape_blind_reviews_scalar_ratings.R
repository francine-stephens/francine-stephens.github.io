## Scalar Review Scraping Functions Script ## 

## Overview & Getting Started

#The script contains functions to perform the following steps:

# * Find the maximum number of pages to be queried on Blind.
# * Generate all the sub-pages that make up the reviews
# * Scrape the information from each of them
# * Combine the information into one comprehensive data frame

# This script is sourced in the execute_blind_webscraper.Rmd file. 
# Parameter values for these functions are set in the first chunk of the execute file. 

################################################################################
## Step 1
# **Write Functions to Extract key Information from Each Field of the Review**
#   
# Rely on developer tools, i.e., inspect option when right clicking on feature on webpage, to find the html tag that contains the piece of the review you want to extract. 
# 
# The pieces of the review that we care about are AND can scrape: 
#   
# - Review title
# - Reviewer's job title
# - Review ratings


# Define functions that extract the key pieces of the review

## Title
get_review_title <- function(html){
  html %>% 
    # Specify the relevant tag
    html_nodes('.rvtit') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()                             
}

## Reviewer Demographics (current/former; job title)
get_reviewer_info <- function(html){
  html %>% 
    # Specify the relevant tag for the node
    html_nodes('.auth') %>% 
    html_text() %>% 
    str_trim() %>% 
    str_remove("Verified User") %>% 
    str_trim() %>%
    unlist()
}

## Star-Ratings
get_reviews <- function(html){
  html %>% 
    # Specify the relevant tag for the node
    html_nodes('.review_item_inr') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%  
    # Convert the list into a vector
    unlist()                             
}

################################################################################

## Step 2

#*Scrape Website*

get_data_table <- function(html){
  
  # Extract the Basic information from the HTML
  title <- get_review_title(html) 
  review_info <- get_reviewer_info(html)
  reviews <- get_reviews(html)
  
  
  # Combine into a tibble
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
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    # Apply to all URLs
    map(get_data_from_url) %>%  
    # Combine the tibbles into one tibble
    bind_rows()      
}

scraped_data <- scrape_write_table(url) 


################################################################################
## Step 3 

#*Create a clean data table & Export for Analysis**
clean_scraped_data <- function(x) {
  x %>%
    mutate(
      #Overall Rating
      overall_rating = str_extract(review,
                                   "Rating Score[[:digit:]]" ),
      overall_rating = str_remove(overall_rating,
                                  "Rating Score"), 
      #Career Growth
      career_growth_rating = str_extract(review, 
                                         "[[:digit:]]Career"),
      career_growth_rating = str_extract(career_growth_rating, "[[:digit:]]"),
      #WLB 
      wlb_rating = str_extract(review,
                               "[[:digit:]]Work - Life Balance"),
      wlb_rating = str_extract(wlb_rating, 
                               "[[:digit:]]"),
      #Compensation / Benefits
      compensation_rating = str_extract(review, 
                                        "[[:digit:]]Compensation / Benefits"),
      compensation_rating = str_extract(compensation_rating, 
                                        "[[:digit:]]"),
      #Company Culture
      company_culture_rating = str_extract(review,
                                           "[[:digit:]]Company"),
      company_culture_rating = str_extract(company_culture_rating, 
                                           "[[:digit:]]"),
      #Management 
      management_rating = str_extract(review, 
                                      "[[:digit:]]Management"),
      management_rating = str_extract(management_rating, 
                                      "[[:digit:]]"),
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
    mutate(across(ends_with("_rating"), ~as.numeric(.x))) %>% 
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

