library(magrittr)
library(polite)
library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)

sanitiser_url <- paste0('https://www.amazon.co.uk/s?k=hand+sanitiser&i=drugstore&rh=n%3A65801031%2Cn%3A2826561031&dc&page=', 
                        seq(1, 25), '&qid=1583507458&rnid=1642204031&ref=sr_pg_2')

sanitiser_scrape <- function(search_url){
  bow_page <- polite::bow(search_url)
  
  scrape_products <- polite::scrape(bow_page) %>%
    rvest::html_nodes('div.s-result-list') %>%           # Results
    rvest::html_nodes('div[data-index]') %>%             # All boxes
    rvest::html_nodes('div.a-section.a-spacing-medium')  # Only relevant bits
  
  names <- scrape_products %>%
    rvest::html_nodes('h2') %>%
    rvest::html_text()
  volumes <- data.frame(raw = stringr::str_extract(names, '[0-9]+ ?[mM]?.[lL]{1}')) %>%
    dplyr::mutate(num = as.integer(stringr::str_extract(raw, '[0-9]+')),
                  vol = stringr::str_extract(raw,'[a-zA-Z]+')) %>%
    dplyr::mutate(volume = ifelse(grepl('[mM]', vol), 
                                  num, 
                                  num * 1000))
  all_info <- scrape_products %>%
    rvest::html_text() 
  prices <- all_info %>%
    stringr::str_extract('£[0-9]*\\.[0-9]*') %>%
    stringr::str_extract('[0-9]*\\.[0-9]*') %>%
    as.double()
  delivery_cost <- all_info %>%
    stringr::str_extract('£[0-9]*\\.[0-9]* delivery')%>%
    stringr::str_extract('[0-9]*\\.[0-9]*') %>%
    as.double()
  
  results_df <- data.frame(date = rep(Sys.Date(), length(names)),
                           names = names,
                           prices = prices,
                           delivery = delivery_cost,
                           volume = volumes$volume,
                           stringsAsFactors = FALSE)
  return(results_df)
}

scrape_results <- purrr::map_dfr(sanitiser_url, sanitiser_scrape) %>%
  dplyr::distinct()

all_results <- readr::read_csv('data/sanitiser_scrape.csv')
results <- rbind(all_results, scrape_results) %>% dplyr::distinct()


sanitiser_results <- results %>%
  dplyr::mutate(ppv = prices / volume,
                whole_price = prices + ifelse(is.na(delivery), 0, delivery)) %>%
  dplyr::mutate(dppv = whole_price / volume) %>%
  dplyr::filter(!is.na(ppv))

ggplot(sanitiser_results)+
  geom_histogram(aes(x = ppv))+
  geom_histogram(aes(x = dppv), fill = 'red', alpha = 0.5)



sanitiser_results %>%
  dplyr::filter(ppv < quantile(sanitiser_results$ppv, 0.99)) %>% # Remove outliers
  dplyr::group_by(date) %>%
  dplyr::summarise(Median = median(ppv),
                   med_dppv = median(dppv),
                   Mean = mean(ppv),
                   mean_dppv = mean(dppv),
                   sd_ppv = sd(ppv),
                   sd_dppv = sd(dppv),
                   num = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(se_ppv = sd_ppv / sqrt(num),
                se_dppv = sd_dppv / sqrt(num)) %>%
  tidyr::pivot_longer(cols = Median:mean_dppv,
                      names_to = 'metric') %>%
  dplyr::mutate(upper_bound = ifelse(grepl('[mM]ean', metric),
                                           ifelse(grepl('dppv', metric), 
                                                  value + 1.96 * se_dppv, 
                                                  value + 1.96 * se_ppv),
                                           value),
                lower_bound = ifelse(grepl('[mM]ean', metric),
                                     ifelse(grepl('dppv', metric), 
                                            value - 1.96 * se_dppv, 
                                            value - 1.96 * se_ppv),
                                     value)) %>%
  dplyr::filter(metric %in% c('Mean', 'Median')) %>%
  ggplot(aes(x = date))+
  geom_ribbon(aes(ymin=lower_bound, # Error bands
                  ymax=upper_bound, 
                  fill = metric),
              alpha=0.25)+
  geom_line(aes(y = value, 
                color = metric),
            size = 1)+ # Point estimates
  labs(title = 'Amazon.co.uk price per volume of liquid hand sanitiser products over time',
       subtitle = 'Error band represents 1.96 * Standard Error',
       x = '',
       y = 'Price per mL (£)')+
  theme_minimal()+
  scale_colour_brewer(
    type = "qual",
    palette = 7,
    aesthetics = c("colour", 'fill')
  )

write.csv(results, 'data/sanitiser_scrape.csv', 
          row.names = FALSE)
