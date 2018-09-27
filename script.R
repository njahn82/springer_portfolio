library(tidyverse)
library(rcrossref)
tt <- readxl::read_xlsx("v1.xlsx", skip = 8)
imprints <-
  c(
    "Springer",
    "Springer Nature",
    "BioMed Central",
    "Nature Research",
    "Palgrave Macmillan"
  )
tt %>%
  filter(!grepl("No longer published", Comment)) %>%
  mutate(imprints = ifelse(Imprint %in% imprints, Imprint, "other")) -> my_df
my_df %>% count(imprints, `Open Access`)
my_df %>%
  gather(`ISSN print`,
         `ISSN electronic`,
         key = "issn_type",
         "value" = issn) %>%
  filter(!is.na(issn)) -> issn_spread
issns_list <-
  purrr::map(uniqe(issn_spread$product_id), function(x) {
    issns <- issn_spread %>%
      filter(product_id == x) %>%
      .$issn
    names(issns) <- rep("issn", length(issns))
    as.list(issns)
  })
jn_facets <- purrr::map(issns_list, .f = purrr::safely(function(x) {
  tmp <- rcrossref::cr_works(tmp <- rcrossref::cr_works(
    filter = c(
      x,
      from_pub_date = "2015-01-01",
      until_pub_date = "2018-12-31",
      type = "journal-article"
    ),
    facet = TRUE,
    # less api traffic
    select = "DOI"
  )
  #' Parse the relevant information
  #' - `issn` - issns  found in open apc data set
  #' - `year_published` - published volume per year (Earliest year of publication)
  #' - `license_refs` - facet counts for license URIs of work
  #' - `journal_title` - Crossref journal title (in case of journal name change, we use the most frequent name)
  #' - `publisher` - Crossref publisher (in case of publisher name change, we use the most frequent name)
  #'
  #' To Do: switch to current potential
  if (!is.null(tmp)) {
    tibble::tibble(
      issn = unlist(x[1]),
      year_published = list(tmp$facets$published),
      license_refs = list(tmp$facets$license)
      #    journal_title = tt$facets$`container-title`$.id[1],
      #    publisher = tt$facets$publisher$.id[1]
    )
  } else {
    NULL
  }
}))
jn_facets %>%
  map_df("result") %>%
  left_join(issn_spread, by = "issn") %>%
  unnest(year_published) %>%
  write_csv("jn_facets_y_published.csv")
jn_facets %>%
  map_df("result") %>%
  left_join(issn_spread, by = "issn") %>%
  unnest(license_refs) -> license_all
write_csv(license_all, "license_all.csv")
license_all %>%
  mutate(license_ref = tolower(.id)) %>%
  select(-.id) %>%
  mutate(hybrid_license = ifelse(grepl(
    "creativecommons",
    license_ref
  ), TRUE, FALSE)) %>%
  filter(hybrid_license == TRUE, !`Open Access` %in% "Fully Open Access") -> license_sub
issn_spread %>%
  select(product_id, issn) %>%
  filter(product_id %in% license_sub$product_id) -> issn_license
issns_list <-
  purrr::map(issn_license$product_id, function(x) {
    issns <- issn_spread %>%
      filter(product_id == x) %>%
      .$issn
    names(issns) <- rep("issn", length(issns))
    as.list(issns)
  })
issn_license %>%
  mutate(issns_list = issns_list) %>%
  inner_join(license_sub, by = "product_id") %>%
  distinct(product_id, license_ref, .keep_all = TRUE) -> oa_license_df
cr_license <- purrr::map2(oa_license_df$license_ref, oa_license_df$issns_list,
                          .f = purrr::safely(function(x, y) {
                            u <- x
                            issn <- y
                            tmp <- rcrossref::cr_works(filter = c(issn, 
                                                                  license.url = u, 
                                                                  license.delay = 0,
                                                                  type = "journal-article",
                                                                  from_pub_date = "2013-01-01", 
                                                                  until_pub_date = "2018-12-31"),
                                                       cursor = "*", cursor_max = 5000L, 
                                                       limit = 1000L) 
                            tibble::tibble(
                              issn =  unlist(issn[1]),
                             # year_published = list(tmp$facets$published),
                              license = u,
                              md = list(tmp$data)
                            )
                          }))
#' into one data frame!
#' dump it
cr_license_df <- cr_license %>% 
  purrr::map_df("result") 
cr_license_df %>% 
  unnest(md) %>% 
  select(1:27) %>% 
  write_csv("hybrid_oa.csv")

