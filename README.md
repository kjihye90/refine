# refine
data wraggling (1st exercise)

library(dplyr)

library(tidyr)

#Clean up brand names
refine_original <- transform (refine_original, company=tolower(company))
refine_original$company[refine_original$company=="phillips"]<-"philips"
refine_original$company[refine_original$company=="phillps"]<-"philips"
refine_original$company[refine_original$company=="fillips"]<-"philips"
refine_original$company[refine_original$company=="phlips"]<-"philips"
refine_original$company[refine_original$company=="akz0"]<-"akzo"
refine_original$company[refine_original$company=="ak zo"]<-"akzo"
refine_original$company[refine_original$company=="azko"]<-"akzo"
refine_original$company[refine_original$company=="unilver"]<-"unilever"

#Separate product code and name
refine_original <- separate(refine_original, Product.code...number, c("product_code", "product__number"), sep = "-")

#Add product category
refine_original <- mutate(refine_original, "product_category"
                          =ifelse(product_code == "p", "Smartphone", "")) %>%
  mutate("product_category" = ifelse(product_code == "x", "Laptop", product_category)) %>%
  mutate("product_category" = ifelse(product_code == "q", "Tablet", product_category)) %>%
  mutate("product_category" = ifelse(product_code == "v", "TV", product_category))

#Add full address
refine_original <- unite(refine_original, "full_address", address, city, country, sep = ",")

#Create dummy variables
refine_original <- mutate(refine_original, "company_philips" = ifelse(company == "philips", 1, 0)) %>%
  mutate("company_akzo" = ifelse(company == "akzo", 1, 0)) %>%
  mutate("company_van_houten" = ifelse(company == "van houten", 1, 0)) %>%
  mutate("company_unilever" = ifelse(company == "unilever", 1, 0))

refine_original <- mutate(refine_original, "product_smartphone" = ifelse(product_category == "Smartphone", 1, 0)) %>%
  mutate("product_TV" = ifelse(product_category == "TV", 1, 0)) %>%
  mutate("product_Laptop" = ifelse(product_category == "Laptop", 1, 0)) %>%
  mutate("product_Tablet" = ifelse(product_category == "Tablet", 1, 0))
