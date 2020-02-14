# Inspired by: https://github.com/BenGlicksberg/PatientExploreR/blob/master/app.R
# install.packages("timevis")
library(timevis)

library(data.table)
library(magrittr)
library(RColorBrewer)
library(grDevices)
library(lubridate)

library(shiny)

log_start <- lubridate::now()

empty <- data.table(groups = character(),
                    subgroups = character(),
                    start = as.Date(character()),
                    end = as.Date(character()),
                    content = character(),
                    content_type = character(),
                    headline = character(),
                    desc = character(),
                    className = character())

# Parameters for data generation
num_ev <- 100
num_periods <- 50
start_date <- as.Date('2017-01-01')
end_date <- as.Date('2019-01-01')
person <- c("Tim", "Steve", "John")

### event data generation ----
types <- c("one", "two", "three")
ev_classNames <- c("mild", "medium", "spicy")
events <- data.frame(person = sample(person, num_ev, replace = TRUE),
                     ev_id = seq(1, num_ev, length = num_ev),
                     groups = "event",
                     subgroups = sample(types, num_ev, replace = TRUE),
                     start = sample(seq(start_date, end_date, by = "day"), num_ev),
                     end = as.Date(NA),
                     content = "event description",
                     headline = "Event happened",
                     desc = "Event information",
                     stringsAsFactors = FALSE) %>%
  dtplyr::lazy_dt(.) %>%
  dplyr::mutate(className = ifelse(subgroups == "one", sample(ev_classNames, num_ev, replace = TRUE), "normal"), 
                content_type = subgroups) %>%
  data.table::as.data.table(.)

### time range generation ---- 
period_types <- c("alpha", "beta", "gamma")
periods <- data.frame(person = sample(person, num_periods, replace = TRUE),
                      ev_id = seq(num_ev+1, num_ev + num_periods+1, length = num_periods),
                      groups = "period",
                      subgroups = sample(period_types, num_periods, replace = TRUE),
                      start = sample(seq(start_date, end_date, by = "day"), num_periods),
                      end = sample(seq(start_date, end_date, by = "day"), num_periods),
                      content = "period description",
                      headline = "Period of X",
                      desc = "Period information",
                      stringsAsFactors = FALSE) %>%
  dtplyr::lazy_dt(.) %>%
  dplyr::mutate(className = subgroups, 
                content_type = subgroups,
                end = as.Date(ifelse(end < start, start + 14, end), origin = "1970-01-01")) %>%
  data.table::as.data.table(.)

### combine events and periods ----
all_data <- dplyr::bind_rows(events, periods)



### Parameters for grouping and styling -------
colour_col <- 'content_type'
group_col <- 'groups'
qual_col_pals <- RColorBrewer::brewer.pal.info[brewer.pal.info$category == 'qual',]

# Set group colours -------
# For some classes, we want fixed colors because they are meaningful
one_palette <- data.table(subgroups = 'one',
                         className = ev_classNames,
                         color = brewer.pal(length(ev_classNames), "RdYlBu"))
# For other classes, we automatically set them
two_palette <- data.table(subgroups = 'two',
                          className = "normal",
                          color = brewer.pal(length("normal"), "Accent"))
three_palette <- data.table(subgroups = 'three',
                          className = "normal",
                          color = brewer.pal(length("normal"), "Paired"))

class_cols <- do.call("rbind", list(one_palette, two_palette, three_palette))

# For the rest, we generate random colours
all_classnames <- unique(all_data[, .(subgroups, className)])[className != ""]
n <- length(unique(all_classnames$className))
palettes <- qual_col_pals[rownames(qual_col_pals) != "Accent", ]
col_code <- sample(unlist(mapply(brewer.pal, palettes$maxcolors, rownames(palettes))), n, replace = TRUE)
class_cols$codes <- col_code[1:nrow(class_cols)]
class_cols$color <- ifelse(!is.na(class_cols$color), class_cols$color, class_cols$codes)

# Generate CSS to go into the HTML tags
CSS_colors <- paste0(".", class_cols$className, " {background-color:", class_cols$color, 
                     "; border-color:", class_cols$color, ";} 
                     .vis-item.vis-dot.", class_cols$className, " {opacity:0.5;}", 
                     collapse = " ")


grouped_html <- copy(class_cols)[, .(cols = paste0("<font color=\'", color, "\'><li>", 
                                                   className, "</font></li><br/>", collapse = " ")), 
                                 .(subgroups)] %>%
  .[, .(group_html = paste0("<font size=10px><b>", subgroups, "</b></font><br/><ul>", cols, "</ul>"))]

print("Prepare stylings")

