
library(Tplyr)
library(knitr)
library(admiral)
library(rtables)
library(tern)
library(dplyr)
library(pharmaRTF)

adsl <- df_explicit_na(admiral_adsl)

t <- tplyr_table(adsl, TRT01P, where = SAFFL == "Y") %>% 
  add_layer(
    group_desc(AGE,by="Age (years)")
  )%>%
  add_layer(
    group_count(AGEGR1)
  )%>%
  add_layer(
    group_count(RACE,by="Race")
  ) %>% 
  add_layer(
    group_count(SEX,by="Sex, n (%)")
  )  %>% 
  add_layer(
    group_count(TRT01A,by="TRT01A")
  )  %>% 
  add_layer(
    group_count(RACE,by="Race")
  ) %>%
  build(metadata=TRUE) %>% 
  arrange(ord_layer_index, ord_layer_1, ord_layer_2) %>% 
  apply_row_masks(row_breaks = TRUE) %>% 
  select(starts_with("row_label"), var1_Placebo, `var1_Xanomeline Low Dose`, `var1_Xanomeline High Dose`) 




# Make the table
ht <- huxtable::as_hux(t, add_colnames=TRUE) %>%
  huxtable::set_bold(1, 1:ncol(t), TRUE) %>% # bold the first row
  huxtable::set_align(1, 1:ncol(t), 'center') %>% # Center align the first row 
  huxtable::set_align(2:nrow(t), 3:ncol(t), 'center') %>% # Center align the results
  huxtable::set_valign(1, 1:ncol(t), 'bottom') %>% # Bottom align the first row
  huxtable::set_bottom_border(1, 1:ncol(t), 1) %>% # Put a border under the first row
  huxtable::set_width(1.5) %>% # Set the table width
  huxtable::set_escape_contents(FALSE) %>% # Don't escape RTF syntax
  huxtable::set_col_width(c(.2, .2, .15, .15, .15, .15))
  # Set the column widths
  
  ht

doc <- pharmaRTF::rtf_doc(ht) %>% 
  pharmaRTF::add_titles(
    pharmaRTF::hf_line("Protocol: CDISCPILOT01", "PAGE_FORMAT: Page %s of %s", align='split', font="Times New Roman"),
    pharmaRTF::hf_line("Table 14-2.01", align='center', italic = TRUE),
    pharmaRTF::hf_line("Summary of Demographic and Baseline Characteristics", font_size = 15)
  ) %>% 
  pharmaRTF::add_footnotes(
    pharmaRTF::hf_line("FILE_PATH: Source: %s", "DATE_FORMAT: %H:%M %A, %B %d, %Y", align='center', bold=FALSE, italic=TRUE)
  ) %>% 
  pharmaRTF::set_font_size(10) %>%
  pharmaRTF::set_ignore_cell_padding(TRUE)


write_rtf(doc, file="table.rtf")

