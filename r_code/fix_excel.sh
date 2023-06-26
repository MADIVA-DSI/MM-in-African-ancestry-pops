#!/bin/bash

## CONCATENATE BROKEN LINES (LOOP CURRENT BROKEN LINE AS LONG AS IT HAS LESS THAN 10 FIELDS - FILE SHOULD HAVE 11
## id|clinical_summary|date|date_has_time|event_id|created_at|updated_at|level_of_care_id|discharge_type_id|has_autopsy|discharge_location
LC_ALL=C sed -e 's/	/|/g; s//\n/g' MM_review_data_June2023.txt > tmp_1.tsv
LC_ALL=C awk -F"|" '{ while (NF < 58) { brokenline=$0; getline; $0 = brokenline $0}; print }' tmp_1.tsv > tmp_2.tsv
LC_ALL=C iconv -f utf-8 -t utf-8 -c tmp_2.tsv > tmp_3.tsv
cut -d'|' -f 1-58 tmp_3.tsv > the_data.tsv
sed -i.bak '/^|||||||||||||||||||||||||||||||||||||||||||||||||||||||||$/d' the_data.tsv

# ## REMOVE THAT SECOND IRRITATINF SUMMARY COLUMN!
# cut -d'|' -f 1,3- new_file.csv > new_file_minimal.csv

# ## REPLACE PIPE DELIMITER WITH A TAB DELIMETER
# sed -n 's/|/\t/gp' new_file_minimal.csv > new_file_minimal_tab.csv 
