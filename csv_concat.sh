#!/bin/bash


direc=$(dirname `pwd`)
series=$(basename  $direc )

head -1 `ls first_innings_batting*.csv|head -n 1` > "../$series.first_innings_batting.csv"

for filename in first_innings_batting*.csv; do sed 1d $filename >> "../$series.first_innings_batting.csv";done


head -1 `ls second_innings_batting*.csv|head -n 1` > "../$series.second_innings_batting.csv"

for filename in second_innings_batting*.csv; do sed 1d $filename >> "../$series.second_innings_batting.csv";done



head -1 `ls second_innings_bowling*.csv|head -n 1` > "../$series.second_innings_bowling.csv"

for filename in second_innings_bowling*.csv; do sed 1d $filename >> "../$series.second_innings_bowling.csv";done

head -1 `ls first_innings_bowling*.csv|head -n 1` > "../$series.first_innings_bowling.csv"

for filename in first_innings_bowling*.csv; do sed 1d $filename >> "../$series.first_innings_bowling.csv";done

head -1 `ls match_details*.csv|head -n 1` > "../$series.match_details.csv"

for filename in match_details*.csv; do sed 1d $filename >> "../$series.match_details.csv";done	

head -1 `ls score_details*.csv|head -n 1` > "../$series.score_details.csv"

for filename in score_details*.csv; do sed 1d $filename >> "../$series.score_details.csv";done
