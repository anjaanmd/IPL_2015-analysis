#!/bin/bash


direc=$(dirname `pwd`)
series=$(basename  $direc )

head -1 `ls *_batting*.csv|head -n 1` > "../$series.batting.csv"

for filename in *_batting*.csv; do sed 1d $filename >> "../$series.batting.csv";done

head -1 `ls *_bowling*.csv|head -n 1` > "../$series.bowling.csv"

for filename in *_bowling*.csv; do sed 1d $filename >> "../$series.bowling.csv";done

head -1 `ls match_details*.csv|head -n 1` > "../$series.match_details.csv"

for filename in match_details*.csv; do sed 1d $filename >> "../$series.match_details.csv";done	

head -1 `ls score_details*.csv|head -n 1` > "../$series.score_details.csv"

for filename in score_details*.csv; do sed 1d $filename >> "../$series.score_details.csv";done
