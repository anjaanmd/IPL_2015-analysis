# IPL_2015-analysis
Analysing the IPL 2015 in numbers


Using Beautiful soup and other python packages generated the following csv files:

- first_innings_batting.match_id.csv 
- first_innings_bowling.match_id.csv 
- match_details.match_id.csv 
- score_details.match_id.csv 
- second_innings_batting.match_id.csv 
- second_innings_bowling.match_id.csv

These files are generated using scraper.py(which is available in an another repo of mine in this github account) for every match.

Using bash concatenated the relevant files to make the following files:

- 2015_ipl.batting.csv	
- 2015_ipl.bowling.csv	
- 2015_ipl.match_details.csv	
- 2015_ipl.score_details.csv	


Then these files are added to a MySQL database.



<div>
    <a href="https://rawgit.com/anjaanmd/IPL_2015-analysis/draft/files/runs.html" target="_blank" title="click to see interactive graph" style="display: block; text-align: center;"><img src="https://cdn.rawgit.com/anjaanmd/IPL_2015-analysis/draft/runs.svg" alt="runs.svg" style="max-width: 100%;width: 600px;"  width="600"/></a>
</div>
