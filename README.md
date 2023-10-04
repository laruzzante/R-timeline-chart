# R-timeline-chart
R code to plot a timeline chart to monitor business/work interventions (activities) and workload spread, based on the "vistime" R package.

This code is suited for cases where there are scheduled times and possible min/max/average durations of interventions, for which cumulative timeline charts are desired.

The input is an Excel datasheet with:
1. A datetime column specifying the scheduled startint time of each intervention.
2. A category of activity column (for grouping related activities and graphically separating the categories).
3. Intervention names.
4. Intervention abbreviation labels (for cleaner graphs).

An example Excel datasheet is provided. No need to completely fill in the "abbreviations" label as missing values will be filled with corresponding row's intervention field.

Output (polished) example timeline chart:
relative%20path/to/img.jpg?raw=true
![Alt text](example_timeline_chart.jpg "Nursing activities in clinic")
