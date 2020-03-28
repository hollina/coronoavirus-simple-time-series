# Coronavirus, simple time series

## Deaths compared to Italy

A comparison of confirmed coronavirus deaths by country to Italy. The comparison is in `days since 100 confirmed deaths` rather than calendar date to account for differential start-dates in exposure.  

<p align="center">
  <img  width="800" ssrc="https://github.com/hollina/coronoavirus-simple-time-series/blob/master/output/coronavirus_deaths_compared_to_italy.png" ">
</p>
## Cases compared to New York

A comparison of confirmed coronavirus cases by state to New York. The comparison is in `days since 10 confirmed cases` rather than calendar date to account for differential start-dates in exposure.  

<p align="center">
  <img  width="800" src="https://github.com/hollina/coronoavirus-simple-time-series/blob/master/output/coronavirus_ln_cases_by_state.png">
</p>



## Source

Country level data are  from the Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE): https://github.com/CSSEGISandData/COVID-19
State level data are from the New York Times: https://github.com/nytimes/covid-19-data


## True v confirmed cases

Link for information on true v confirmed cases (I have not verified accuracy of graphs/figures here): https://medium.com/@tomaspueyo/coronavirus-act-today-or-people-will-die-f4d3d9cd99ca 


The basic idea is that true infections are not detected until roughly 10 to 14 days after they occur, which is captured well by this figure from the medium article: 

<figure style="float:left;">
<img src="https://miro.medium.com/max/7168/1*r-ddYhoUtP_se6x-NOEinA.png" align="left"  width="800"  /> 
</figure>

## Motivation

This repository was started tp create a simple R script that replicates a figure circulated on-line. 

Original figures can be found at these two links

- https://www.motherjones.com/kevin-drum/2020/03/update-the-united-states-is-not-a-coronavirus-outlier/
- https://www.ft.com/content/ff3affea-63c7-11ea-b3f3-fe4680ea68b5

## License

This github repo: [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

