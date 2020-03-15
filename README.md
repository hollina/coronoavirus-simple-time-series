# Coronavirus, simple time series

This repository is a simple R script that replicate a figure I've seen circulated on-line. It is a comparison of confirmed coronavirus cases by country to Italy. The comparison is in `days since 100 confirmed cases` rather than calendar date to account for differential start-dates in exposure.  

Note: confirmed cases is certainly an undercount relative to true cases. See link at bottom of read me for more. 

<figure style="float:left;">
<img src="https://github.com/hollina/coronoavirus-simple-time-series/blob/master/output/coronavirus_cases_compared_to_italy.png" align="left"  width="800"  /> 
</figure>

## Source

This code uses the `coronavirus` package written by RamiKrispin and available here: https://github.com/RamiKrispin/coronavirus

This package pulls data from the Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE): https://coronavirus.jhu.edu

## Deaths

The data also include deaths, which have less of a clear overlap with Italy. It is likely this death count does not capture all coronavirus related deaths. 

<figure style="float:left;">
<img src="https://github.com/hollina/coronoavirus-simple-time-series/blob/master/output/coronavirus_deaths_compared_to_italy.png" align="left"  width="800"  /> 
</figure>

## True v confirmed cases

Link for information on true v confirmed cases (I have not verified accuracy of graphs/figures here): https://medium.com/@tomaspueyo/coronavirus-act-today-or-people-will-die-f4d3d9cd99ca 


The basic idea is that true infections are not detected until roughly 10 to 14 days after they occur, which is captured well by this figure from the medium article: 

<figure style="float:left;">
<img src="https://miro.medium.com/max/7168/1*r-ddYhoUtP_se6x-NOEinA.png" align="left"  width="800"  /> 
</figure>

## License

This github repo: [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

