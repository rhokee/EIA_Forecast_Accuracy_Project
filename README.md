Starting in 1979, the EIA published an Annual Energy Outlook report in which they forecast energy production for most major energy resources. During the 2023-2024 academic year, several faculty and students across the St. Lawrence University Environmental Studies and MSCS (Math, Statistics, & Computer Science) Departments, including myself, came together to conduct research on the accuracy of these forecasts. This project was guided by the following questions: Does the EIA NEMS model make accurate forecasts? Has the NEMS model improved in accuracy over time? Does the size of the resource influence accuracy? Which resources are predicted with the most/least accuracy? And finally, is there any difference in prediction accuracy for production, consumption, and renewable energy generation?

My role in the project was to create an interactive Shiny App website that analyzes and visualizes the forecast data. This site builds multiple plots, according to certain criteria that are determined by a user’s inputs, that help visualize forecast accuracy over time for different resources.

As shown by the Shiny App, the NEMS model often overestimates the production and consumption of non-renewable energy sources and often underestimates the production and consumption of renewable energy sources. This is likely due to the fact that more recent trends in energy production and consumption have diverged from historical trends; increased awareness of the harmful impacts of non-renewable energy sources, as well as decreased prices of renewables, have caused our society to undergo a major shift in how we produce and consume our energy. A future goal of mine could therefore be to attempt to create an energy forecast model that more accurately takes into account these more recent trends when making its predictions.

The code for the Shiny App is contained within the file "app.R", and the data used in this project is contained within the file "energy_forecast_data.csv".

Additionally, a link to the Shiny App can be found below:

https://stlawu.shinyapps.io/EIA_Data_Analysis_v2/
