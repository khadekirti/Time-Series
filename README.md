# Time-Series

Translink is a division of the Queensland government’s Department of Transport and Main  Roads. They operate the go card system which allows card-based access and payment for South East Queensland public transport systems, including buses, trains, ferries and trams. Some of the data collected via go cards has been analysed by UQ’s Dr. Jiwon Kim of the School of Civil Engineering (see https://www.jiwonkim.co/ ). Translink is interested in monitoring and predicting passenger numbers and demand to help with planning changes to routes and their frequency, and how to best respond to unusual incidents and events.

A subset of the dataset is available from 25 Feb to 24 Mar 2013. Task is to construct predictive models based on this data for a single high-traffic pair of regions, namely from region 1 to region 5 (Brisbane City to South Brisbane). Considering the v0_num_traj column (not the v10 – v60 columns) –this is the number of passenger trajectories (trips using all forms of public transport) occurring during the listed time period (hour) for the listed route. The time_id shows the hour since midnight, making the start time and end time redundant, since these are just in minutes since midnight.  This data also has presence of weekends. 

TransLink is interested to analyse busier time periods. 

There are two types of model created in this project. 

<b> Model – 1 </b>:  The first model is based entirely on reasonable summaries (e.g. averages) from past data, without a stochastic model (see for example https://otexts.com/fpp2/simple-methods.html).

<b> Model – 2 </b> :  The second should involve the fitting of a stochastic time series model.

Other aspects of the report/code : 
  - Check for stationarity and seasonality of the time-series data
  - Choose and detail each type of model used (including mathematical form and explanation of notation) and some details of how it was fitted. Explain why each      model may be suitable for this type of data
  - Report full details of each fitted mod
  - Discuss the limitations of each model with respect to this dataset
  - Give and plot model predictions for each model over the observed data range (include the observed values somehow for comparison). Also give 95% predictive intervals for the stochastic model over this range
  - Evaluate accuracy 1 and 2 hours ahead via final day
  - Make predictions for test day, which will be evaluated by RMSE vs true counts for that day 
  - Include a paragraph aimed at a member of the TransLink planning staff who may not have a statistics background, explaining how your modelling could potentially help them make decisions about how many bus/train/ferries services to run at various times to meet demand.


Credits: Dr.Ian Wood
UQ, Masters of Data Science 
DATA7202 Statistical Methods for Data Science
