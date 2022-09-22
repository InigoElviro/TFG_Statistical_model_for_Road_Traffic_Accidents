# TFG_Statistical_model_for_Road_Traffic_Accidents

Road traffic accidents in Spain are one of the main causes of death and injury, in particular the work-related traffic accidents (WTA) are the principal cause of sick leave in Spain and casualties in work-related accidents. WTA can be divided  into two kinds, 'in itinere' for those commuting travels between the workplace and the residence of the worker, and in mission, this is, during working time. The high economic costs associated to the mishap fall upon both the injured as well as upon the society as a whole and the company.

The industrial parks of Zaragoza register most part of the WTA, warning about the need for taking measures to alleviate the situation. Although there exist reports that collect and compare partially the situation of said industrial parks, it does not exist a  systematic methodology that allows to analyse the accidents.

The objectives of the study are, firstly, to reproduce the report of the Instituto Aragonés de Seguridad y Salud Laboral (ISSLA) for 2021, corresponding to the period 2017-19, and the findings that in itself are expressed, as well as to analyse if said findings can be extrapolated to the preceding and subsequent periods of the report.

Secondly, through the use of categorical variables, ubication-wise and temporary-wise,  statistical models are built that analyse the response, which is to say, the number of accidents per hour and day. In particular, two approaches have been considered for the study of the accidents.

A first statistical model, over the data base between years 2009 and 2021, employs the implemented categorical variables, and the use of the hourly and daily harmonics. Upon this model it has been also studied the impact of  COVID-19, both the first stage of confinement, and the successive states of alarm.

In the second model it is taken as data base the restricted one to the location of the 4 important industrial parks of Zaragoza. This way risks are estimated and compared between industrial parks.

For both models it has been analysed a  brief economic assessment that may help to take future decisions in terms of reducing the risk of accident.


## How to use the different R files 

In this section is briefly disccused how to best employ the R files develpoed in this project:

#### Main code

-01 - **Librerias**: Helpful R libraries used.

-02 - **Funciones**: Own functions developed to ease the analysis of the response.

-03 - **Lectura e inicializacion**: Loading step of the database, and introduction of new variables, ubication-wise and temporary-wise, and categorical variables.

-04 - **Informe_ISSLA**: Reproduction of the report of the Instituto Aragonés de Seguridad y Salud Laboral (ISSLA) for 2021, corresponding to the period 2017-19.

-04.5.1 - **Informe_ISSLA_Ampliación**: Extrapolation of the report to the preceding and subsequent periods of the report.

-04.5.2 - **Comparativa**: Brief comparison of the most significant results. 

-05 - **Exploratorio**: Exploratory analysis of the reponse, which is to say, the number of accidents per hour and day. In particular, two approaches have been considered for the study of the accidents.

-06 - **Modelizacion**: First statistical model, over the data base between years 2009 and 2021, employs the implemented categorical variables, and the use of the hourly and daily harmonics. Upon this model it has been also studied the impact of  COVID-19, both the first stage of confinement, and the successive states of alarm.

-07 - **Modelo y dataframe por poligonos**:Second model that takes as data base the restricted one to the location of the 4 important industrial parks of Zaragoza. This way risks are estimated and compared between industrial parks.

#### Experimental tests

-10 - **Modelización alternativa**: Alternative modeling.

-11 - **Pruebas**: Tests.
