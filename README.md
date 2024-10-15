# antidepressant_use_in_spatial_social_networks
This repository contains the code and data to reproduce results of Lengyel et al. "Antidepressant use in spatial social networks". The published data, however, do not include personal information that would allow identification of individuals. The original iWiW dataset is protected with an NDA but can be accessed after authorization at the Data Bank of the Centre for Economic and Regional Studies. Further information about this can be requested from the corresponding author at lengyel.balazs@krtk.hun-ren.hu. The original antidepressant data cannot be accessed since its’ use has been restricted to the members of the research team.

I. Town-level tables

1. population.csv: contains town identifier, town name, and population
2. usage_rate_jaras.dta: contains town identifier and the rate of antidepressant users in the subregion of the town
3. usage_stats2012_all_settlements.dta: town identifier and town-level information on antidepressant usage
4. iWiW_town_edges: aggregate number of friendship connections between towns
5. iWiW_town_nodes: town identifier, number of iWiW users, population, GPS coordinates

II. iWiW shape file
These files can be used to generate the thematic map in Figure 1B.

III. Individual table

6. id_master_v3: contains the matched sample. Individual information includes quantity of antidepressant usage, node-level network measures, town id, gender. We leave out the age of individuals that makes it impossible to identify individual users. 

IV. Code to reproduce results in the main paper
7. Figure 1_2.R
All code that produces components of Figure 1B, and Figure 2B,C,D.

8. Figure 3.R
Code contains regression models run on individual data and visualization of coefficients and marginal effects. This original code contains reference to the age variable that is left out from the individual table.

9. Table 1.R
Code contains the generation of the IV variable, regression models run on individual data. This original code contains reference to the age variable that is left out from the individual table.
