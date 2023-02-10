
### Getting Started

- Select the start year and end year for your analyses
- Select the number of topics for the topic model (you can think of topics like "themes")
- Select the journals that you would like to include in the topic model
- Click Run Topic Model. The app will navigate to the "Topic Model" tab and display the results

### Topic Model Tab

This is the main tab for interpreting the topic model results. For each topic, the top words for the topic are plotted. These top words describe what each topic is "about."

When you set the number of topics slider in the sidebar, that then restricts the model output.
It  sets the number of "themes" to that number.
Once the model is fit, each topic represents a separate theme in the abstracts.
Changing the number of topics will change the model output.
Including more topics will typically lead to more fine-grained, separate topics.
For each topic multiple pieces of information are conveyed:

- The topic plot header indicates the proportion of the dataset that is "about" this topic.
- The words listed in each topic's plot are the most probable words for that topic and essentially "define" the topic.
- The bar chart values represent the conditional probability of the word, given the topic.

The question that drives analysis on this tab is "What is this dataset about, assuming there are N distinct themes?" where N is the number of topics you selected at the beginning of the analysis.

#### Analysis by Methodology

When running a topic model using data from the UTD journals, we also include an analysis plot for the hand-coded methodologies. 
For each method in our categorization (listed on the y-axis), we show the average topic proportions for articles coded with that method.

#### Top Scoring Articles per Topic

Below the topic model visualization, we include a table of the "top" articles for each topic. 
Each article in the input dataset can be scored by a learned topic model.
This score represents what percentage of the article's abstract is associated with each topic.
We display the articles with the highest percentage for each topic as a way to show the most relevant articles for a particular topic.

### Trend Analysis Tab

This tab shows how the spread between the topics changes year-over-year. 

The first plot shows the distribution of the topics over the selected abstracts year-to-year.
This represents how much of each year's abstract text relates to the particular theme.
The numeric value at the top of the plot is the KL-divergence of the topics from a uniform distribution, which quantifies how far from balanced the particular year is with respect to the topics. 

The second plot shows how many papers from each of the possible journals are included each year.

Both plots are interactive.
If you want to isolate a particular topic or journal, you can double click on the legend to do so.
If you want to exclude a journal or topic, simply click it once.

### Abstract Scores Tab

Each abstract is a mixture of the topics. 
This tab lets you see the abstracts that score highest for a particular topic or topics. 
By selecting one or more topics from the dropdown list you can view the percentage of each abstract that is related to the particular topic.
Each column in the table is searchable and sortable.

### Network Analysis Tab

<!--Each article in our data set was tagged with human-selected keywords.-->
This tab provides a title keyword network analysis, so you can see which keywords are connected across paper titles. 
The graph is interactive. 
If you click a node, all papers with that keyword are displayed below. 
If you click an edge, all papers with both keywords are displayed below. 
Zoom in/out with your scroll wheel.

