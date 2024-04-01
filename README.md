A project that attempts to answer the question: “What are the key factors that influence critic’s ratings of movies on RottenTomatoes? 

**Initial Hypothesis** 

1. Movies with a longer overall runtime tend to have a higher critics rating, likely due to more develop plots and characters
2. Movies released during the summer and winter blockbuster seasons will exhibit higher critics rating, and this trend will vary depending on the year
3. Movies starring a more developed roster of popular actors will have higher critics ratings
4. Critics ratings will generally be in line with audience ratings, but will likely be lower due to professional experience

**The Code Base is Divided Into the Following Sections** 
1. Data cleaning and preperation ([Dataset]([url](https://www.kaggle.com/datasets/stefanoleone992/rotten-tomatoes-movies-and-critic-reviews-dataset?select=rotten_tomatoes_movies.csv)
2. Establish the variables the have the highest impact on critics rating measured by AIC via OLS regression
3. Classify high critics rating as those that fall above the median and measure those same variables in a LOGIT regression model
4. Utilize stepwise regression to attempt to develop a model with higher explanatory power
5. Use the trained model to predict on the test split (80/20) split (78% accuracy)
6. Develop a CART model to predict on the test split (80/20) split. Shows that high-low conditions split on audience ratings (F1 - 0.78)
7. Develop a CART model to test on a 70/30 split. Model remains stable (F1 - 0.79) but does observe a slightly higher recall ability
8. Validate CART models using cross-validation

![image](https://github.com/Clanda2/BUS462Final/assets/149719672/d7e2da4c-0400-4840-9e08-4bca58cbec1e)
![image](https://github.com/Clanda2/BUS462Final/assets/149719672/bd9b8cfa-1dce-4294-bdca-d672c0aabdc0)
![image](https://github.com/Clanda2/BUS462Final/assets/149719672/b62cac45-aaa0-4c61-87ec-11d25212c165)


**Findings**
1. 
