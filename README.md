# SEISMIC
Shared code and standards for participants in the SEISMIC collaboration. Code is primarily in R, and will make regular use of `tidyverse` and packages therein. Also included are SQL scripts we run locally at Michigan, which contain much of the filtering and primary sample definitions.

## Use
1. `STEM_courses_UM.tab`: A list of STEM courses at the University of Michigan.

2. `LARC.sql`: An example SQL query to pull the columns necessary for these studies. It creates the `students.tsv` table needed.

3. `example.Rmd`: An RMarkdown document that wraps the two R routines that create plots, regressions.

4. `course_performance_setup.R`: Loads libraries, reads in data.

5. `course_performance.R`: Creates plots,regressions, which can be (but are not required to be) wrapped in markdown.

6. `basic_compute_pairwise.R`: Takes CLEANED student course and student record tables (max is about 6000 students) and outputs a two elements list: a similarity matrix (it isn't a matrix strictly speaking), and clusters students by similarity.

## Output
`test.pdf`: Plots of 'grade penalties' and regression tables that include coefficients for URM, gender, and first gen. These are a straw man and a launching point.
