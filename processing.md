I want to cut out the variables that aren't adding value to the dataset. Anything from the department or university isn't really related to the class, so we shouldn't be storing them by class. It takes way more memory (which isn't really a problem with just CS classes after 2015, but I want to be able to use this same process to handle every class), and its harder to interpret. To do that, we need to split `Course.Name` into a `Name` and a `Semester`, so that we can perform analysis based on time.

``` r
splitNames <- csEvals$Course.Name %>%
  str_split(" \\(|\\)") 

csEvals$Course.Name <- splitNames %>% 
  sapply(function(x) extract2(x, 1))

csEvals$Semester <- splitNames %>% 
  sapply(function(x) extract2(x, 2))
```

Now let's pull out all the department and university data into a seperate dataframe, and remove it from `csEvals`.

``` r
firstOfEachTime <- !duplicated(csEvals$Semester)
aggregateStats <- csEvals[firstOfEachTime,] %>%
  select(matches("Department|University"))

# 61 variables? That's weird, it should be divisible by two,
# because each variable for department should have a corresponding
# variable for the university. 

colnames(aggregateStats)
```

    ##  [1] "Department"                                        
    ##  [2] "Course.Related.Questions.Department"               
    ##  [3] "Course.Related.Questions.University"               
    ##  [4] "Learning.Related.Questions.Department"             
    ##  [5] "Learning.Related.Questions.University"             
    ##  [6] "Instructor.Related.Questions.Department"           
    ##  [7] "Instructor.Related.Questions.University"           
    ##  [8] "Instructor.Effectiveness.Department"               
    ##  [9] "Instructor.Effectiveness.University"               
    ## [10] "Syllabus.Helped.Department"                        
    ## [11] "Syllabus.Helped.University"                        
    ## [12] "Textbook.Helped.Department"                        
    ## [13] "Textbook.Helped.University"                        
    ## [14] "Online.Helped.Department"                          
    ## [15] "Online.Helped.University"                          
    ## [16] "Assignments.Helped.Department"                     
    ## [17] "Assignments.Helped.University"                     
    ## [18] "Lectures.Helped.Department"                        
    ## [19] "Lectures.Helped.University"                        
    ## [20] "Inclass.Helped.Department"                         
    ## [21] "Inclass.Helped.University"                         
    ## [22] "Classroom.Technology.Helped.Department"            
    ## [23] "Classroom.Technology.Helped.University"            
    ## [24] "Course.Was.Intellectual.Department"                
    ## [25] "Course.Was.Intellectual.University"                
    ## [26] "Learned.a.Lot.Department"                          
    ## [27] "Learned.a.Lot.University"                          
    ## [28] "Learned.Concepts.And.Principles.Department"        
    ## [29] "Learned.Concepts.And.Principles.University"        
    ## [30] "Developed.Skills.Expressing.Department"            
    ## [31] "Developed.Skills.Expressing.University"            
    ## [32] "Learned.Analyze.and.Evaluate.Department"           
    ## [33] "Learned.Analyze.and.Evaluate.University"           
    ## [34] "Instructor.Communication.Skills.Department"        
    ## [35] "Instructor.Communication.Skills.University"        
    ## [36] "Instructor.Communicated.Department"                
    ## [37] "Instructor.Communicated.University"                
    ## [38] "Instructor.Stated.Objectives.Department"           
    ## [39] "Instructor.Stated.Objectives.University"           
    ## [40] "Instructor.Covered.Stated.Department"              
    ## [41] "Instructor.Covered.Stated.University"              
    ## [42] "Instructor.Prepared.Department"                    
    ## [43] "Instructor.Prepared.University"                    
    ## [44] "Instructor.Used.Class.Time.Well.Department"        
    ## [45] "Instructor.Used.Class.Time.Well.University"        
    ## [46] "Instructor.Provided.Feedback.Department"           
    ## [47] "Instructor.Provided.Feedback.University"           
    ## [48] "Instructor.Fairly.Evaluated.Performance.Department"
    ## [49] "Instructor.Fairly.Evaluated.Performance.University"
    ## [50] "Would.Recommend.Department"                        
    ## [51] "Would.Recommend.University"                        
    ## [52] "Respected.Students.Department"                     
    ## [53] "Respected.Students.University"                     
    ## [54] "Instructor.Took.Effective.Action.Department"       
    ## [55] "Instructor.Took.Effective.Action.University"       
    ## [56] "Instructor.Available.Outside.of.Class.Department"  
    ## [57] "Instructor.Available.Outside.of.Class.University"  
    ## [58] "Instructor.Displayed.Enthusiasm.Department"        
    ## [59] "Instructor.Displayed.Enthusiasm.University"        
    ## [60] "Instructor.Overall.Department"                     
    ## [61] "Instructor.Overall.University"

``` r
# "Department" is the odd one out. It's probably a good idea
# to leave it in here: knowing the department average isn't much use without 
# knowing the department!

csEvals <- csEvals %>%
  select(-matches(".(Department|University)"))

# The regexes are slightly different because I want to
# match "Department" in the first but not the second
```

Much more manageable. Time to define some utility functions on TRACE evaluations.

``` r
avgTime <- function(eval) {
  ifelse(!is.na(eval$Spent.1.4.Hours),
     eval$Spent.1.4.Hours * (1 + 4) / 2,
     0) %>%
  ifelse(!is.na(eval$Spent.5.8.Hours),
     . + eval$Spent.5.8.Hours * (5 + 8) / 2,
     .) %>%
  ifelse(!is.na(eval$Spent.9.12.Hours),
     . + eval$Spent.9.12.Hours * (9 + 12) / 2,
     .) %>%
  ifelse(!is.na(eval$Spent.13.16.Hours),
     . + eval$Spent.13.16.Hours * (13 + 16) / 2,
     .) %>%
  ifelse(!is.na(eval$Spent.17.20.Hours),
     . + eval$Spent.17.20.Hours * (17 + 20) / 2,
     .) %>%
    `/`(100)
}
```

More processing of the data:

``` r
# Add avgTime to each 
csEvals <- csEvals %>%
  mutate(AvgTime = avgTime(.))
  
csEvals %>% group_by(Semester) %>%   summarize(avg=mean(AvgTime), na.rm=TRUE)
```

    ## # A tibble: 10 x 3
    ##                        Semester       avg na.rm
    ##                           <chr>     <dbl> <lgl>
    ##  1     201550: Full Summer 2015 13.200556  TRUE
    ##  2 201560: Summer 2 2015 Survey  8.515000  TRUE
    ##  3            201610: Fall 2015 10.010141  TRUE
    ##  4          201630: Spring 2016 11.205469  TRUE
    ##  5        201640: Summer 1 2016  9.844167  TRUE
    ##  6     201650: Full Summer 2016 12.980000  TRUE
    ##  7        201660: Summer 2 2016 11.303000  TRUE
    ##  8            201710: Fall 2016 10.251087  TRUE
    ##  9          201730: Spring 2017 10.411746  TRUE
    ## 10              Spring A 201730  7.192500  TRUE
