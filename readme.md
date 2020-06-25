Data and analysis code for 

Shi, Z., Allenmark, F., Zhu, X., Elliott, M. A., & Müller, H. J. (2019). To quit or not to quit in dynamic search. Attention, Perception & Psychophysics. https://doi.org/10.3758/s13414-019-01857-7


# Experiments

## Experiment 1

This is a replication of the study (Horowitz & Wolfe, 1998) with some modification. In this experiment, a search display was either static or dynamic (i.e., the location of the items are reshuffled every 110 ms). The display was presented until a response or 5 seconds maximum. In addition, the set size of the display varied with 8, 12, or 16 items, and target presence vs. absence 1:1. The static and dynamic search displays were presented in separated block, counter-balanced. And participants were asked for detecting the target (Absence vs. Presence)

__Summary of the manipulated factors__

1. Target: Presence vs. Absence 
2. Set size: 8, 12, 16
3. Display Type: Static vs. Dynamic (Block-wise)

__Data Information__

1. Each condition was repeated 35 times
2. Total trials: 2 x 3 x 2 x 35 = 420 
3. 11 Participants

__Experimental program__: 
1.Exp1_dynSearch.m

## Experiment 2

The experimental paradigm was the same as in Experiment 1, with the following exceptions. We conducted two separate sessions. In one session, we rewared target-absent trials whenever the participant made a correct response of 'target absent', while leaving the target-present trials no reward. After each corrected response in the reward trial, a feedback was provided and told participants that they earned 1 more cent in this trial. In another session, we rewarded the target-presence trials. 

__Summary of the manipulated factors__

1. Target: Presence vs. Absence 
2. Set size: 8, 12, 16
3. Display Type: Static vs. Dynamic (Block-wise)
4. Reward: Target Presence, Absence (Session-wise)

__Data Information__

1. Each condition was repeated 35 times
2. Total trials: 2 x 3 x 2 x 35 x 2 = 840 
3. 12 Participants

__Experimental program__: 

1. Exp2_dynSearch1.m (reward absence)
2. Exp2_dynSearch2.m (reward presence)

## Experiment 3

Experiment 3 was essentially the same as Experiment 1, except that the vertical line of the distractor ‘L’ was shifted inward by a small amount (0.095deg), making it more similar to the target ‘T’ thus increasing the difficulty of the search task. In addition, the total number of trials was increased to 576. In addition, we control the display density in such a way that all displays had the same density. In addition, we counter-balanced the display location for the small and large set size by randomizing the location of the display. 

__Summary of the manipulated factors__

1. Target: Presence vs. Absence 
2. Set size: 8, 12, 16
3. Display Type: Static vs. Dynamic (Block-wise)
4. Display location: 4 quadrant (as randomization factor)

__Data Information__

1. Each condition was repeated 48 times
2. Total trials: 2 x 3 x 2 x 48 = 576 
3. 12 Participants

__Experimental program__: 

1. Exp3_dynSearch.m


## Data

Raw data are stored in the sub-folder `data`: exp1.csv, exp2.csv, and exp3.csv. 

Mean error rates and mean RTs are stored in expN_err.csv and expN_mrt.csv. 


