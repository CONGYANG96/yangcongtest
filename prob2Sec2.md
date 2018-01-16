Prob \#2
================
Cong Yang
Janurary 16th, 2017

First, load all the required libraries

``` r
library(rmarkdown)
library(sarsop)
library(MDPtoolbox)
```

    ## Loading required package: Matrix

    ## Loading required package: linprog

    ## Loading required package: lpSolve

Second, set the basic model parameter. Now we only have 100 prossible states since we change the observation martrix to 100X100X100. Therefore, I do not use the same data as the paper. Instead, I set the maximum wolf population size to 99 and minimun wolf population to 0, so that we have 100 states in total.

``` r
Nmax<-99
Nmin<-0
K=Nmax
lambda <- 1.25
beta <- 0.98
epsilon <- 1*10^-10
discount <- 0.9
```

Third, the control variable At is the harvest rate Ht, a discrete variable ranging from 0% to 100% with an increment of 1/(K + 1) therefore allowing as many possible actions as there are number of states. We are given that the probability of taking a successful action is p=0.9; therefore, the probability of the action not working (failed action) is (1-p)=0.9. Also, becasue I change the number of state, I re-define the alpha function ('alpha = 1' if '50 &lt;= Nt+1 &lt;= 99', otherwise, 'alpha = 0').

``` r
H = matrix(0,nrow=1,ncol=1)
for (i in 1:K){
  H[i+1]=1/(K+1)+H[i]
}

AlphaFunction  <- function(Ntplus){
  if (Ntplus<50 | Ntplus>99)
  {
    res <- 0
  }
  else{
    res <- 1
  }
}

NtFunction <- function (Nt,Ht,lambda)
{
  res <- lambda * Nt * (1 - Ht)
}

UtilityFunction <- function (Nt,Ht,alpha)
{
  res <- Nt * (1 - Ht) * alpha
}

utility <- matrix(0, nrow = K+1, ncol = length(H)) 
states=0:K
transition <- array(0, dim = c(K+1, K+1, length(H)))
observation<-transition
Ntplus_storage<- matrix(0, nrow = K+1, ncol = length(H)) 
for (k in 0:K) {
  
  for (i in 1:length(H)) {
    
    Ntplus <- NtFunction(k, H[i],lambda)
    
    Alpha <- AlphaFunction(Ntplus)
    
    Ntplus <- min(round(Ntplus), K)
    
    utility[k+1,i] <- UtilityFunction(Ntplus,H[i],Alpha)
    
    index <- which(states == Ntplus)
    
    transition[k+1,index,i] <- 1
    
    observation[k+1,index,i] <-0.9
    
    observation[k+1,k+1,i]<-observation[k+1,k+1,i] + 0.1
  }
}
```

Fourth, use the built-in function to find the optimal solution (if converge then the while loop stop)

``` r
converge <- 0
preci <- 1
tic <- 0
while(converge == 0)
{
  if (tic == 0)
  {
    result <- sarsop(transition, observation, utility, discount, precision = preci)
    policy_previous <- compute_policy(result, transition, observation, utility)
    tic = tic + 1
  }
  else
  {
    preci <- preci/10
    result <- sarsop(transition, observation, utility, discount, precision = preci)
    policy_current <- compute_policy(result, transition, observation, utility)
    if (mean(abs(policy_current$policy-policy_previous$policy)) <= 1/K)
    {
      converge <-1
      policy <- policy_current
    }
    else
    {
      policy_previous <- policy_current
    }
  }
}
```

    ## load time: 6.29 sec, init time: 0.56 sec, run time: 0.82 sec, final precision: 0.99893 end_condition:   target precision reached

    ## load time: 6.41 sec, init time: 0.57 sec, run time: 1.09 sec, final precision: 0.0950641 end_condition:   target precision reached

    ## load time: 6.7 sec, init time: 0.64 sec, run time: 1.4 sec, final precision: 0.00919043 end_condition:   target precision reached

    ## load time: 6.34 sec, init time: 0.61 sec, run time: 1.57 sec, final precision: 0.000841848 end_condition:   target precision reached

Fifth, plot the result

``` r
policy_val <- H[policy[,1,]]
print(policy)
```

    ##     policy    value state
    ## 1        1   0.0000     1
    ## 2        1   0.0000     2
    ## 3        1   0.0000     3
    ## 4        1   0.0000     4
    ## 5        1 245.9910     5
    ## 6        1 248.7242     6
    ## 7        1 276.3600     7
    ## 8        1   0.0000     8
    ## 9        1 307.0673     9
    ## 10       1 369.6220    10
    ## 11       1 341.1853    11
    ## 12       1 373.7289    12
    ## 13       1 379.0949    13
    ## 14       1   0.0000    14
    ## 15       1 415.2542    15
    ## 16       1 421.2167    16
    ## 17       1 464.0500    17
    ## 18       1   0.0000    18
    ## 19       1 461.3933    19
    ## 20       1 468.0188    20
    ## 21       1 469.2061    21
    ## 22       1 552.5210    22
    ## 23       1 512.6596    23
    ## 24       1   0.0000    24
    ## 25       1 520.0275    25
    ## 26       1 521.3400    26
    ## 27       1 558.6602    27
    ## 28       1   0.0000    28
    ## 29       1 569.6216    29
    ## 30       1 627.6400    30
    ## 31       1 577.8077    31
    ## 32       1 579.2673    32
    ## 33       1 620.7342    33
    ## 34       1   0.0000    34
    ## 35       1 689.1590    35
    ## 36       1 632.9121    36
    ## 37       1 634.6128    37
    ## 38       1   0.0000    38
    ## 39       1 642.0081    39
    ## 40       1 643.6361    40
    ## 41       1 689.7037    41
    ## 42       1 764.4800    42
    ## 43       1 696.8016    43
    ## 44       1   0.0000    44
    ## 45       1 703.2260    45
    ## 46       1 705.1159    46
    ## 47       1 778.8420    47
    ## 48       1   0.0000    48
    ## 49       1 713.3327    49
    ## 50       1 715.1426    50
    ## 51       1 760.7770    51
    ## 52       1 767.1705    52
    ## 53       1 768.2986    53
    ## 54       1   0.0000    54
    ## 55       1 796.4450    55
    ## 56       1 775.1329    56
    ## 57       1 777.1225    57
    ## 58       1   0.0000    58
    ## 59       1 780.9230    59
    ## 60       1 803.1310    60
    ## 61       1 785.8132    61
    ## 62       1 787.7130    62
    ## 63       1 788.3609    63
    ## 64       1   0.0000    64
    ## 65       1 793.7981    65
    ## 66       2 793.7973    66
    ## 67       4 807.3290    67
    ## 68       1   0.0000    68
    ## 69       1 796.2686    69
    ## 70       1 797.2767    70
    ## 71       1 798.3927    71
    ## 72       2 808.8300    72
    ## 73       1 800.4087    73
    ## 74       1   0.0000    74
    ## 75       1 802.4247    75
    ## 76       1 803.5416    76
    ## 77       1 804.5496    77
    ## 78       1   0.0000    78
    ## 79       2 804.9855    79
    ## 80       1 811.8000    80
    ## 81       2 809.0280    81
    ## 82       4 807.0180    82
    ## 83       5 806.2090    83
    ## 84       1   0.0000    84
    ## 85       7 805.8600    85
    ## 86       8 804.1680    86
    ## 87       9 803.3580    87
    ## 88       1   0.0000    88
    ## 89      11 801.7380    89
    ## 90      13 795.7617    90
    ## 91      13 800.1180    91
    ## 92      14 798.9300    92
    ## 93      15 798.4701    93
    ## 94       1   0.0000    94
    ## 95       1 797.4027    95
    ## 96       1 798.1560    96
    ## 97      22 790.5230    97
    ## 98       1   0.0000    98
    ## 99       1 798.0591    99
    ## 100     21 791.6464   100

``` r
plot(states, policy_val, xlab="Population size", ylab="harvest rate")
```

![](prob2Sec2_files/figure-markdown_github/Plot-1.png)
Sixth, original solution without measurement uncertainty (using MDPToolbox) to the same figure for comparison purposes.

``` r
pol_ite<-mdp_policy_iteration_modified(transition, utility, discount, epsilon)
```

    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped by maximum number of iteration condition 10"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"
    ## [1] "MDP Toolbox: iterations stopped, epsilon-optimal value function"

``` r
policy_pol<-matrix(0,nrow=K+1,ncol=1)
policy_pol<-H[pol_ite$policy]
plot(states, policy_pol, xlab="Population size", ylab="harvest rate (Pol_Ite)")
```

![](prob2Sec2_files/figure-markdown_github/original-1.png)
