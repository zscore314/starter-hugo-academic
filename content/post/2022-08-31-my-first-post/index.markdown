---
title: Football, Pythagoras and Bad Statistical Communication
author: Zach Eisenstein
date: '2022-08-31'
slug: my-first-post
categories: []
tags: 
  - bayesian
  - brms
  - R
  - tidyverse
subtitle: ''
summary: ''
authors: []
lastmod: '2022-08-31T21:27:09-04:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

I recently picked up the book [Mathletics](https://www.amazon.com/Mathletics-Gamblers-Managers-Mathematics-Sports-dp-0691177627/dp/0691177627/) "How gamblers, managers, and fans use mathematics in sports." by Wayne Winston after hearing a recommendation in an online R meetup. I like math, i like sports, what could go wrong. 

The first chapter "Baseball's Pythagorean Theorem" describes a mathematical relationship attributed to Bill James. The idea is to estimate the percentage of games won in baseball based solely on the offensive and defensive production (runs scored, and runs allowed respectively).

> Bill James studied many years of Major League Baseball standing and found the percentage of games won by a baseball team can be well approximated by the formula

{{< math >}}$$`
\text{% games won} = \frac{\text{runs scored}^2} {\text{runs scored}^2 + \text{runs allowed }^2}
`$${{< /math >}}

My first thought was this doesn't look very "pythagorean" at all but I continued. Later, the right hand side of the equation was divided through by {{< math >}}$\text{runs scored}^2${{< /math >}} and by letting {{< math >}}$R = \frac{\text{runs scored}} {\text{runs allowed}}${{< /math >}} you get the new expression:

{{< math >}}$$`
\text{% games won} = \frac{R^2} {R^2 + 1}
`$${{< /math >}}

Now this **really** isn't looking pythagorean!

The chapter continues touting the accuracy of the formula:

> For example, the 2016 LA Dodgers scored 725 runs and gave up 638 runs. Their scoring ratio was 1.136, with a predicted win percentage of .5636 and they actually won .5618.


but to the authors' credit, they attempted to replicate Bill James expression using data from the 2005-2016 seasons (and excel data table, tsk tsk). What they did was generalize the equation above to allow the exponent to vary ie {{< math >}}$\text{% games won} = \frac{R^e} {R^e + 1}${{< /math >}}. Ultimately excel found that an exponent of 1.8 minimized the error and that was relatively close to **the original** pythagorean theorem.

To this point, I was not too bothered (except for the use of excel). I was only mildly concerned about the lack of communication of uncertainty in the estimate. But where things started to not sit right with me was in the estimation of e for other sports. 

> Daryl Morey, currently the GM for the Houston Rockets NBA team, has shown that for the NFL, e = 2.37 gives the most accurate prediction while for the NBA e = 13.91. 

While it's not entirely clear, the estimate of 2.37 appears to have been generated from a *single* NFL season, 2015.  
This is where I started to have issues:

In no particular order:

- How uncertain is the estimate of this magic exponent?
- Why is it we're assuming this particular functional form? Is there something special about the non-linear equation?
- Could you perhaps get better predictions using points for and points against separately?
- How good are predictions based on random chance alone? In the NFL, there are only 16 games (now 17) in a season. So wins are clustered around 8. 
- Rather than the focus being on the correct estimate, perhaps we should instead quantify the reduction in uncertainty based on the prediction?
- Predicting wins based on points scored seems a bit like prediciting foot length by shoe size. It's less a predictor and more intimately tied to the outcome itself.
- Why is the basketball GM opining on football analytics (half kidding)

I set off to investigate some of the above. 


```r
library(tidyverse)
library(nflverse)
theme_set(theme_light())
```

Lets start by looking at the shape of this "pythagorean theorem"


```r
tibble(R = seq(0.5, 1.5, 0.1)) %>%
    crossing(tibble(e = c(2, 1.8, 2.37, 13.91),
                    label = c('MLB (2)', 'MLB (1.8)', 'NFL (2.37)', 'NBA (13.91)'))) %>%
    mutate(pred_win = R^e / (R^e + 1)) %>%
    ggplot(aes(R, pred_win, color = label)) +
        geom_line()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-1.png" width="672" />
It seems scoring ratio is strongly associated with win probability in the NBA, not so much in the other sports. 

Now I'll use NFL data utilizing the `load_schedules` function from `nflreadrr` to explore further. 

Start by getting wins and losses by team.


```r
games <- nflreadr::load_schedules(1999:2021)

home <- games %>%
  filter(game_type == 'REG') %>%
  transmute(
    season = season,
    team = home_team,
    week = week,
    points_for = home_score,
    points_against = away_score
  )

away <- games %>%
  filter(game_type == 'REG') %>%
  transmute(
    season = season,
    team = away_team,
    week = week,
    points_for = away_score,
    points_against = home_score
  )

results <- bind_rows(home, away) %>%
  arrange(week) %>%
  mutate(win = case_when(
    points_for > points_against ~ 1,
    points_for < points_against ~ 0,
    TRUE ~ 0.5
  ))

results <- results %>%
  group_by(season, team) %>%
  summarise(
    w = sum(win == 1),
    l = sum(win == 0),
    t = sum(win == 0.5),
    total_games = n(),
    win_pct = sum(win) / n(),
    pf = sum(points_for),
    pa = sum(points_against),
    pd = pf - pa
    
  ) %>%
  ungroup() %>%
  mutate(r = pf / pa)
```

```
## `summarise()` has grouped output by 'season'. You can override using the
## `.groups` argument.
```

```r
head(results)
```

```
## # A tibble: 6 × 11
##   season team      w     l     t total_games win_pct    pf    pa    pd     r
##    <int> <chr> <int> <int> <int>       <int>   <dbl> <int> <int> <int> <dbl>
## 1   1999 ARI       6    10     0          16   0.375   245   382  -137 0.641
## 2   1999 ATL       5    11     0          16   0.312   285   380   -95 0.75 
## 3   1999 BAL       8     8     0          16   0.5     324   277    47 1.17 
## 4   1999 BUF      11     5     0          16   0.688   320   229    91 1.40 
## 5   1999 CAR       8     8     0          16   0.5     421   381    40 1.10 
## 6   1999 CHI       6    10     0          16   0.375   272   341   -69 0.798
```

The results table contains all team results from the 1999 through 2021 seasons

Let's take a look at the results of the 2015 season


```r
results %>%
  filter(season == "2015") %>%
  ggplot(aes(r, win_pct)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.075, alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Scoring Ratio, R",
       y = "Win Percentage",
       title = "2015 NFL Season")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />
## Logistic Regression


```r
results %>%
  filter(season == "2015") %>%
  glm(win_pct ~ r, family = binomial, weights=total_games, data = .)
```

```
## 
## Call:  glm(formula = win_pct ~ r, family = binomial, data = ., weights = total_games)
## 
## Coefficients:
## (Intercept)            r  
##      -2.757        2.684  
## 
## Degrees of Freedom: 31 Total (i.e. Null);  30 Residual
## Null Deviance:	    77.21 
## Residual Deviance: 12.46 	AIC: 115.2
```


Let's take a look at my hometown team, the Chicago Bears


```r
results %>%
  filter(team == "CHI") %>%
  select(season, win_pct, r) %>%
  ggplot(aes(r, win_pct)) +
  geom_point()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />



