---
title: When Diversification Increases Risk
author: Zach Eisenstein
date: '2022-09-03'
draft: false
slug: when-diversification-increases-risk
categories: []
tags:
  - risk
  - ERM
  - fat tails
  - R
  - tidyverse
subtitle: ''
summary: ''
authors: []
lastmod: '2022-09-03T07:29:17-04:00'
featured: no
image:
  caption: 'Image credit: Zach Eisenstein'
  focal_point: ''
  preview_only: true
projects: []
commentable: true
---

<link href="{{< blogdown/postref >}}index_files/tabwid/tabwid.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index_files/tabwid/scrool.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index_files/tabwid/tabwid.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index_files/tabwid/scrool.css" rel="stylesheet" />

## Intro

Harry Markowitz of [Modern Portfolio Theory](https://en.wikipedia.org/wiki/Modern_portfolio_theory) fame said that “Diversification is the only free lunch in investing” as it allows investors to reduce volatility without eroding returns. Diversification, through pooling of independent insured exposures, is also fundamental to risk management in insurance. **But what if risk pooling actually *increased* risk?**

The great Paul Embrechts and co-authors recently published an article entitled [An unexpected stochastic dominance: Pareto distributions catastrophes, and risk exchange](https://arxiv.org/pdf/2208.08471.pdf). In this brief post I will explore the paper’s unintuitive findings using simulation.

## A brief review of the pareto distribution

The [pareto distribution](https://en.wikipedia.org/wiki/Pareto_distribution) is a power-law distribution that was originally used to measure wealth inequality and underlies the familiar [80-20 rule](https://en.wikipedia.org/wiki/Pareto_principle). In its most basic form, it contains two variables, a shape variable $\alpha$, and a scale $\theta$ (minimum value, typically known) with probability density $f(x) = \frac{\alpha \theta^\alpha}{(x)^{\alpha + 1}}$, CDF $F(x) = 1 - (\frac{\theta}{x})^{\alpha}$ and mean $E[X] = \theta \frac{\alpha}{\alpha - 1}$.

The shape $\alpha$, (“tail exponent”) controls the degree to which rare, extreme events disproportionately control the shape of the distribution (e.g.the mean).

Below you can see the effect of $\alpha$ on the tail of the distribution.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-1.png" width="672" />

## The unexpected stochastic dominance

Let’s first introduce the concept of stochastic dominance.

> For two random variable X and Y representing random losses, we say X is smaller than Y in first-order stochastic dominance, denoted by $X \le_{st} Y$, if $P(X \le x) \ge P(Y \le x) for x \in \mathbb{R}$

This implies that X is the preferred risk for decision makers as globally, for a given loss amount x, it has a lower probability of exceeding x than Y.

The key result in the paper is Theorem 1.

> For iid random variables $X_1, ..., X_n$ following a Pareto distribution with infinite mean and weights $\theta_1, ..., \theta_n$ with $\sum_{i = 1}^{n} \theta_i = 1$, our main finding in Theorem 1 is the stochastic dominance relation $$X_1 \le_{st} \theta_1 X_1 + ... + \theta_n X_n$$

In plain english, for a pareto distributed variable with infinite mean (i.e.$\alpha < 1$), the quantiles (or Value at Risk–VaR) of a single random variable will be **less than or equal to** that of a “diversified” portfolio of identical risks!

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

While VaR has been known to fail sub-additivity, that has been confined to some contrived cases and for select percentiles. The amazing result here is that this holds across the full distribution. You’re better off putting all your eggs in one basket!

## A simulation approach

I’ll explore the finding above using simulation and visualization. I’ll utilize the `*pareto` functions from the `EnvStats` package.

### Finite Mean

First lets look at the case of finite mean, where diversification works. We’ll simulate 10 iid parteo random variables with $\alpha = 3$ and compare the distributions of a standalone portfolio of X1 to a portfolio with 10 equal shares of X1:X10 $\frac{1}{n}\sum_{i = 1}^{n} X_i$.

To simulate I’ll write a function `rpareto_n` that, in addition to the tail factor, takes the number of risks `n` as an input and returns the average for each trial.

``` r
rpareto_n <- function(n = 1, alpha = 3, scale = 1, trials = 10e3){

  # Simulate n iid pareto variables
  sim <- matrix(rpareto(n * trials, shape = alpha, location = scale),
             nrow = trials)

  # equal proportions
  b <- rowSums(sim / n)

  return(b)

}
```

…and simulate.

``` r
sim_n1 <- rpareto_n(1, 3)
sim_n10 <- rpareto_n(10, 3)
```

Both distributions have the same mean (except for simulation error). But the diversified risk is the preferred one for the tail region (upper quartile). This is consistent with our intuition of the benefits of diversification.

<template id="36f47a3c-133a-4f68-8860-8ed1bab5ba89"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  margin-top: 1.275em;
  margin-bottom: 1.275em;
  border-color: transparent;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
</style><div class="tabwid"><style>.cl-becb2bf0{}.cl-beb21b10{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-beb23596{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-beb235a0{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-beb28618{width:54.2pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28622{width:51.4pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28623{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb2862c{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb2862d{width:54.2pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28636{width:51.4pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28637{width:51.4pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28638{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28639{width:54.2pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28640{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28641{width:54.2pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28642{width:51.4pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb28643{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb2864a{width:54.2pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-beb2864b{width:51.4pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-becb2bf0'>
<caption class="Table Caption">

(#tab:tab1)Alpha = 3

</caption>
<thead><tr style="overflow-wrap:break-word;"><td class="cl-beb28643"><p class="cl-beb23596"><span class="cl-beb21b10">Metric</span></p></td><td class="cl-beb2864a"><p class="cl-beb235a0"><span class="cl-beb21b10">n = 1</span></p></td><td class="cl-beb2864b"><p class="cl-beb235a0"><span class="cl-beb21b10">n = 10</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-beb28623"><p class="cl-beb23596"><span class="cl-beb21b10">Mean</span></p></td><td class="cl-beb28618"><p class="cl-beb235a0"><span class="cl-beb21b10">1.500</span></p></td><td class="cl-beb28622"><p class="cl-beb235a0"><span class="cl-beb21b10">1.498</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-beb28638"><p class="cl-beb23596"><span class="cl-beb21b10">SD</span></p></td><td class="cl-beb28639"><p class="cl-beb235a0"><span class="cl-beb21b10">0.866</span></p></td><td class="cl-beb28637"><p class="cl-beb235a0"><span class="cl-beb21b10">0.270</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-beb28640"><p class="cl-beb23596"><span class="cl-beb21b10">0.5</span></p></td><td class="cl-beb28641"><p class="cl-beb235a0"><span class="cl-beb21b10">1.260</span></p></td><td class="cl-beb28642"><p class="cl-beb235a0"><span class="cl-beb21b10">1.443</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-beb28623"><p class="cl-beb23596"><span class="cl-beb21b10">0.75</span></p></td><td class="cl-beb28618"><p class="cl-beb235a0"><span class="cl-beb21b10">1.587</span></p></td><td class="cl-beb28622"><p class="cl-beb235a0"><span class="cl-beb21b10">1.602</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-beb28623"><p class="cl-beb23596"><span class="cl-beb21b10">0.8</span></p></td><td class="cl-beb28618"><p class="cl-beb235a0"><span class="cl-beb21b10">1.710</span></p></td><td class="cl-beb28622"><p class="cl-beb235a0"><span class="cl-beb21b10">1.648</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-beb28640"><p class="cl-beb23596"><span class="cl-beb21b10">0.9</span></p></td><td class="cl-beb28641"><p class="cl-beb235a0"><span class="cl-beb21b10">2.154</span></p></td><td class="cl-beb28642"><p class="cl-beb235a0"><span class="cl-beb21b10">1.793</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-beb28623"><p class="cl-beb23596"><span class="cl-beb21b10">0.95</span></p></td><td class="cl-beb28618"><p class="cl-beb235a0"><span class="cl-beb21b10">2.714</span></p></td><td class="cl-beb28622"><p class="cl-beb235a0"><span class="cl-beb21b10">1.945</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-beb28640"><p class="cl-beb23596"><span class="cl-beb21b10">0.99</span></p></td><td class="cl-beb28641"><p class="cl-beb235a0"><span class="cl-beb21b10">4.642</span></p></td><td class="cl-beb28642"><p class="cl-beb235a0"><span class="cl-beb21b10">2.397</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-beb28623"><p class="cl-beb23596"><span class="cl-beb21b10">0.995</span></p></td><td class="cl-beb28618"><p class="cl-beb235a0"><span class="cl-beb21b10">5.848</span></p></td><td class="cl-beb28622"><p class="cl-beb235a0"><span class="cl-beb21b10">2.644</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-beb2862c"><p class="cl-beb23596"><span class="cl-beb21b10">0.999</span></p></td><td class="cl-beb2862d"><p class="cl-beb235a0"><span class="cl-beb21b10">10.000</span></p></td><td class="cl-beb28636"><p class="cl-beb235a0"><span class="cl-beb21b10">3.336</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="1ac3873d-fc4f-4129-8443-e46a6f7b4de9"></div>
<script>
var dest = document.getElementById("1ac3873d-fc4f-4129-8443-e46a6f7b4de9");
var template = document.getElementById("36f47a3c-133a-4f68-8860-8ed1bab5ba89");
var caption = template.content.querySelector("caption");
if(caption) {
  caption.style.cssText = "display:block;text-align:center;";
  var newcapt = document.createElement("p");
  newcapt.appendChild(caption)
  dest.parentNode.insertBefore(newcapt, dest.previousSibling);
}
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

### Infinite Mean

Now lets look at the same metrics using an infinite mean pareto, with $\alpha = 0.5$

**The standalone risk now outperforms across the whole distribution!**

<template id="b02ecbcd-6ad4-4ccb-980f-265e11d95fe0"><style>
.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  display: table;
  margin-top: 1.275em;
  margin-bottom: 1.275em;
  border-color: transparent;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
</style><div class="tabwid"><style>.cl-bfcb8cfc{}.cl-bfc11646{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-bfc11650{font-family:'Arial';font-size:6.6pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;position: relative;bottom:3.3pt;}.cl-bfc131a8{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-bfc131a9{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-bfc1717c{width:75.6pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc17186{width:69.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc17190{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc17191{width:75.6pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171ae{width:69.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171b8{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171b9{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171ba{width:69.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171c2{width:75.6pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171c3{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171cc{width:69.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171cd{width:75.6pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171ce{width:75.6pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171d6{width:69.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171d7{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171d8{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171e0{width:69.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-bfc171e1{width:75.6pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-bfcb8cfc'>
<caption class="Table Caption">

(#tab:tab2)Alpha = 0.5

</caption>
<thead><tr style="overflow-wrap:break-word;"><td class="cl-bfc171d8"><p class="cl-bfc131a8"><span class="cl-bfc11646">Metric</span></p></td><td class="cl-bfc171e0"><p class="cl-bfc131a9"><span class="cl-bfc11646">n = 1</span></p></td><td class="cl-bfc171e1"><p class="cl-bfc131a9"><span class="cl-bfc11646">n = 10</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-bfc17190"><p class="cl-bfc131a8"><span class="cl-bfc11646">Mean</span><span class="cl-bfc11650">*</span></p></td><td class="cl-bfc17186"><p class="cl-bfc131a9"><span class="cl-bfc11646">Inf</span></p></td><td class="cl-bfc1717c"><p class="cl-bfc131a9"><span class="cl-bfc11646">163,549</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-bfc17190"><p class="cl-bfc131a8"><span class="cl-bfc11646">SD</span><span class="cl-bfc11650">*</span></p></td><td class="cl-bfc17186"><p class="cl-bfc131a9"><span class="cl-bfc11646">Inf</span></p></td><td class="cl-bfc1717c"><p class="cl-bfc131a9"><span class="cl-bfc11646">11,464,535</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-bfc171b9"><p class="cl-bfc131a8"><span class="cl-bfc11646">0.5</span></p></td><td class="cl-bfc171ba"><p class="cl-bfc131a9"><span class="cl-bfc11646">4</span></p></td><td class="cl-bfc171c2"><p class="cl-bfc131a9"><span class="cl-bfc11646">36</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-bfc171c3"><p class="cl-bfc131a8"><span class="cl-bfc11646">0.75</span></p></td><td class="cl-bfc171cc"><p class="cl-bfc131a9"><span class="cl-bfc11646">16</span></p></td><td class="cl-bfc171cd"><p class="cl-bfc131a9"><span class="cl-bfc11646">167</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-bfc171c3"><p class="cl-bfc131a8"><span class="cl-bfc11646">0.8</span></p></td><td class="cl-bfc171cc"><p class="cl-bfc131a9"><span class="cl-bfc11646">25</span></p></td><td class="cl-bfc171cd"><p class="cl-bfc131a9"><span class="cl-bfc11646">269</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-bfc17190"><p class="cl-bfc131a8"><span class="cl-bfc11646">0.9</span></p></td><td class="cl-bfc17186"><p class="cl-bfc131a9"><span class="cl-bfc11646">100</span></p></td><td class="cl-bfc1717c"><p class="cl-bfc131a9"><span class="cl-bfc11646">1,083</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-bfc17190"><p class="cl-bfc131a8"><span class="cl-bfc11646">0.95</span></p></td><td class="cl-bfc17186"><p class="cl-bfc131a9"><span class="cl-bfc11646">400</span></p></td><td class="cl-bfc1717c"><p class="cl-bfc131a9"><span class="cl-bfc11646">4,485</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-bfc17190"><p class="cl-bfc131a8"><span class="cl-bfc11646">0.99</span></p></td><td class="cl-bfc17186"><p class="cl-bfc131a9"><span class="cl-bfc11646">10,000</span></p></td><td class="cl-bfc1717c"><p class="cl-bfc131a9"><span class="cl-bfc11646">85,971</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-bfc17190"><p class="cl-bfc131a8"><span class="cl-bfc11646">0.995</span></p></td><td class="cl-bfc17186"><p class="cl-bfc131a9"><span class="cl-bfc11646">40,000</span></p></td><td class="cl-bfc1717c"><p class="cl-bfc131a9"><span class="cl-bfc11646">424,116</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-bfc171b8"><p class="cl-bfc131a8"><span class="cl-bfc11646">0.999</span></p></td><td class="cl-bfc171ae"><p class="cl-bfc131a9"><span class="cl-bfc11646">1,000,000</span></p></td><td class="cl-bfc17191"><p class="cl-bfc131a9"><span class="cl-bfc11646">8,810,384</span></p></td></tr></tbody><tfoot><tr style="overflow-wrap:break-word;"><td  colspan="3"class="cl-bfc171d7"><p class="cl-bfc131a8"><span class="cl-bfc11650">*</span><span class="cl-bfc11646">Mean and SD are infinite, results are simply artifacts of a finite sample</span></p></td></tr></tfoot></table></div></template>
<div class="flextable-shadow-host" id="4b63eea2-f145-4f7d-930b-87157b3dac1b"></div>
<script>
var dest = document.getElementById("4b63eea2-f145-4f7d-930b-87157b3dac1b");
var template = document.getElementById("b02ecbcd-6ad4-4ccb-980f-265e11d95fe0");
var caption = template.content.querySelector("caption");
if(caption) {
  caption.style.cssText = "display:block;text-align:center;";
  var newcapt = document.createElement("p");
  newcapt.appendChild(caption)
  dest.parentNode.insertBefore(newcapt, dest.previousSibling);
}
var fantome = dest.attachShadow({mode: 'open'});
var templateContent = template.content;
fantome.appendChild(templateContent);
</script>

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

### Impact of Changing n

So we’ve seen that in an infinite mean regime, diversification failed with n=10 was worse than n=1. But what about n = 2, 3…?

Below I’ve repeated the simulation for additional number of risks.

**When dealing with infinte mean random variables, the more “diversified” the exposure, the greater the risk!**

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

## Conclusion

While the result is amazing, we must consider that it’s limited to infinite mean distributions. The authors described some real world risks that had approximates tail factors of less than 1 (marine, wildfire losses). Certainly in the world of insurance where policy limits are the norm, we’d expect losses, no matter how extreme, to be bounded. That said, it’s a fascinating result with consequences for the future of risk management.

Note: Most code has been omitted for brevity. Source code [here](https://github.com/zscore314/starter-hugo-academic/blob/main/content/post/2022-09-03-when-diversification-increases-risk/index.Rmarkdown)
