---
title: When Diversification Increases Risk
author: Zach Eisenstein
date: '2022-09-03'
draft: true
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
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

<link href="{{< blogdown/postref >}}index_files/tabwid/tabwid.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index_files/tabwid/scrool.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index_files/tabwid/tabwid.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index_files/tabwid/scrool.css" rel="stylesheet" />

## Intro

Harry Markowitz of [Modern Portfolio Theory](https://en.wikipedia.org/wiki/Modern_portfolio_theory) fame said that “Diversification is the only free lunch in investing” as it allows investors to reduce volatility without eroding returns. Diversification, through pooling of independent insured exposures, is also fundamental to risk management in insurance. **But what if risk pooling actually *increased* risk?**

The great Paul Embrechts and co-authors recently published an article entitled [An unexpected stochastic dominance: Pareto distributions catastrophes, and risk exchange](https://arxiv.org/pdf/2208.08471.pdf). In this brief post I will explore the paper’s unintuitive findings using simulation.

## A brief review of the pareto distribution

The [pareto distribution](https://en.wikipedia.org/wiki/Pareto_distribution) is a power-law distribution that was originally used to measure wealth inequality and underlies the familiar [80-20 rule](https://en.wikipedia.org/wiki/Pareto_principle). In its most basic form, it contains two variables, a shape variable {{\< math \>}}$\alpha${{\< /math \>}}, and a scale {{\< math \>}}$\theta${{\< /math \>}} (minimum value, typically known) with probability density {{\< math \>}}$f(x) = \frac{\alpha \theta^\alpha}{(x)^{\alpha + 1}}${{\< /math \>}}, CDF {{\< math \>}}$F(x) = 1 - (\frac{\theta}{x})^{\alpha}${{\< /math \>}} and mean {{\< math \>}}$E[X] = \theta \frac{\alpha}{\alpha - 1}${{\< /math \>}}.

The shape {{\< math \>}}$\alpha${{\< /math \>}}, (“tail exponent”) controls the degree to which rare, extreme events disproportionately control the shape of the distribution (e.g. the mean).

Below you can see the effect of {{\< math \>}}$\alpha${{\< /math \>}} on the tail of the distribution.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-1.png" width="672" />

## The unexpected stochastic dominance

Let’s first introduce the concept of stochastic dominance.

> For two random variable X and Y representing random losses, we say X is smaller than Y in first-order stochastic dominance, denoted by {{\< math \>}}$X \le_{st} Y${{\< /math \>}}, if {{\< math \>}}$P(X \le x) \ge P(Y \le x) for x \in \mathbb{R}${{\< /math \>}}

This implies that X is the preferred risk for decision makers as globally, for a given loss amount x, it has a lower probability of exceeding x than Y.

The key result in the paper is Theorem 1.

> For iid random variables {{\< math \>}}$X_1, ..., X_n${{\< /math \>}} following a Pareto distribution with infinite mean and weights {{\< math \>}}$\theta_1, ..., \theta_n${{\< /math \>}} with {{\< math \>}}$\sum_{i = 1}^{n} \theta_i = 1${{\< /math \>}}, our main finding in Theorem 1 is the stochastic dominance relation {{\< math \>}}$$X_1 \le_{st} \theta_1 X_1 + ... + \theta_n X_n$${{\< /math \>}}

In plain english, for a pareto distributed variable with infinite mean (i.e. {{\< math \>}}$\alpha < 1${{\< /math \>}}), the quantiles (or Value at Risk–VaR) of a single random variable will be **less than or equal to** that of a “diversified” portfolio of identical risks! While VaR has been known to fail sub-additivity, that has been confined to some contrived cases and for select percentiles. The amazing result here is that this holds across the full distribution. You’re better off putting all your eggs in one basket!

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

## A simulation approach

### Finite Mean

I’ll explore the finding above using simulation and visualization.

First lets look at the case of finite mean, where diversification works. We’ll simulate 10 iid parteo random variables with {{\< math \>}}$\alpha = 3${{\< /math \>}} and compare the distributions of a standalone portfolio of X1 to a portfolio with 10 equal shares of X1:X10

``` r
trials <- 10e3
alpha <- 3
n <- 10 # number of iid pareto variables

# Simulate n iid pareto variables
sim <- matrix(rpareto(n * trials, shape = alpha, location = 1),
           nrow = trials)

# single risk
a <- sim[, 1]

# a tenth of each risk
b <- rowSums(sim / n)
```

<template id="0e3cb7e7-47e9-4eaa-ba7f-945cbad139b5"><style>
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
</style><div class="tabwid"><style>.cl-6ab2bb16{}.cl-6a626968{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-6a630d96{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-6a630db4{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-6a63ac92{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a63aca6{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a63acb0{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a63acb1{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a63acba{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a63acbb{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-6ab2bb16'>
<thead><tr style="overflow-wrap:break-word;"><td class="cl-6a63acba"><p class="cl-6a630d96"><span class="cl-6a626968">Metric</span></p></td><td class="cl-6a63acbb"><p class="cl-6a630db4"><span class="cl-6a626968">Standalone</span></p></td><td class="cl-6a63acbb"><p class="cl-6a630db4"><span class="cl-6a626968">Diversified</span></p></td><td class="cl-6a63acbb"><p class="cl-6a630db4"><span class="cl-6a626968">Difference</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-6a63aca6"><p class="cl-6a630d96"><span class="cl-6a626968">Mean</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.494</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.505</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">0.010</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a63aca6"><p class="cl-6a630d96"><span class="cl-6a626968">SD</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">0.790</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">0.281</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">-0.509</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a63aca6"><p class="cl-6a630d96"><span class="cl-6a626968">0.5</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.259</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.446</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">0.187</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a63aca6"><p class="cl-6a630d96"><span class="cl-6a626968">0.75</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.592</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.603</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">0.012</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a63aca6"><p class="cl-6a630d96"><span class="cl-6a626968">0.8</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.711</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.654</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">-0.057</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a63aca6"><p class="cl-6a630d96"><span class="cl-6a626968">0.9</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">2.151</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.801</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">-0.350</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a63aca6"><p class="cl-6a630d96"><span class="cl-6a626968">0.95</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">2.680</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">1.967</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">-0.712</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a63aca6"><p class="cl-6a630d96"><span class="cl-6a626968">0.99</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">4.532</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">2.438</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">-2.094</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a63aca6"><p class="cl-6a630d96"><span class="cl-6a626968">0.995</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">5.891</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">2.872</span></p></td><td class="cl-6a63ac92"><p class="cl-6a630db4"><span class="cl-6a626968">-3.019</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a63acb1"><p class="cl-6a630d96"><span class="cl-6a626968">0.999</span></p></td><td class="cl-6a63acb0"><p class="cl-6a630db4"><span class="cl-6a626968">8.821</span></p></td><td class="cl-6a63acb0"><p class="cl-6a630db4"><span class="cl-6a626968">3.815</span></p></td><td class="cl-6a63acb0"><p class="cl-6a630db4"><span class="cl-6a626968">-5.005</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="23d1833f-83a8-4221-8e65-36e21f1de473"></div>
<script>
var dest = document.getElementById("23d1833f-83a8-4221-8e65-36e21f1de473");
var template = document.getElementById("0e3cb7e7-47e9-4eaa-ba7f-945cbad139b5");
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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

### Ininite Mean

Now lets look at the same metrics using an infinite mean pareto, with {{\< math \>}}$\alpha = 0.5${{\< /math \>}}

``` r
trials <- 10e3
alpha <- 0.5
n <- 10 # number of iid pareto variables

# Simulate n iid pareto variables
sim <- matrix(rpareto(n * trials, shape = alpha, location = 1),
           nrow = trials)

# single risk
a <- sim[, 1]

# a tenth of each risk
b <- rowSums(sim / n)
```

<template id="392ceadf-a8a7-4489-8652-2c74edb315ee"><style>
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
</style><div class="tabwid"><style>.cl-6c5f9588{}.cl-6c49e2f6{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-6c49e30a{font-family:'Arial';font-size:6.6pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;position: relative;bottom:3.3pt;}.cl-6c4a0e66{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-6c4a0e7a{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-6c4a7982{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a7996{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a7997{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a79a0{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a79a1{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a79aa{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a79ab{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a79b4{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a79b5{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a79be{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a79bf{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6c4a79c8{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-6c5f9588'>
<thead><tr style="overflow-wrap:break-word;"><td class="cl-6c4a79c8"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">Metric</span></p></td><td class="cl-6c4a79bf"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">Standalone</span></p></td><td class="cl-6c4a79bf"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">Diversified</span></p></td><td class="cl-6c4a79bf"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">Difference</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-6c4a7996"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">Mean</span><span class="cl-6c49e30a">*</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">158,615.597</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">811,511.128</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">652,895.530</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6c4a7996"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">SD</span><span class="cl-6c49e30a">*</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">12,204,965.481</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">59,095,411.518</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">46,890,446.037</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6c4a79aa"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">0.5</span></p></td><td class="cl-6c4a79a1"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">3.975</span></p></td><td class="cl-6c4a79a1"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">35.363</span></p></td><td class="cl-6c4a79a1"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">31.389</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6c4a79b4"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">0.75</span></p></td><td class="cl-6c4a79ab"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">15.601</span></p></td><td class="cl-6c4a79ab"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">157.450</span></p></td><td class="cl-6c4a79ab"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">141.849</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6c4a79aa"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">0.8</span></p></td><td class="cl-6c4a79a1"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">24.323</span></p></td><td class="cl-6c4a79a1"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">243.632</span></p></td><td class="cl-6c4a79a1"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">219.309</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6c4a79aa"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">0.9</span></p></td><td class="cl-6c4a79a1"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">96.526</span></p></td><td class="cl-6c4a79a1"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">975.462</span></p></td><td class="cl-6c4a79a1"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">878.936</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6c4a7996"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">0.95</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">406.599</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">4,280.195</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">3,873.596</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6c4a7996"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">0.99</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">10,351.704</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">105,152.124</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">94,800.420</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6c4a7996"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">0.995</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">40,512.956</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">509,940.777</span></p></td><td class="cl-6c4a7982"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">469,427.821</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6c4a79a0"><p class="cl-6c4a0e66"><span class="cl-6c49e2f6">0.999</span></p></td><td class="cl-6c4a7997"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">2,779,334.905</span></p></td><td class="cl-6c4a7997"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">11,471,264.812</span></p></td><td class="cl-6c4a7997"><p class="cl-6c4a0e7a"><span class="cl-6c49e2f6">8,691,929.907</span></p></td></tr></tbody><tfoot><tr style="overflow-wrap:break-word;"><td  colspan="4"class="cl-6c4a79be"><p class="cl-6c4a0e66"><span class="cl-6c49e30a">*</span><span class="cl-6c49e2f6">Mean and SD are infinite, results are simply artifacts of a finite sample</span></p></td></tr></tfoot></table></div></template>
<div class="flextable-shadow-host" id="9df5bcba-2458-4780-871f-2fce2f80f75b"></div>
<script>
var dest = document.getElementById("9df5bcba-2458-4780-871f-2fce2f80f75b");
var template = document.getElementById("392ceadf-a8a7-4489-8652-2c74edb315ee");
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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

## Conclusion

While the result is amazing, we must consider that it’s limited to infinite mean distributions. The authors described some real world risks that had approximates tail factors of less than 1 (marine and wildfire losses). Certainly in the world of insurance where policy limits are the norm, we’d expect losses, no matter how extreme, to be bounded. That said, it’s a fascinating result with consequences for the future of risk management.
