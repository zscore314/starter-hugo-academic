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

<template id="5d47101a-76d1-450e-9169-4f676144d31b"><style>
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
</style><div class="tabwid"><style>.cl-9676ca12{}.cl-9659a98c{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-9659d8e4{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-9659d8ee{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-965a5152{width:70.7pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a515c{width:76.2pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a515d{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a5166{width:71.9pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a5167{width:70.7pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a5170{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a5171{width:76.2pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a517a{width:71.9pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a517b{width:70.7pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a517c{width:76.2pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a5184{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a518e{width:71.9pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a518f{width:70.7pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a5198{width:71.9pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a5199{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a51a2{width:76.2pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a51a3{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a51ac{width:71.9pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a51ad{width:76.2pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-965a51b6{width:70.7pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-9676ca12'>
<thead><tr style="overflow-wrap:break-word;"><td class="cl-965a51a3"><p class="cl-9659d8e4"><span class="cl-9659a98c">Metric</span></p></td><td class="cl-965a51ad"><p class="cl-9659d8ee"><span class="cl-9659a98c">Standalone</span></p></td><td class="cl-965a51ac"><p class="cl-9659d8ee"><span class="cl-9659a98c">Diversified</span></p></td><td class="cl-965a51b6"><p class="cl-9659d8ee"><span class="cl-9659a98c">Difference</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-965a515d"><p class="cl-9659d8e4"><span class="cl-9659a98c">Mean</span></p></td><td class="cl-965a515c"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.515</span></p></td><td class="cl-965a5166"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.502</span></p></td><td class="cl-965a5152"><p class="cl-9659d8ee"><span class="cl-9659a98c">-0.012</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-965a5184"><p class="cl-9659d8e4"><span class="cl-9659a98c">SD</span></p></td><td class="cl-965a517c"><p class="cl-9659d8ee"><span class="cl-9659a98c">0.919</span></p></td><td class="cl-965a518e"><p class="cl-9659d8ee"><span class="cl-9659a98c">0.269</span></p></td><td class="cl-965a517b"><p class="cl-9659d8ee"><span class="cl-9659a98c">-0.651</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-965a5199"><p class="cl-9659d8e4"><span class="cl-9659a98c">0.5</span></p></td><td class="cl-965a51a2"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.263</span></p></td><td class="cl-965a5198"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.444</span></p></td><td class="cl-965a518f"><p class="cl-9659d8ee"><span class="cl-9659a98c">0.180</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-965a515d"><p class="cl-9659d8e4"><span class="cl-9659a98c">0.75</span></p></td><td class="cl-965a515c"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.600</span></p></td><td class="cl-965a5166"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.601</span></p></td><td class="cl-965a5152"><p class="cl-9659d8ee"><span class="cl-9659a98c">0.001</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-965a515d"><p class="cl-9659d8e4"><span class="cl-9659a98c">0.8</span></p></td><td class="cl-965a515c"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.724</span></p></td><td class="cl-965a5166"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.649</span></p></td><td class="cl-965a5152"><p class="cl-9659d8ee"><span class="cl-9659a98c">-0.075</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-965a5199"><p class="cl-9659d8e4"><span class="cl-9659a98c">0.9</span></p></td><td class="cl-965a51a2"><p class="cl-9659d8ee"><span class="cl-9659a98c">2.185</span></p></td><td class="cl-965a5198"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.807</span></p></td><td class="cl-965a518f"><p class="cl-9659d8ee"><span class="cl-9659a98c">-0.378</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-965a515d"><p class="cl-9659d8e4"><span class="cl-9659a98c">0.95</span></p></td><td class="cl-965a515c"><p class="cl-9659d8ee"><span class="cl-9659a98c">2.748</span></p></td><td class="cl-965a5166"><p class="cl-9659d8ee"><span class="cl-9659a98c">1.980</span></p></td><td class="cl-965a5152"><p class="cl-9659d8ee"><span class="cl-9659a98c">-0.768</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-965a5199"><p class="cl-9659d8e4"><span class="cl-9659a98c">0.99</span></p></td><td class="cl-965a51a2"><p class="cl-9659d8ee"><span class="cl-9659a98c">4.872</span></p></td><td class="cl-965a5198"><p class="cl-9659d8ee"><span class="cl-9659a98c">2.477</span></p></td><td class="cl-965a518f"><p class="cl-9659d8ee"><span class="cl-9659a98c">-2.394</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-965a5199"><p class="cl-9659d8e4"><span class="cl-9659a98c">0.995</span></p></td><td class="cl-965a51a2"><p class="cl-9659d8ee"><span class="cl-9659a98c">6.500</span></p></td><td class="cl-965a5198"><p class="cl-9659d8ee"><span class="cl-9659a98c">2.764</span></p></td><td class="cl-965a518f"><p class="cl-9659d8ee"><span class="cl-9659a98c">-3.737</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-965a5170"><p class="cl-9659d8e4"><span class="cl-9659a98c">0.999</span></p></td><td class="cl-965a5171"><p class="cl-9659d8ee"><span class="cl-9659a98c">12.034</span></p></td><td class="cl-965a517a"><p class="cl-9659d8ee"><span class="cl-9659a98c">3.694</span></p></td><td class="cl-965a5167"><p class="cl-9659d8ee"><span class="cl-9659a98c">-8.340</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="29744f9c-30cc-4f8d-8905-bab86a9c8fee"></div>
<script>
var dest = document.getElementById("29744f9c-30cc-4f8d-8905-bab86a9c8fee");
var template = document.getElementById("5d47101a-76d1-450e-9169-4f676144d31b");
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

<template id="9b235cc1-d7d3-4471-be8f-8e79d1c1a843"><style>
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
</style><div class="tabwid"><style>.cl-97fc3aa2{}.cl-97e1c532{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-97e1c546{font-family:'Arial';font-size:6.6pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;position: relative;bottom:3.3pt;}.cl-97e216f4{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-97e21712{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-97e370f8{width:81.7pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e37116{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e37120{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e3712a{width:81.7pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e37134{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e3713e{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e37148{width:81.7pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e37152{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e3715c{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e37166{width:81.7pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e37170{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e3717a{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e37184{width:81.7pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e37198{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e371ac{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e371b6{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e371b7{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-97e371c0{width:81.7pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-97fc3aa2'>
<thead><tr style="overflow-wrap:break-word;"><td class="cl-97e371b7"><p class="cl-97e216f4"><span class="cl-97e1c532">Metric</span></p></td><td class="cl-97e371c0"><p class="cl-97e21712"><span class="cl-97e1c532">Standalone</span></p></td><td class="cl-97e371b6"><p class="cl-97e21712"><span class="cl-97e1c532">Diversified</span></p></td><td class="cl-97e371b6"><p class="cl-97e21712"><span class="cl-97e1c532">Difference</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-97e37116"><p class="cl-97e216f4"><span class="cl-97e1c532">Mean</span><span class="cl-97e1c546">*</span></p></td><td class="cl-97e370f8"><p class="cl-97e21712"><span class="cl-97e1c532">6,376.632</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">166,943.159</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">160,566.527</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-97e37116"><p class="cl-97e216f4"><span class="cl-97e1c532">SD</span><span class="cl-97e1c546">*</span></p></td><td class="cl-97e370f8"><p class="cl-97e21712"><span class="cl-97e1c532">260,530.768</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">10,432,408.013</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">10,171,877.246</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-97e37152"><p class="cl-97e216f4"><span class="cl-97e1c532">0.5</span></p></td><td class="cl-97e37148"><p class="cl-97e21712"><span class="cl-97e1c532">4.023</span></p></td><td class="cl-97e3715c"><p class="cl-97e21712"><span class="cl-97e1c532">35.376</span></p></td><td class="cl-97e3715c"><p class="cl-97e21712"><span class="cl-97e1c532">31.353</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-97e37152"><p class="cl-97e216f4"><span class="cl-97e1c532">0.75</span></p></td><td class="cl-97e37148"><p class="cl-97e21712"><span class="cl-97e1c532">16.144</span></p></td><td class="cl-97e3715c"><p class="cl-97e21712"><span class="cl-97e1c532">160.979</span></p></td><td class="cl-97e3715c"><p class="cl-97e21712"><span class="cl-97e1c532">144.834</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-97e37170"><p class="cl-97e216f4"><span class="cl-97e1c532">0.8</span></p></td><td class="cl-97e37166"><p class="cl-97e21712"><span class="cl-97e1c532">25.561</span></p></td><td class="cl-97e3717a"><p class="cl-97e21712"><span class="cl-97e1c532">255.421</span></p></td><td class="cl-97e3717a"><p class="cl-97e21712"><span class="cl-97e1c532">229.860</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-97e37116"><p class="cl-97e216f4"><span class="cl-97e1c532">0.9</span></p></td><td class="cl-97e370f8"><p class="cl-97e21712"><span class="cl-97e1c532">101.644</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">1,075.581</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">973.937</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-97e37116"><p class="cl-97e216f4"><span class="cl-97e1c532">0.95</span></p></td><td class="cl-97e370f8"><p class="cl-97e21712"><span class="cl-97e1c532">385.605</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">3,993.518</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">3,607.913</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-97e37116"><p class="cl-97e216f4"><span class="cl-97e1c532">0.99</span></p></td><td class="cl-97e370f8"><p class="cl-97e21712"><span class="cl-97e1c532">12,538.558</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">120,867.418</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">108,328.861</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-97e37116"><p class="cl-97e216f4"><span class="cl-97e1c532">0.995</span></p></td><td class="cl-97e370f8"><p class="cl-97e21712"><span class="cl-97e1c532">44,657.672</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">551,609.643</span></p></td><td class="cl-97e37120"><p class="cl-97e21712"><span class="cl-97e1c532">506,951.971</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-97e3713e"><p class="cl-97e216f4"><span class="cl-97e1c532">0.999</span></p></td><td class="cl-97e3712a"><p class="cl-97e21712"><span class="cl-97e1c532">776,974.969</span></p></td><td class="cl-97e37134"><p class="cl-97e21712"><span class="cl-97e1c532">10,297,499.340</span></p></td><td class="cl-97e37134"><p class="cl-97e21712"><span class="cl-97e1c532">9,520,524.371</span></p></td></tr></tbody><tfoot><tr style="overflow-wrap:break-word;"><td  colspan="4"class="cl-97e371ac"><p class="cl-97e216f4"><span class="cl-97e1c546">*</span><span class="cl-97e1c532">Mean and SD are infinite, results are simply artifacts of a finite sample</span></p></td></tr></tfoot></table></div></template>
<div class="flextable-shadow-host" id="1d043bb3-d709-4aeb-8005-494254e05728"></div>
<script>
var dest = document.getElementById("1d043bb3-d709-4aeb-8005-494254e05728");
var template = document.getElementById("9b235cc1-d7d3-4471-be8f-8e79d1c1a843");
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
