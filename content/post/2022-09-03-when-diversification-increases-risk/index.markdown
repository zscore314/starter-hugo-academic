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

``` r
quantile_compare <- function(q = c(.5, .75, .8, .9, .95, .99, .995, .999)) {
  tibble(Metric        = c('Mean', 'SD', q), 
         Standalone  = c(mean(a), sd(a), quantile(a, q)),
         Diversified = c(mean(b), sd(b), quantile(b, q)),
         Difference    = Diversified - Standalone
  ) %>%
    mutate_if(is.numeric, ~round(., 3)) %>%
    flextable::flextable()
}

quantile_compare()
```

<template id="ad133f72-6c29-4537-9b7e-fbc5b98e905b"><style>
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
</style><div class="tabwid"><style>.cl-691468a0{}.cl-68ed48e2{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-68ed6e12{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-68ed6e1c{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-68edc5a6{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-68edc5b0{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-68edc5ba{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-68edc5bb{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-68edc5bc{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-68edc5c4{width:54pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-691468a0'>
<thead><tr style="overflow-wrap:break-word;"><td class="cl-68edc5bc"><p class="cl-68ed6e12"><span class="cl-68ed48e2">Metric</span></p></td><td class="cl-68edc5c4"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">Standalone</span></p></td><td class="cl-68edc5c4"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">Diversified</span></p></td><td class="cl-68edc5c4"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">Difference</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-68edc5b0"><p class="cl-68ed6e12"><span class="cl-68ed48e2">Mean</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.496</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.498</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">0.002</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-68edc5b0"><p class="cl-68ed6e12"><span class="cl-68ed48e2">SD</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">0.793</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">0.267</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">-0.526</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-68edc5b0"><p class="cl-68ed6e12"><span class="cl-68ed48e2">0.5</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.266</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.443</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">0.177</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-68edc5b0"><p class="cl-68ed6e12"><span class="cl-68ed48e2">0.75</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.595</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.599</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">0.004</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-68edc5b0"><p class="cl-68ed6e12"><span class="cl-68ed48e2">0.8</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.709</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.643</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">-0.065</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-68edc5b0"><p class="cl-68ed6e12"><span class="cl-68ed48e2">0.9</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">2.161</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.796</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">-0.365</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-68edc5b0"><p class="cl-68ed6e12"><span class="cl-68ed48e2">0.95</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">2.663</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">1.957</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">-0.706</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-68edc5b0"><p class="cl-68ed6e12"><span class="cl-68ed48e2">0.99</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">4.476</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">2.390</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">-2.086</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-68edc5b0"><p class="cl-68ed6e12"><span class="cl-68ed48e2">0.995</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">5.884</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">2.662</span></p></td><td class="cl-68edc5a6"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">-3.222</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-68edc5bb"><p class="cl-68ed6e12"><span class="cl-68ed48e2">0.999</span></p></td><td class="cl-68edc5ba"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">8.626</span></p></td><td class="cl-68edc5ba"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">3.561</span></p></td><td class="cl-68edc5ba"><p class="cl-68ed6e1c"><span class="cl-68ed48e2">-5.065</span></p></td></tr></tbody></table></div></template>
<div class="flextable-shadow-host" id="e210c960-506a-4b2b-b70f-bde2e6871826"></div>
<script>
var dest = document.getElementById("e210c960-506a-4b2b-b70f-bde2e6871826");
var template = document.getElementById("ad133f72-6c29-4537-9b7e-fbc5b98e905b");
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

``` r
ft_1 <- quantile_compare()

ft_1 <- footnote(
  x = ft_1, i = 1:2, j = 1,
  ref_symbols = "*",
  value = as_paragraph("Mean and SD are infinite, results are simply artifacts of a finite sample")
) %>%
  autofit()
  
ft_1
```

<template id="ed0b3056-ef59-44c1-b7e6-0c883fe358e5"><style>
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
</style><div class="tabwid"><style>.cl-6a733b86{}.cl-6a68f2a2{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-6a68f2ac{font-family:'Arial';font-size:6.6pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;position: relative;bottom:3.3pt;}.cl-6a691872{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-6a69187c{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-6a696c6e{width:90.9pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696c78{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696c82{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696c83{width:90.9pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696c8c{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696c8d{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696c96{width:90.9pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696c97{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696c98{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696ca0{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696ca1{width:90.9pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696ca2{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696caa{width:90.9pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696cab{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696cac{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696cb4{width:97pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696cb5{width:50.5pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-6a696cbe{width:90.9pt;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(102, 102, 102, 1.00);border-top: 2pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}</style><table class='cl-6a733b86'>
<thead><tr style="overflow-wrap:break-word;"><td class="cl-6a696cb5"><p class="cl-6a691872"><span class="cl-6a68f2a2">Metric</span></p></td><td class="cl-6a696cbe"><p class="cl-6a69187c"><span class="cl-6a68f2a2">Standalone</span></p></td><td class="cl-6a696cb4"><p class="cl-6a69187c"><span class="cl-6a68f2a2">Diversified</span></p></td><td class="cl-6a696cb4"><p class="cl-6a69187c"><span class="cl-6a68f2a2">Difference</span></p></td></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-6a696c78"><p class="cl-6a691872"><span class="cl-6a68f2a2">Mean</span><span class="cl-6a68f2ac">*</span></p></td><td class="cl-6a696c6e"><p class="cl-6a69187c"><span class="cl-6a68f2a2">10,644.492</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">62,433.757</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">51,789.265</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a696c78"><p class="cl-6a691872"><span class="cl-6a68f2a2">SD</span><span class="cl-6a68f2ac">*</span></p></td><td class="cl-6a696c6e"><p class="cl-6a69187c"><span class="cl-6a68f2a2">478,094.084</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">2,129,566.526</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">1,651,472.443</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a696c97"><p class="cl-6a691872"><span class="cl-6a68f2a2">0.5</span></p></td><td class="cl-6a696c96"><p class="cl-6a69187c"><span class="cl-6a68f2a2">4.006</span></p></td><td class="cl-6a696c98"><p class="cl-6a69187c"><span class="cl-6a68f2a2">35.534</span></p></td><td class="cl-6a696c98"><p class="cl-6a69187c"><span class="cl-6a68f2a2">31.528</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a696ca2"><p class="cl-6a691872"><span class="cl-6a68f2a2">0.75</span></p></td><td class="cl-6a696ca1"><p class="cl-6a69187c"><span class="cl-6a68f2a2">15.496</span></p></td><td class="cl-6a696ca0"><p class="cl-6a69187c"><span class="cl-6a68f2a2">157.156</span></p></td><td class="cl-6a696ca0"><p class="cl-6a69187c"><span class="cl-6a68f2a2">141.660</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a696ca2"><p class="cl-6a691872"><span class="cl-6a68f2a2">0.8</span></p></td><td class="cl-6a696ca1"><p class="cl-6a69187c"><span class="cl-6a68f2a2">24.602</span></p></td><td class="cl-6a696ca0"><p class="cl-6a69187c"><span class="cl-6a68f2a2">248.716</span></p></td><td class="cl-6a696ca0"><p class="cl-6a69187c"><span class="cl-6a68f2a2">224.114</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a696c78"><p class="cl-6a691872"><span class="cl-6a68f2a2">0.9</span></p></td><td class="cl-6a696c6e"><p class="cl-6a69187c"><span class="cl-6a68f2a2">94.803</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">1,032.096</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">937.293</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a696c78"><p class="cl-6a691872"><span class="cl-6a68f2a2">0.95</span></p></td><td class="cl-6a696c6e"><p class="cl-6a69187c"><span class="cl-6a68f2a2">379.403</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">3,693.758</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">3,314.355</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a696c78"><p class="cl-6a691872"><span class="cl-6a68f2a2">0.99</span></p></td><td class="cl-6a696c6e"><p class="cl-6a69187c"><span class="cl-6a68f2a2">11,195.401</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">80,466.282</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">69,270.882</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a696c78"><p class="cl-6a691872"><span class="cl-6a68f2a2">0.995</span></p></td><td class="cl-6a696c6e"><p class="cl-6a69187c"><span class="cl-6a68f2a2">45,339.143</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">303,869.701</span></p></td><td class="cl-6a696c82"><p class="cl-6a69187c"><span class="cl-6a68f2a2">258,530.558</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-6a696c8d"><p class="cl-6a691872"><span class="cl-6a68f2a2">0.999</span></p></td><td class="cl-6a696c83"><p class="cl-6a69187c"><span class="cl-6a68f2a2">1,120,334.249</span></p></td><td class="cl-6a696c8c"><p class="cl-6a69187c"><span class="cl-6a68f2a2">11,495,658.503</span></p></td><td class="cl-6a696c8c"><p class="cl-6a69187c"><span class="cl-6a68f2a2">10,375,324.254</span></p></td></tr></tbody><tfoot><tr style="overflow-wrap:break-word;"><td  colspan="4"class="cl-6a696cac"><p class="cl-6a691872"><span class="cl-6a68f2ac">*</span><span class="cl-6a68f2a2">Mean and SD are infinite, results are simply artifacts of a finite sample</span></p></td></tr></tfoot></table></div></template>
<div class="flextable-shadow-host" id="55ebc6bc-bcbc-4100-b09e-243bec04ccbd"></div>
<script>
var dest = document.getElementById("55ebc6bc-bcbc-4100-b09e-243bec04ccbd");
var template = document.getElementById("ed0b3056-ef59-44c1-b7e6-0c883fe358e5");
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
