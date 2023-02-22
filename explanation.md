---
title: "Explanation of R examples"
author: "Clement Lee"
date: "2023-02-22"
output: pdf_document
papersize: a4
---

In "bad", there are files in common formats generated from the problematic script **regression.R**. I tried to include as many issues as I can come up with, and I'm sure you won't want to run it when you look at it. In "good", **regression.Rmd** is an attempt to tell the same story but in a literate way. Using it, references.bib and BloodPressure.csv, you should be able to generate the pdfs and htmls. In the spirit of reproducibility, feel free to test if it works on your machine.

The Rmd is relatively simple as I hoped to make the statistical analysis and the code as widely applicable as possible. I didn't include a lot of text as I want to be flexible with the format of the generated output, as you can see from the four generated files. In the actual materials we can add more explanation.

I couldn't quite decide whether to include ggplot2 (and other tidyverse packages) or not, and in the end I included one ggplot as a minimal example. I didn't want to make this too R-centric, but at the same time the philosophy and development of tidyverse aligns with almost all topics of CodeRep. Maybe we can discuss about this.

Saad also made a good point on using contributed packages (including ggplot2), which is that, as the analysis and file structure gets more advanced, it is likely that we need to organise the package dependencies in one place. This could be a teaching point in literate programming, or a connection to the topic of architecture / coding style.

There are several aspects which I think can potentially be incorporated in the materials:

  1. Software requirements: what needs to be installed on what OS and what platform. The collection of pandoc, (R)markdown, LaTeX, knitr, Quarto, etc. could be confusing enough to deter some people at this first hurdle.
  2. Bad coding practices: uninformative naming of variables & objects; reusing names for different objects (thus overwriting); inconsistencies of assignment operators, white spaces, and indentation; the taboos: absolute file path, setwd(), remove(list = ls()), etc.; no set.seed() when generating (pseudo-)random numbers; use of the dot in R (as it means member function in other languages)? 
  3. Non-reproducibility (hard coding numbers, using pre-generated plots) vs reproducibility (dynamically generating results with one button click or command, even when data changes)
  4. Documentation: moving away from comments in the R script, to prose and code chunks interweaving in a logical and readable fashion, leading to clear and concise documentation
  5. Chunk options: facilitating the code chunks in a flexible way, giving the user choices of printing the code, evaluating the code, and displaying the results, according to the format of the generated output
  6. Generated output: various purposes (technical report, package vignette, journal article, presentation slides) in various formats (md/pdf/html/docx), meeting various requirements (machine readable for GitHub, webpage for accessibility, docx for non-technical collaborators)
  7. Linking to other topics: version control (to keep track of source file and generated output); architecture/coding style (to organise dependencies); containers (to ensure full reproducibility); documentation (especially if developing a growing set of functions into a package); CI/CD (YAML as I saw it in one of their ideas)
