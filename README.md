# Accelerated Data Analysis & Pipeline Toolkit (ADAPT)

ADAPT is an analytics toolkit developed by the Social Investment Agency to accelerate the delivery of high quality insights. It standardises and automates common analytic tasks, reducing delivery time while improving consistency, transparency, and robustness.

## Overview

Analytic and research projects often begin with a period of data wrangling. As the number of data sources increases, so too does the complexity of preparation. ADAPT has been developed by the Social Investment Agency (SIA) in response to this challenge. It provides a standardised, automated approach to common analytic tasks, enabling analysts to move from data to insight more quickly and reliably.

By imposing structure and automation across analytic workflows, ADAPT significantly reduces development time while improving quality. Projects that once took months can now be delivered in weeks, and weeks can become days. The toolkit supports consistent, transparent, and robust analysis by simplifying repeatable processes, clarifying workflow, and reducing errors.

ADAPT is designed for researchers and analysts who want dependable, scalable approaches to analytics. It is now available for other researchers and analysts to benefit from.

## User guidance



## Four component Tools

ADAPT consists of four modular tools that share common functionality and are distributed together as a single toolkit. Each tool can be used by itself or combined to support end-to-end delivery.

* **Summarise** - Simplifies the creation of summary statistics (e.g. counts, totals, means) from unit record data, with flexible filtering and subsetting.

* **Confidentialise** - Applies user defined confidentiality rules to summarised results, supporting safe release of outputs from secure environments such as the IDI.

* **Assembly** - Fetches and combines data from multiple source tables into a single rectangular “master” table. This standardised step improves transparency, traceability, and reproducibility.

* **Pipeline** - Automates the execution of entire analyses, with scheduling and robust error handling to enable unattended, repeatable runs.

## Analytic Delivery Framework

The design of ADAPT reflects the delivery framework used in SIA IDI projects. While not mandatory, this framework helps clarify how the tools fit together:

* **Definitions** – Identify and extract the concepts or measures required for analysis from source data.
* **Assembly** – Combine all required data into a single master table that serves as the source of truth for analysis.
* **Analyse** – Perform analysis, including data cleaning, modelling, and summarisation.
* **Output** – Prepare results for delivery, including validation, standardisation, and privacy protections.

## Successor to Earlier Tools

ADAPT is the successor to SIA’s earlier dataset assembly, summarisation, and confidentialisation tools. It combines the strongest features of those tools with newer design improvements, resulting in a significant step up in quality, usability, and capability.

## Control Files and Reliability

ADAPT uses control files (Excel or CSV) to specify how each tool should run. Control files provide a standard, structured way to define inputs and behaviour, and are validated before execution. This approach serves both as configuration and documentation, making analyses easier to review, modify, and reproduce.

Because control files require no programming, ADAPT is accessible to users regardless of their preferred language. Although implemented as an R package, users typically interact only with control file templates and simple inputs.

Reliability is a core design principle of ADAPT. The toolkit is extensively tested using automated tests that compare example inputs to expected outputs. ADAPT includes hundreds of such tests, which are run whenever the toolkit is updated to prevent regressions and ensure correctness.

To further support correct and confident use, ADAPT provides worked examples, templates, and built‑in input validation with clear guidance when inputs are invalid. Together, these features support robust, transparent, and dependable analytic delivery.

## Getting started

Like any piece of software, the toolkit must be installed before it can be used. As ADAPT is provided as an R package, users must have R installed. We recommend a recent version of R with RStudio as the development environment.

To install the toolkit, you need a copy of the package file in a folder that is accessible from R. The latest public version of the package can be downloaded from on our GitHub page : this is the ADAPT_*.tar.gz file from this repository.

Once you have a copy of the package, Code block 1 provides the fastest way to install ADAPT. This uses the remotes package to automate installation of all its dependencies. When you execute Code block 1, it will open a file select window and prompt you to navigate to the package (*.tar.gz) file for installation.

```r
# installation helper
if(!"remotes" %in% installed.packages()){
  install.packages("remotes")
}

# locate package file
package_path = rstudioapi::selectFile(caption = "Select R package", filter = "Packages (*.gz)")

# install
remotes::install_local(path = package_path)
```

## Built-in Learning Resources

ADAPT includes several built-in resources to help users learn and use the toolkit effectively once installed:

* Worked examples and templates - The toolkit provides a set of worked examples and control file templates that demonstrate common use cases. These can be copied locally and adapted for new analyses.

* Command discovery - All available ADAPT functions can be easily explored from within R, making it straightforward to discover available tools and understand their purpose.

* Standard documentation - Every function in ADAPT includes detailed help documentation covering inputs, outputs, and expected behaviour. Users can access this directly from R, including keyword search across help files.

Together, these resources support self-guided learning, reduce onboarding time, and help users understand how to configure and run analyses correctly.


## Adoption and adaptation
We encourage researchers to adopt the assembly tool into their own research projects. To enhance this adoption, the overview document notes a range of patterns that shape our wider workflow that we have found to enhance our research. We hope other researchers and analysts also find these valuable, but we accept that others may wish to continue using their existing practices, simply adopting the tool to speed things up.

We discourage researchers in the IDI from editing the code of the tool. Part of the value of a tool is the consistency it provides between researchers. When researchers begin to edit the tool, then we no longer have a shared tool but instead have a collection of bespoke code that with a common origin. The overview document provides some guidance for other ways you can build upon the Dataset Assembly Tool without editing its code.






## Citation
Social Investment Agency (2026). Accelerated Data Analysis & Pipeline Toolkit (ADAPT). Source code. https://github.com/nz-social-investment-agency/ADAPT

## Getting Help
Enquiries can be sent to info@sia.govt.nz
