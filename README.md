# Accelerated Data Analysis & Pipeline Toolkit (ADAPT)

ADAPT is an analytics toolkit developed by the Social Investment Agency (SIA) to accelerate the delivery of high quality insights. It standardises and automates common analytic tasks, reducing delivery time while improving consistency, transparency, and robustness.

By imposing structure and automation across analytic workflows, ADAPT significantly reduces development time while improving quality. Projects that once took months can now be delivered in weeks, and weeks can become days. The toolkit simplifies repeatable processes, clarifies workflows, and reduces errors, making it a dependable and scalable option for researchers and analysts. ADAPT is now available for others to use and benefit from.

ADAPT is the successor to SIA’s earlier dataset assembly, summarisation, and confidentialisation tools. It combines the strongest features of those tools with newer design improvements, resulting in a significant step up in quality, usability, and capability.

## Four component tools

ADAPT consists of four modular tools that share common functionality and are distributed together as a single toolkit. Each tool can be used by itself or combined to support end-to-end delivery.

* **Summarise** - Simplifies the creation of summary statistics (e.g. counts, totals, means) from unit record data, with flexible filtering and subsetting.
* **Confidentialise** - Applies user defined confidentiality rules to summarised results, supporting safe release of outputs from the IDI.
* **Assembly** - Fetches and combines data from multiple source tables into a single rectangular “master” table ready for analysis.
* **Pipeline** - Automates the execution of entire analyses, with scheduling and robust error handling to enable unattended, repeatable runs.

## User guidance

This repository provides the source code for ADAPT and the bundled R package, ready for installation. It should be used alongside the user guidance: ......................

This contains a discussion of the design principles and detailed instructions on the use of each component tool.


## Analytic delivery framework

The design of ADAPT reflects the framework SIA used for the delivery of IDI projects. While not mandatory, this framework helps clarify how the tools fit together:

1. **Definitions** – Construct from source data the best implementation of concepts that is possible in the data.
2. **Assembly** – Combine all required data into a single master table that serves as the source of truth for analysis.
3. **Analyse** – Perform analysis, including data cleaning, modelling, and summarisation.
4. **Output** – Prepare results for delivery, including validation, standardisation, and privacy protections.

## Control files and accessibility

ADAPT uses Excel or CSV control files to specify how each tool should run. Control files provide a standard, structured way to define inputs and behaviour. This approach serves both as configuration and documentation, making analyses easier to review, modify, and reproduce.

Because control files require no programming, ADAPT is accessible to users regardless of their preferred language. Although implemented as an R package, users typically interact only with control file templates and simple scripts.

## Reliability and validation

Reliability is a core design principle of ADAPT. The source code includes an extensive set of automated tests that compare example inputs to expected outputs. ADAPT includes hundreds of such tests, which are run whenever the toolkit is updated to ensure correctness.

In addition, every component of ADAPT has built‑in input validation. This ensures tools only execute when their inputs are valid and that users receive clear guidance when inputs are not. 

## Getting started

Like any piece of software, the toolkit must be installed before it can be used. As ADAPT is provided as an R package, users must have R installed. The next step is to obtain a copy of the package. The latest public version of the package can be downloaded from this GitHub repository: it is the ADAPT_*.tar.gz file.

Once you have a copy of the package, the following code provides the fastest way to install ADAPT. This uses the remotes package to automate installation of all its dependencies. When you execute the code, it will open a file select window and prompt you to navigate to the package (*.tar.gz) file for installation.

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

## Learning resources

ADAPT includes several built-in resources to help users learn and use the toolkit once installed:

* **Worked examples and templates** - The toolkit provides a set of worked examples and control file templates that demonstrate common use cases. They are accessed using the `provide_example()` command from within R.
* **Command discovery** - Typing `ADAPT::` at the console within RStudio should trigger a popup list of the different functions in the package.
* **Standard documentation** - Every function in ADAPT includes detailed help documentation covering its inputs, outputs, and expected behaviour. Users can access this directly from R, using the standard `?` help command.

Together, these resources support self-guided learning, reduce onboarding time, and help users understand how to configure and run analyses.

## Citation

Social Investment Agency (2026). Accelerated Data Analysis & Pipeline Toolkit (ADAPT). Source code. https://github.com/nz-social-investment-agency/ADAPT

## Getting help
Enquiries can be sent to info@sia.govt.nz
