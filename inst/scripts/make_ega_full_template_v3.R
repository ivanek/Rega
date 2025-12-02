## Submission template 'ega_full_template_v3.xlsx' was created using following
## script
## Author: Igor Cervenka
## Licence: Artistic-2.0
library(openxlsx)

# Data -------------------------------------------------------------------------
## Get enums
# Either requires access credentials to EGA (see vignette how to set them up),
# or it will read from a file bundled with the package
enums <- tryCatch(
  {
    ega <- Rega::create_client(Rega::extract_api(), verbosity = 0)
    lapply(ega$get__enums()$enums, function(x) {
      apply(
        Rega::get_enum(ega, x),
        1,
        \(row) paste0(trimws(row), collapse = "--")
      )
    }) |>
      setNames(ega$get__enums()$enums)
  },
  error = function(e_api) {
    file <- system.file("extdata/ega_enums.rds", package = "Rega")
    message("API access failed: ", conditionMessage(e_api))
    message("Falling back to local file: ", file)
    tryCatch(
      readRDS(file),
      error = function(e_file) {
        stop(
          sprintf("File fallback failed: %s", conditionMessage(e_file)),
          call. = FALSE
        )
      }
    )
  }
)

enums_df <- as.data.frame(
  lapply(enums[names(enums) != "repositories"], function(x) {
    length(x) <- max(lengths(enums))
    x
  })
)

## Create instructions
instructions <- c(
  "Please read the instructions carefully to avoid submission errors",
  "",
  "Aliases",
  "This sheet is not part of an actual submission, it contains placeholders that allow for internal linking of Samples, Experiments, Runs, Analyses and Datasets, so they could be translated to provisional or accession IDs during submission.",
  "Please fill this sheet first, creating unique placeholder ID for each Sample, Experiment, Run, Analysiss and Dataset that you are submitting.",
  "Placeholders for Samples are the actual sample names.",
  "This sheet is not a rectangular dataframe, just an enumeration of individual items. See image below for example.",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "Files",
  "Names of raw files for the submission, if the files were encrypted and their names differ only in the c4gh extension, there is no need to add it, it will be appended during the submission process.",
  "If the files have been placed in the sub-directory of EGA Inbox, you can indicate it in the  'EGA Inbox Relative Path' column.",
  "Please fill in this sheet after the Aliases as other sheets that require file names will read values from this sheet.",
  "",
  "Analysis Files",
  "Names of analysis files for the submission, if the files were encrypted and their names differ only in the c4gh extension, there is no need to add it, it will be appended during the submission process.",
  "If the files have been placed in the sub-directory of EGA Inbox, you can indicate it in the  'EGA Inbox Relative Path' column.",
  "Please fill in this sheet after the Aliases as other sheets that require file names will read values from this sheet.",
  "If no analyses files are provided, leave the fields empty.",
  "",
  "Submission",
  "The top level item that encompasses all the other. It will receive an Accession ID once completed that you can reference in your manuscript.",
  "",
  "Studies",
  "Study description is expected to be a 3-5 sentences definition of the project with some background, goals, and details.",
  "One submission can have one or more studies.",
  "Does not have to be the same as the EGA submission name.",
  "",
  "Samples",
  "Summary of all biological samples linked to the submission. Please create a duplicate of the first row if you wish to add more samples.",
  "Samples need an alias which will be used to link files to samples.",
  "",
  "Experiments",
  "Here you have to specify how the experiment was done (Instrument model, design name (e.g. 'TruSeq Rna-Seq'), library type, ...).",
  "You could create more experiments if you have different data type in to the same study/EGA submission (e.g. ATAC-seq and RNA-seq).",
  "One submission can have one or more experiments.",
  "Platform: The platform in sequencing refers to the specific technology or machine used to perform sequencing (e.g. Illumina Sequencing, Pacific Biosciences (PacBio) Sequencing, Oxford Nanopore Technologies (ONT) Sequencing, …)",
  "Instrument mode: The sequencing model refers to the specific instrument version within a platform family (e.g., Illumina NovaSeq 6000, PacBio Sequel II, ...) tailored to different scales, throughput levels, and application types. Each platform offers several models designed to address a range of project needs.  ",
  "Library Selection: Library Selection in sequencing refers to the process of enriching or selecting specific fragments of DNA or RNA that are most relevant for sequencing from a larger pool of nucleic acids. This step is crucial because it determines the portion of the genome, transcriptome, or other DNA/RNA content that will be represented in the final sequencing output.",
  "",
  "Runs",
  "Runs connect samples to files and experiment. Please create a duplicate of the first row if you wish to add more runs.",
  "Files cannot be reused among the different Runs. Each file can only be associated to one Run.",
  "",
  "Analyses",
  "Analyses gather processed files, for example BAM or VCF files including mapping info. They linked to one study and to one/multiple samples, files and experiments.",
  "Currently this is the only sheet/entry that is optional for the EGA submission. If you don't want to submit any analyses, delete the entries in the Alias sheet. There is not need to delete 'Analysis Files' and 'Analyses' sheets, you can leave them empty.",
  "Analysis files cannot be reused among the different Analyses",
  "SEQUENCE VARIATION Analysis type must have 'Chromosomes groups' or 'Chromosomes' specified",
  "SAMPLE PHENOTYPE Analysis type cannot have 'Experiment Types', 'Genome ID', 'Chromosome groups' and 'Chromosomes' specified",
  "Selected 'Chromosome groups' and 'Chromosomes' must match 'Genome ID' (and possibly some other hidden requirement), this is not entirely clear how it is decided, spedifying 'Chromosomes' from group 3 worked for GRCh38.p14",
  "",
  "Datasets",
  "Here you describe your dataset(s).",
  "For each dataset, you need to link to a policy.",
  "Dataset description is expected to be a 3-4 sentences (at least 50 characters), definition of the dataset content, including sample number and details, file type, and technology/experimentation used.",
  "",
  "Collaborators, Repositories, Extra Attributes",
  "These fields are present in other sheets, but for submission they are formatted as key value pairs. ",
  "If you wish to add any of these to your submission, please create an appropriate entry in one of these 3 sheets and you will be able to select in from a dropdown menu afterwards.",
  "",
  "Additional Instructions",
  "Fill in the Samples and Runs by copying the first empty row below, this will preserve the fields where you need to choose multiple values",
  "For Studies, Experiments and Datasets, if multiple tables are needed, copy the whole table to the right (see image below)",
  "Fields marked with * are mandatory",
  "Values for fields with red triangle in the corner need to be chosen from available values",
  "Fields with light green background accept multiple values separated by ; (semicolon)",
  "Fields with light blue background can be present in the table multiple times and can be copied and pasted on the rows below if necessary",
  "Do not delete rows and columns that you don't fill in, just leave them empty"
)

instructions_headers = c(
  "Aliases", "Files", "Analysis Files", "Submission", "Studies", "Samples",
  "Experiments", "Runs", "Analyses", "Datasets",
  "Collaborators, Repositories, Extra Attributes", "Additional Instructions"
)

## Create data for worksheets
data_list <- list(
  "Instructions" = as.data.frame(instructions),
  "Aliases" = data.frame(
    X1 = c("Studies", "Study1"),
    X2 = c("Experiments", "Experiment1"),
    X3 = c("Datasets", "Dataset1"),
    X4 = c("Samples", "Sample1"),
    X5 = c("Runs", "Run1"),
    X6 = c("Analyses", "Analysis1")
  ),
  "Files" = data.frame(X1 = "* File", X2 = "EGA Inbox Relative Path"),
  "Analysis Files" = data.frame(X1 = "* File", X2 = "EGA Inbox Relative Path"),
  "Submission" = data.frame(X1 = c("* Title", "Description", "Collaborators")),
  "Studies" = data.frame(
    X1 = c(
      "* Study", "* Title", "* Description", "* Study Type", "Pubmed Ids",
      "Custom Tags", "Repositories", "Extra Attributes"
    ),
    X2 = c("Study1", NA, NA, NA, NA, NA, NA, NA)
  ),
  "Samples" = data.frame(
    X1 = c("* Alias", "Sample1"),
    X2 = c("* Phenotype", NA),
    X3 = c("Title", NA),
    X4 = c("Description", NA),
    X5 = c("* Biological Sex", NA),
    X6 = c("* Subject Id", NA),
    X7 = c("Biosample Id", NA),
    X8 = c("Case Control", NA),
    X9 = c("Organism Part", NA),
    X10 = c("Cell Line", NA),
    X11 = c("Extra Attributes", NA)
  ),
  "Experiments" = data.frame(
    X1 = c(
      "* Study", "* Experiment", "* Design Description",
      "* Library Selection", "Library Name", "Library Construction Protocol",
      "Paired Nominal Length", "Paired Nominal Sdev", "* Instrument Model Id",
      "* Library Layout", "* Library Strategy", "* Library Source",
      "Extra Attributes"
    ),
    X2 = c("Study1", "Experiment1", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  ),
  "Runs" = data.frame(
    X1 = c("* Run", "Run1"),
    X2 = c("* Experiment", "Experiment1"),
    X3 = c("* Run File Type", "fastq--One/Two Fastq"),
    X4 = c("* Alias", "Sample1"),
    X5 = c("Extra Attributes", NA),
    X6 = c("* Files", NA),
    X7 = c("Files", NA)
  ),
  "Analyses" = data.frame(
    X1 = c(
      "* Study", "* Analysis", "* Title", "* Description",
      "* Analysis Type", "Experiment Types", "Genome Id", "Platform",
      "Chromosome groups", "Chromosomes", "Extra Attributes", "Experiments",
      "* Files", "* Samples"
    ),
    X2 = c(
      "Study1", "Analysis1", NA, NA, NA, NA, NA, NA, NA, NA, NA, "Experiment1",
      NA, "Sample1"
    )
  ),
  "Datasets" = data.frame(
    X1 = c(
      "* Dataset", "* Title", "* Description",
      "* Policy Accession ID", "* Dataset Types", "Extra Attributes",
      "Runs", "Analyses"
    ),
    X2 = c("Dataset1", NA, NA, NA, NA, NA, "Run1", NA)
  ),
  "Collaborators" = data.frame(
    X1 = "* ID", X2 = "* Access Type", X3 = "Comment"
  ),
  "Repositories" = data.frame(
    X1 = "* Repository ID", X2 = "* URL", X3 = "* Label"
  ),
  "Extra Attributes" = data.frame(
    X1 = "* Tag", X2 = "* Value", X3 = "Unit"
  ),
  "Select Input Data" = enums_df
)

# Workbook ---------------------------------------------------------------------
sheet_names <- names(data_list)

# Create workbook
wb <- createWorkbook()

# Tab colors
tab_col <- list(
  "Instructions" = "white",
  "Aliases" = "burlywood4",
  "Files" = "palegreen1",
  "Analysis Files" = "green4",
  "Submission" = "firebrick3",
  "Studies" = "steelblue3",
  "Samples" = "gold",
  "Experiments" = "darkorange2",
  "Runs" = "mediumorchid1",
  "Analyses" = "turquoise1",
  "Datasets" = "black",
  "Collaborators" = "grey80",
  "Repositories" = "grey80",
  "Extra Attributes" = "grey80",
  "Select Input Data" = NULL
)

# Hidden sheets
hidden_sheets <- c("Select Input Data")

# Write each sheet's data
for (s in sheet_names) {
  addWorksheet(
    wb, s,
    tabColour = tab_col[[s]],
    visible = if (s %in% hidden_sheets) "hidden" else "visible"
  )
  if (!is.null(data_list[[s]]) && ncol(data_list[[s]]) > 0) {
    writeData(
      wb, s, data_list[[s]],
      startRow = 1, startCol = 1, withFilter = FALSE, colNames = FALSE
    )
  }
}

# Addtional data ---------------------------------------------------------------
input_data_target <- function(enums, header) {
  cl <- LETTERS[match(header, names(enums))]
  sprintf(
    "'Select Input Data'!$%s%d:$%s%d",
    cl, 1, cl, length(enums[[header]])
  )
}

# 1) Dropdowns
# Sheet - Row - Column - Target
dropdown_data <- list(
  list("Submission", 3, 2, "'Collaborators'!$A$2:$A$100"),
  list("Studies", 1, 2, "'Aliases'!$A$2:$A$100"),
  list("Studies", 7, 2, "'Repositories'!$A$2:$A$100"),
  list("Studies", 8, 2, "'Extra Attributes'!$A$2:$A$100"),
  list("Studies", 4, 2, input_data_target(enums, "study_types")),
  list("Samples", 2, 1, "'Aliases'!$D$2:$D$1000"),
  list("Samples", 2, 11, "'Extra Attributes'!$A$2:$A$100"),
  list("Samples", 2, 5, input_data_target(enums, "biological_sex")),
  list("Samples", 2, 8, input_data_target(enums, "case_controls")),
  list("Experiments", 1, 2, "'Aliases'!$A$2:$A$100"),
  list("Experiments", 2, 2, "'Aliases'!$B$2:$B$100"),
  list("Experiments", 13, 2, "'Extra Attributes'!$A$2:$A$100"),
  list("Experiments", 4, 2, input_data_target(enums, "library_selections")),
  list("Experiments", 9, 2, input_data_target(enums, "platform_models")),
  list("Experiments", 10, 2, input_data_target(enums, "library_layouts")),
  list("Experiments", 11, 2, input_data_target(enums, "library_strategies")),
  list("Experiments", 12, 2, input_data_target(enums, "library_sources")),
  list("Runs", 2, 1, "'Aliases'!$E$2:$E$1000"),
  list("Runs", 2, 2, "'Aliases'!$B$2:$B$100"),
  list("Runs", 2, 4, "'Aliases'!$D$2:$D$1000"),
  list("Runs", 2, 5, "'Extra Attributes'!$A$2:$A$100"),
  list("Runs", 2, 6, "'Files'!$A$2:$A$1000"),
  list("Runs", 2, 7, "'Files'!$A$2:$A$1000"),
  list("Runs", 2, 3, input_data_target(enums, "run_file_types")),
  list("Analyses", 1, 2, "'Aliases'!$A$2:$A$100"),
  list("Analyses", 2, 2, "'Aliases'!$F$2:$F$100"),
  list("Analyses", 11, 2, "'Extra Attributes'!$A$2:$A$100"),
  list("Analyses", 12, 2, "'Aliases'!$B$2:$B$100"),
  list("Analyses", 13, 2, "'Analysis Files'!$A$2:$A$1000"),
  list("Analyses", 14, 2, "'Aliases'!$D$2:$D$1000"),
  list("Analyses", 5, 2, input_data_target(enums, "analysis_types")),
  list("Analyses", 6, 2, input_data_target(enums, "experiment_types")),
  list("Analyses", 7, 2, input_data_target(enums, "genomes")),
  list("Analyses", 10, 2, input_data_target(enums, "chromosomes")),
  list("Datasets", 1, 2, "'Aliases'!$C$2:$C$100"),
  list("Datasets", 6, 2, "'Extra Attributes'!$A$2:$A$100"),
  list("Datasets", 7, 2, "'Aliases'!$D$2:$D$1000"),
  list("Datasets", 8, 2, "'Aliases'!$F$2:$F$100"),
  list("Datasets", 5, 2, input_data_target(enums, "dataset_types"))
)

for (x in dropdown_data) {
  dataValidation(
    wb, x[[1]],
    rows = as.integer(x[[2]]), cols = as.integer(x[[3]]),
    type = "list", value = x[[4]]
  )
}

# Extra for chromosome groups, this is my own addition, not present in EGA
dataValidation(
  wb, "Analyses",
  rows = 9, cols = 2,
  type = "list", value = c('"1,2,3"')
)

# 2) Comments
for (x in dropdown_data) {
  writeComment(
    wb, x[[1]],
    row = as.integer(x[[2]]), col = as.integer(x[[3]]),
    comment = createComment(
      "Select from drop-down list",
      style = createStyle(fontSize = 12, textDecoration = "bold"),
      visible = FALSE
    )
  )
}

# Extra for chromosome groups, this is my own addition, not present in EGA
writeComment(
  wb, "Analyses",
  row = 9, col = 2,
  comment = createComment(
    "Select from drop-down list",
    style = createStyle(fontSize = 12, textDecoration = "bold"),
    visible = FALSE
  )
)

# Styling ----------------------------------------------------------------------
modifyBaseFont(wb, fontSize = 14, fontName = "Arial")
setWindowSize(wb, windowWidth = 20000, windowHeight = 12000)

# 3) Cell fills / Styles:
# Sheet - Fill
rowtable_colors <- c(
  "Aliases" = "burlywood4",
  "Files" = "palegreen1",
  "Analysis Files" = "green4",
  "Samples" = "gold",
  "Runs" = "mediumorchid1",
  "Collaborators" = "gray80",
  "Repositories" = "gray80",
  "Extra Attributes" = "gray80"
)

for (sheet in names(rowtable_colors)) {
  addStyle(
    wb, sheet,
    createStyle(fgFill = rowtable_colors[sheet], textDecoration = "bold"),
    rows = 1, cols = seq_len(ncol(data_list[[sheet]])), stack = TRUE
  )
}

green_bg <- createStyle(fgFill = "darkseagreen2")
blue_bg <- createStyle(fgFill = "lightskyblue2")

# Sheet - Row - Column - Fill
cell_bgs <- list(
  list("Instructions", 74, 1, green_bg),
  list("Instructions", 75, 1, blue_bg),
  list("Submission", 3, 1, blue_bg),
  list("Studies", 5, 1, green_bg),
  list("Studies", 6, 1, green_bg),
  list("Studies", 7, 1, blue_bg),
  list("Studies", 8, 1, blue_bg),
  list("Experiments", 13, 1, blue_bg),
  list("Analyses", 6, 1, blue_bg),
  list("Analyses", 10, 1, blue_bg),
  list("Analyses", 11, 1, blue_bg),
  list("Analyses", 12, 1, blue_bg),
  list("Analyses", 13, 1, blue_bg),
  list("Analyses", 14, 1, blue_bg),
  list("Datasets", 5, 1, blue_bg),
  list("Datasets", 6, 1, blue_bg),
  list("Datasets", 7, 1, blue_bg),
  list("Datasets", 8, 1, blue_bg)
)

for (x in cell_bgs) {
  addStyle(
    wb, x[[1]],
    x[[4]],
    rows = x[[2]], cols = x[[3]], stack = TRUE
  )
}

# Additional Bold headers
bold_font <- createStyle(textDecoration = "bold")

addStyle(
  wb, "Instructions",
  createStyle(textDecoration = "bold", fontSize = 16, fontColour = "red"),
  rows = 1, cols = 1, stack = TRUE
)

addStyle(
  wb, "Instructions", createStyle(textDecoration = "bold", fontSize = 16),
  rows = sapply(
    instructions_headers, \(x) grep(paste0("^", x, "$"), instructions)
  ),
  cols = 1, stack = TRUE
)

coltable_sheets <- c(
  "Submission", "Studies", "Experiments", "Analyses", "Datasets"
)

for (sheet in coltable_sheets) {
  addStyle(
    wb, sheet, bold_font,
    rows = seq_len(nrow(data_list[[sheet]])), cols = 1, stack = TRUE
  )
}

# 4) Column widths,
# Sheet - Column - Width
widths_list <- list(
  list("Aliases", 1, 20), list("Aliases", 2, 20), list("Aliases", 3, 20),
  list("Aliases", 4, 40), list("Aliases", 5, 20), list("Aliases", 6, 20),
  list("Files", 1, 80), list("Files", 2, 80),
  list("Analysis Files", 1, 80), list("Analysis Files", 2, 80),
  list("Submission", 1, 25), list("Submission", 2, 60),
  list("Studies", 1, 25), list("Studies", 2, 60),
  list("Samples", 1, 40), list("Samples", 2, 15), list("Samples", 3, 15),
  list("Samples", 4, 15), list("Samples", 5, 15), list("Samples", 6, 15),
  list("Samples", 7, 15), list("Samples", 8, 15), list("Samples", 9, 15),
  list("Samples", 10, 15), list("Samples", 11, 15),
  list("Experiments", 1, 30), list("Experiments", 2, 60),
  list("Runs", 1, 15), list("Runs", 2, 20), list("Runs", 3, 20),
  list("Runs", 4, 40), list("Runs", 5, 20), list("Runs", 6, 80),
  list("Runs", 7, 80),
  list("Analyses", 1, 25), list("Analyses", 2, 60),
  list("Datasets", 1, 25), list("Datasets", 2, 60),
  list("Collaborators", 1, 20), list("Collaborators", 2, 20),
  list("Collaborators", 3, 20),
  list("Repositories", 1, 20), list("Repositories", 2, 20),
  list("Repositories", 3, 20),
  list("Extra Attributes", 1, 20), list("Extra Attributes", 2, 20),
  list("Extra Attributes", 3, 20)
)

for (x in widths_list) {
  setColWidths(
    wb, x[[1]],
    cols = x[[2]], widths = x[[3]]
  )
}

# 5) Borders
# No borders in Instructions sheet
for (x in setdiff(names(data_list), "Instructions")) {
  addStyle(
    wb, x, createStyle(border = "TopBottomLeftRight"),
    rows = seq_len(nrow(data_list[[x]])), cols = seq_len(ncol(data_list[[x]])),
    gridExpand = TRUE, stack = TRUE
  )
}

# Extra border to submission
addStyle(
  wb, "Submission", createStyle(border = "TopBottomLeftRight"),
  rows = seq_len(nrow(data_list[["Submission"]])), cols = 2,
  gridExpand = TRUE, stack = TRUE
)

# 6) Insert images
insertImage(
  wb, "Instructions", system.file("extdata/aliases.png", package = "Rega"),
  startRow = 8, units = "px", dpi = 144, width = 1302, height = 316
)

insertImage(
  wb, "Instructions", system.file("extdata/multi_table.png", package = "Rega"),
  startRow = length(instructions) + 2,
  units = "px", dpi = 144, width = 2100, height = 304
)

# Save workbook ----------------------------------------------------------------
saveWorkbook(
  wb,
  file.path("inst", "extdata", "ega_full_template_v3.xlsx"),
  overwrite = TRUE
)

