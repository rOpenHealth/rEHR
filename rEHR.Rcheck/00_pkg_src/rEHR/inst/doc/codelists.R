## ----eval = FALSE--------------------------------------------------------
#  install.packages("rEHR")

## ----eval = FALSE--------------------------------------------------------
#  install.packages("devtools")
#  require(devtools)
#  install_github("rEHR", "rOpenHealth")
#  require(rEHR)

## ----eval = FALSE--------------------------------------------------------
#  # vectors of length > 1 are combined as a single AND expression
#  # "-" excludes that term from the search
#  def <- MedicalDefinition(terms = list("peripheral vascular disease", "peripheral gangrene", "-wrong answer",
#                        "intermittent claudication", "thromboangiitis obliterans",
#                        "thromboangiitis obliterans", "diabetic peripheral angiopathy",
#                        c("diabetes", "peripheral angiopathy"),  # combined as a single AND expression
#                        c("diabetes", "peripheral angiopathy"),
#                        c("buerger",  "disease presenile_gangrene"),
#                        "thromboangiitis obliterans",
#                        "-rubbish", # exclusion
#                        c("percutaneous_transluminal_angioplasty", "artery"),
#                        c("bypass", "iliac_artery"),
#                        c("bypass", "femoral_artery"),
#                        c("femoral_artery" , "occlusion"),
#                        c("popliteal_artery", "occlusion"),
#                        "dissecting_aortic_aneurysm", "peripheral_angiopathic_disease",
#                        "acrocyanosis", "acroparaesthesia", "erythrocyanosis",
#                        "erythromelalgia", "ABPI",
#                        c("ankle", "brachial"),
#                        c("ankle", "pressure"),
#                        c("left", "brachial"),
#                        c("left", "pressure"),
#                        c("right", "brachial"),
#                        c("right", "pressure")),
#           codes = list("G73"),
#           tests = NULL,
#           drugs = list("insulin", "diabet", "aspirin"))

## ----eval = FALSE--------------------------------------------------------
#  ## Using the example search definition provided with the package
#  def2 <- import_definitions(system.file("extdata", "example_search.csv", package = "rpcdsearch"))

## ----eval = FALSE--------------------------------------------------------
#  ## Use fileEncoding="latin1" to avoid any issues with non-ascii characters
#  medical_table <- read.delim("Lookups//medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
#  drug_table <- read.delim("Lookups/product.txt", fileEncoding="latin1", stringsAsFactors = FALSE)

## ----eval = FALSE--------------------------------------------------------
#  draft_lists <- build_definition_lists(def, medical_table = medical_table,drug_table = drug_table)

## ----eval = FALSE--------------------------------------------------------
#  out_file <- "def_searches.xlsx"
#  export_definition_search(draft_lists, out_file)

