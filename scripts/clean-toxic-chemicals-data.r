

library(stringr)

chems = read.csv(
  "/Users/Gaston/Documents/Insight/data/chemicals.csv",
  header = TRUE,
  stringsAsFactors = FALSE)

str(chems)


# clean chemical
chemical = str_trim(chems$Chemical)
chemical = gsub(
  pattern = "\\(internal use\\)",
  replacement = "",
  x = chemical)
chemical = tolower(chemical)

# clean toxicity
toxicity = str_trim(chems$Toxicity)
toxicity = gsub("\\s+", "", toxicity)

# clean mechanism
mechanism = str_trim(chems$Mechanism)
typo_mechanism = c(
  which(mechanism == "SQE - developmental     FR - female, male"),
  which(mechanism == "SQE - developmental    FR - female, male")
)
mechanism[typo_mechanism] = "SQE"

# clean CAS
cas = str_trim(chems$CAS)
cas = gsub("---", "", cas)
cas = gsub("\\s+", "", cas)

# clean
nsrl_madl = chems$NSRL_MADL
nsrl_madl = gsub("\\s+", "", nsrl_madl)
nsrl_madl


# clean data frame
clean_chems = data.frame(
  chemical = chemical,
  toxicity = toxicity,
  mechanism = mechanism,
  cas = cas,
  listed = chems$Listed,
  nsrl_madl,
  stringsAsFactors = FALSE)

str(clean_chems)


# resave
write.table(
  x = clean_chems,
  "/Users/Gaston/Documents/Insight/data/toxic_chemicals_california.csv",
  row.names = FALSE,
  sep = ",")


# test everything works
chems = read.csv(
  "/Users/Gaston/Documents/Insight/data/toxic_chemicals_california.csv",
  header = TRUE,
  stringsAsFactors = FALSE)

# longest chemical name
a = sapply(chems$chemical, nchar)
which(a == max(a))
