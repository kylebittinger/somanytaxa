example_input <- c(
  "k__Bacteria; p__Firmicutes; c__Clostridia; o__Clostridiales; f__Lachnospiraceae",
  "k__Bacteria; p__Proteobacteria; c__Gammaproteobacteria; o__Pasteurellales; f__Pasteurellaceae; g__Haemophilus; s__parainfluenzae",
  "k__Bacteria; p__TM7; c__TM7-3; o__CW040; f__; g__; s__")

example_lineage_df <- tibble::tibble(
  Kingdom = c("Bacteria", "Bacteria", "Bacteria"),
  Phylum = c("Firmicutes", "Proteobacteria", "TM7"),
  Class = c("Clostridia", "Gammaproteobacteria", "TM7-3"),
  Order = c("Clostridiales", "Pasteurellales", "CW040"),
  Family = c("Lachnospiraceae", "Pasteurellaceae", NA),
  Genus = c(NA, "Haemophilus", NA),
  Species = c(NA, "parainfluenzae", NA))

test_that("split_lineage works", {
  expect_equal(split_assignments(example_input), example_lineage_df)
})

test_that("split_lineage works without removing prefix", {
  expected <- tibble::tibble(
    Kingdom = c("k__Bacteria", "k__Bacteria", "k__Bacteria"),
    Phylum = c("p__Firmicutes", "p__Proteobacteria", "p__TM7"),
    Class = c("c__Clostridia", "c__Gammaproteobacteria", "c__TM7-3"),
    Order = c("o__Clostridiales", "o__Pasteurellales", "o__CW040"),
    Family = c("f__Lachnospiraceae", "f__Pasteurellaceae", "f__"),
    Genus = c(NA, "g__Haemophilus", "g__"),
    Species = c(NA, "s__parainfluenzae", "s__"))
  expect_equal(
    split_assignments(example_input, remove = NULL),
    expected)
})

test_that("format_assignments works", {
  expected <- c(
    "Firmicutes - Lachnospiraceae (unclassified)",
    "Proteobacteria - Haemophilus",
    "TM7 - CW040 (unclassified)")
  expect_equal(format_assignments(example_lineage_df), expected)
})

