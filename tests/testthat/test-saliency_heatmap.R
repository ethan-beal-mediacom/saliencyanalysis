context("test-saliency_heatmap")

saliency_heatmap(saliency_files_directory = "test-saliency-files",
                 raw_image_directory = "test-raw-image-files",
                 output_directory = "test-output-files", output_only = TRUE)

output_files = list.files("test-output-files/4_final_output", full.names =TRUE)

test_that("test that output_only works", {
  expect_equal(length(output_files), 3)
})


