test_that("download_gmd_csv works", {
  df <- download_gmd_csv(start_year = 2000, end_year = 2020)
  expect_true(is.data.frame(df))
  expect_true(all(df$year >= 2000 & df$year <= 2020))

  df_country <- download_gmd_csv(country = "United States")
  expect_true(all(df_country$countryname == "United States"))

  df_iso3 <- download_gmd_csv(ISO3 = "USA")
  expect_true(all(df_iso3$ISO3 == "USA"))
})
