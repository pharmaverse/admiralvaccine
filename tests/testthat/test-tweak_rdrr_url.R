test_that("Check tweak_rdrr_url", {
  html <- xml2::read_html('<a href="https://rdrr.io/pkg/admiral/man/convert_blanks_to_na.html"></a>')
  tweak_rdrr_url(html)
  links <- xml2::xml_find_all(html, ".//a")
  hrefs <- xml2::xml_attr(links, "href")
  expect_equal(xml2::url_parse(hrefs)$scheme, "https")
  expect_equal(hrefs, "https://pharmaverse.github.io/admiral/pre-release/reference/convert_blanks_to_na.html")
})
