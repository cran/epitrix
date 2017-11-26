context("Testing name hashing")

test_that("Hashing outputs as expected", {
    skip_on_cran()

    expect_equal_to_reference(hash_names(c("sweet", "baby", "jesus")),
                              file = "rds/hashref_1.rds")

    expect_equal_to_reference(hash_names(NA),
                              file = "rds/hashref_na.rds")

    expect_equal(nrow(hash_names(NULL)), 0L)

    expect_identical(hash_names("foo BaR "), hash_names("foobar"))

    expect_identical(hash_names("Pétèr and Stévën"),
                     hash_names("peter and steven"))

    expect_identical(hash_names("Pïôtr and ÿgòr"),
                     hash_names("píotr and YGÓR"))

    expect_is(hash_names("klsdfsdndsnjs"), "data.frame")
    expect_is(hash_names("klsdfsdndsnjs", full = FALSE), "character")
    expect_equal(
      nchar(hash_names("klsdfsdndsnjs", size = 6, full = FALSE)),
      6L)
    expect_equal(
      nchar(hash_names("klsdfsdndsnjs", size = 10, full = FALSE)),
      10L)

})