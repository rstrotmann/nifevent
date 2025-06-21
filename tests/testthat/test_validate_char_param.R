test_that("validate_char_param works correctly", {
  # Test valid single character string (default behavior)
  expect_silent(validate_char_param("test", "test_param"))
  expect_silent(validate_char_param("", "test_param", allow_empty = TRUE))

  # Test allow_null = TRUE
  expect_silent(validate_char_param(NULL, "test_param", allow_null = TRUE))

  # Test allow_multiple = TRUE (all elements must be non-empty, non-NA)
  expect_silent(validate_char_param(c("a", "b", "c"), "test_param", allow_multiple = TRUE))

  # Test combination of allow_null and allow_empty
  expect_silent(validate_char_param(NULL, "test_param", allow_null = TRUE, allow_empty = TRUE))
  expect_silent(validate_char_param("", "test_param", allow_null = TRUE, allow_empty = TRUE))

  # Test combination of allow_null and allow_multiple
  expect_silent(validate_char_param(NULL, "test_param", allow_null = TRUE, allow_multiple = TRUE))
  expect_silent(validate_char_param(c("a", "b"), "test_param", allow_null = TRUE, allow_multiple = TRUE))

  # Test combination of allow_empty and allow_multiple
  expect_silent(validate_char_param(c("", "b"), "test_param", allow_empty = TRUE, allow_multiple = TRUE))
  expect_silent(validate_char_param(character(0), "test_param", allow_empty = TRUE, allow_multiple = TRUE))

  # Test all flags enabled
  expect_silent(validate_char_param(NULL, "test_param", allow_null = TRUE, allow_empty = TRUE, allow_multiple = TRUE))
  expect_silent(validate_char_param(c("", "b"), "test_param", allow_null = TRUE, allow_empty = TRUE, allow_multiple = TRUE))
})


test_that("validate_char_param throws errors for invalid inputs", {
  # Test NULL when not allowed
  expect_error(
    validate_char_param(NULL, "test_param"),
    "test_param must be a single character string"
  )

  # Test non-character inputs
  expect_error(
    validate_char_param(123, "test_param"),
    "test_param must be a single character string"
  )
  expect_error(
    validate_char_param(TRUE, "test_param"),
    "test_param must be a single character string"
  )
  expect_error(
    validate_char_param(list("a"), "test_param"),
    "test_param must be a single character string"
  )

  # Test multiple character strings when not allowed
  expect_error(
    validate_char_param(c("a", "b"), "test_param"),
    "test_param must be a single character string"
  )

  # Test empty string when not allowed
  expect_error(
    validate_char_param("", "test_param"),
    "test_param must be a non-empty character string"
  )

  # Test empty string with allow_null but not allow_empty
  expect_error(
    validate_char_param("", "test_param", allow_null = TRUE),
    "test_param must be a non-empty character string"
  )

  # Test multiple strings with allow_null but not allow_multiple
  expect_error(
    validate_char_param(c("a", "b"), "test_param", allow_null = TRUE),
    "test_param must be a single character string"
  )

  # Test NA_character_ (should always error)
  expect_error(
    validate_char_param(NA_character_, "test_param"),
    "test_param must not contain NA"
  )
  expect_error(
    validate_char_param(NA_character_, "test_param", allow_null = TRUE),
    "test_param must not contain NA"
  )
  expect_error(
    validate_char_param(NA_character_, "test_param", allow_multiple = TRUE),
    "test_param must not contain NA"
  )
  # Test vector with NA (should always error)
  expect_error(
    validate_char_param(c("a", NA_character_), "test_param", allow_multiple = TRUE),
    "test_param must not contain NA"
  )
})


test_that("validate_char_param handles edge cases correctly", {
  # Test single space character
  expect_silent(validate_char_param(" ", "test_param"))
  expect_silent(validate_char_param(" ", "test_param", allow_empty = TRUE))

  # Test special characters
  expect_silent(validate_char_param("!@#$%^&*()", "test_param"))
  expect_silent(validate_char_param("", "test_param", allow_empty = TRUE))

  # Test unicode characters
  expect_silent(validate_char_param("café", "test_param"))
  expect_silent(validate_char_param("测试", "test_param"))
})


test_that("validate_char_param returns invisible NULL", {
  result <- validate_char_param("test", "test_param")
  expect_null(result)
  expect_true(inherits(result, "NULL"))

  result <- validate_char_param(NULL, "test_param", allow_null = TRUE)
  expect_null(result)
  expect_true(inherits(result, "NULL"))
})


test_that("validate_char_param error messages are correct", {
  # Test error message format
  expect_error(
    validate_char_param(NULL, "my_parameter"),
    "my_parameter must be a single character string"
  )

  expect_error(
    validate_char_param("", "empty_param"),
    "empty_param must be a non-empty character string"
  )

  expect_error(
    validate_char_param(c("a", "b"), "multi_param"),
    "multi_param must be a single character string"
  )
})

