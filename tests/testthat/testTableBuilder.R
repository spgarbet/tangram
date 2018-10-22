context("Table Builder")

test_that("Flattening arguments works for embedded vectors of numbers",
{
  fl <- args_flatten(NA, c(1,2,3), NA)

  expect_equal(length(fl), 5)
  expect_true(is.na(fl[[1]]))
  expect_equal(fl[[2]], 1)
  expect_equal(fl[[3]], 2)
  expect_equal(fl[[4]], 3)
  expect_true(is.na(fl[[5]]))
})

test_that("Flattening arguments works for basic list",
{
  fl <- args_flatten(NA, list(1, 2, 3), NA)

  expect_equal(length(fl), 5)
  expect_true(is.na(fl[[1]]))
  expect_equal(fl[[2]], 1)
  expect_equal(fl[[3]], 2)
  expect_equal(fl[[4]], 3)
  expect_true(is.na(fl[[5]]))
})

test_that("Flattening arguments does not flatten cells",
{
  fl <- args_flatten(cell_label("abc", units="alpha"))

  expect_equal(length(fl), 1)
})

test_that("row_header creates a new header with class cell_header in elements for first call", {
  tb <- tangram(1,1) %>%
        row_header(NA, c(1,2,3))

  x <- attr(tb, "row_header")

  expect_equal(class(x), c("tangram", "list"))
  expect_equal(class(x[[1]][[1]]), c("cell_header", "cell_label", "cell", "logical"))

  expect_true(x[[1]][[2]] == 1)
  expect_equal(class(x[[1]][[2]]), c("cell_header", "cell_label", "cell", "numeric"))

  expect_true(x[[1]][[3]] == 2)

  expect_true(x[[1]][[4]] == 3)
})


test_that("col_header creates a new header with class cell_header in elements for first call", {
  tb <- tangram(1,1) %>%
        col_header("Jim", c(1,2,3))

  x <- attr(tb, "col_header")

  expect_equal(class(x), c("tangram", "list"))
  expect_true(x[[1]][[1]] == "Jim")
  expect_equal(class(x[[1]][[1]]), c("cell_header", "cell_label", "cell", "character"))

  expect_true(x[[1]][[2]] == 1)
  expect_equal(class(x[[1]][[2]]), c("cell_header", "cell_label", "cell", "numeric"))

  expect_true(x[[1]][[3]] == 2)

  expect_true(x[[1]][[4]] == 3)
})

test_that("row_header creates a new header with class cell_subheader in elements for later call", {
  tb <-tangram(1,1) %>%
        row_header("First", NA) %>%
        row_header("Second", hmisc_iqr(rnorm(20)))

  x <- attr(tb, "row_header")

  expect_equal(class(x), c("tangram", "list"))
  expect_equal(length(x), 2)
  expect_equal(length(x[[1]]), 2)
  expect_equal(length(x[[2]]), 2)

  expect_equal(class(x[[1]][[1]]), c("cell_header", "cell_label", "cell", "character"))
  expect_equal(class(x[[1]][[2]]), c("cell_header", "cell_label", "cell", "logical"))
  expect_equal(class(x[[2]][[1]]), c("cell_subheader", "cell_header", "cell_label", "cell", "character"))
  expect_equal(class(x[[2]][[2]]), c("cell_subheader", "cell_header", "cell_label", "cell", "character"))

})

test_that("col_header creates a new header with class cell_subheader in elements for later call", {
  tb <- tangram(1,1) %>%
        col_header("First", NA) %>%
        col_header("Second", hmisc_iqr(rnorm(20)))

  x <- attr(tb, "col_header")

  expect_equal(class(x), c("tangram", "list"))
  expect_equal(length(x), 2)
  expect_equal(length(x[[1]]), 2)
  expect_equal(length(x[[2]]), 2)

  expect_equal(class(x[[1]][[1]]), c("cell_header", "cell_label", "cell", "character"))
  expect_equal(class(x[[1]][[2]]), c("cell_header", "cell_label", "cell", "logical"))
  expect_equal(class(x[[2]][[1]]), c("cell_subheader", "cell_header", "cell_label", "cell", "character"))
  expect_equal(class(x[[2]][[2]]), c("cell_subheader", "cell_header", "cell_label", "cell", "character"))
})

test_that("New Table Builder returns an empty 1x1 table",
{
  tb <- tangram(1,1)

  expect_true(inherits(tb, "tangram"))
  expect_equal(length(tb), 1)
  expect_equal(length(tb[[1]]), 1)
  expect_equal(class(tb[[1]][[1]]), c("cell", "character"))
  expect_equal(attr(tb, 'row'), 1)
  expect_equal(attr(tb, 'col'), 1)
})

test_that("home moves the cursor to 1,1", {
  tb <- tangram(1,1) %>% cursor_down() %>% cursor_right() %>% home()

  expect_equal(attr(tb, 'row'), 1)
  expect_equal(attr(tb, 'col'), 1)
})

test_that("cursor_right moves the cursor 1 to the right", {
  tb <- tangram(1,1) %>% cursor_right()

  expect_equal(attr(tb, 'row'), 1)
  expect_equal(attr(tb, 'col'), 2)
})

test_that("cursor_right moves the cursor n to the right", {
  tb <- tangram(1,1) %>% cursor_right(23)

  expect_equal(attr(tb, 'row'), 1)
  expect_equal(attr(tb, 'col'), 24)

  # And allow for crazy negative usage
  tb <- tb %>% cursor_right(-2)
  expect_equal(attr(tb, 'col'), 22)
})

test_that("cursor_right errors when request to move beyond left most column via a negative value", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10)

  expect_error(cursor_right(tb, -10))
  expect_error(home(tb) %>% cursor_right(-1))
})

test_that("cursor_down moves the cursor 1 down", {
  tb <- tangram(1,1) %>% cursor_down()

  expect_equal(attr(tb, 'row'), 2)
  expect_equal(attr(tb, 'col'), 1)
})

test_that("cursor_down moves the cursor n down", {
  tb <- tangram(1,1) %>% cursor_down(23)

  expect_equal(attr(tb, 'row'), 24)
  expect_equal(attr(tb, 'col'), 1)

  # And allow for crazy negative usage
  tb <- tb %>% cursor_down(-2)
  expect_equal(attr(tb, 'row'), 22)
})

test_that("cursor_down errors when requested to move above top row via a negative value", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10)

  expect_error(cursor_down(tb, -10))
  expect_error(home(tb) %>% cursor_down(-1))
})


test_that("cursor_left moves the cursor 1 to the left", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10) %>% cursor_left()

  expect_equal(attr(tb, 'row'), 10)
  expect_equal(attr(tb, 'col'), 9)
})

test_that("cursor_left moves the cursor n to the left", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10) %>% cursor_left(5)

  expect_equal(attr(tb, 'row'), 10)
  expect_equal(attr(tb, 'col'), 5)
})

test_that("cursor_left errors when request to move beyond left most column", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10)

  expect_error(cursor_left(tb, 10))
  expect_error(home(tb) %>% cursor_left(1))
})

test_that("cursor_up moves the cursor 1 up", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10) %>% cursor_up()

  expect_equal(attr(tb, 'row'), 9)
  expect_equal(attr(tb, 'col'), 10)
})

test_that("cursor_up moves the cursor n up", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10) %>% cursor_up(5)

  expect_equal(attr(tb, 'row'), 5)
  expect_equal(attr(tb, 'col'), 10)
})

test_that("cursor_up errors when request to move beyond top most column", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10)

  expect_error(cursor_up(tb, 10))
  expect_error(home(tb) %>% cursor_up(1))
})

test_that("cursor_pos positions cursor correctly and doesn't allow negative values", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10)

  expect_equal(attr(tb, 'row'), 10)
  expect_equal(attr(tb, 'col'), 10)

  expect_error(cursor_pos(tb, -1, 10))
  expect_error(cursor_pos(tb, 10, -1))
})

test_that("carriage return goes to first column without advancing row", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10) %>% carriage_return()

  expect_equal(attr(tb, 'row'), 10)
  expect_equal(attr(tb, 'col'), 1)
})

test_that("line_feed moves the cursor 1 down", {
  tb <- tangram(1,1) %>% line_feed()

  expect_equal(attr(tb, 'row'), 2)
  expect_equal(attr(tb, 'col'), 1)
})

test_that("line_feed moves the cursor n down", {
  tb <- tangram(1,1) %>% line_feed(23)

  expect_equal(attr(tb, 'row'), 24)
  expect_equal(attr(tb, 'col'), 1)
})

test_that("new_line both moves down a line and returns to first column", {
  tb <- tangram(1,1) %>% cursor_pos(10, 10) %>% new_line()

  expect_equal(attr(tb, 'row'), 11)
  expect_equal(attr(tb, 'col'), 1)
})

test_that("new_row opens a new row at the bottom", {
   tb <- tangram(1,1) %>% cursor_right()

   tb[[1]] <- list("A", "B")
   tb[[3]] <- list("C")

   tb <- tb %>% new_row()

   expect_equal(attr(tb, 'row'), 4)
   expect_equal(attr(tb, 'col'), 1)
})

test_that("new_col opens a new col at right of the top most defined col", {
  tb <- tangram(1,1) %>% cursor_down(2)

  tb[[1]] <- list("A", "B")
  tb[[3]] <- list("C")

  tb <- tb %>% new_col()

  expect_equal(attr(tb, 'row'), 1)
  expect_equal(attr(tb, 'col'), 3)
})

test_that("table_builder_apply works over a vector", {
   tb <- tangram(1,1) %>%
         table_apply(1:3, FUN=function(tbl, x) {
           tbl %>% cursor_down(x) %>% cursor_right(x)
         })

  expect_equal(attr(tb, 'row'), 7)
  expect_equal(attr(tb, 'col'), 7)
})

test_that("write_cell writes to table with key info",
{
  tb   <- tangram(1,1) %>%
          write_cell(cell_n(2), subrow="S", subcol="T")
  cell <- tb[[1]][[1]]

  expect_true(inherits(cell, "cell_n"))
  expect_true(inherits(cell, "cell"))
  expect_equal(attr(tb, 'row'), 1)
  expect_equal(attr(tb, 'col'), 1)
  expect_equal(length(tb), 1)
  expect_equal(length(tb[[1]]), 1)
})

test_that("add_col will add a single column", {
  tb   <- tangram(1,1) %>%
          add_col(hmisc_iqr(rnorm(50), subrow="S", subcol="T"))

  expect_equal(attr(tb, 'row'), 1)
  expect_equal(attr(tb, 'col'), 2)
})

test_that("add_col will add multiple columns as cells", {
  tb   <- tangram(1,1) %>%
          add_col(hmisc_iqr(rnorm(50), subrow="S", subcol="T"),
            cell_n(4),
            hmisc_fraction(1,2), "", "1")

  expect_equal(attr(tb, 'row'), 1)
  expect_equal(attr(tb, 'col'), 6)
})

