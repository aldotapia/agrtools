test_that("kc_curve follows the four FAO-56 stages", {
  kc = kc_curve(c(0, 1, 30, 50, 70, 100, 115, 130, 131), 30, 40, 30, 30,
                kc_ini = 0.3, kc_mid = 1.2, kc_end = 0.6)
  expect_equal(kc, c(NA, 0.3, 0.3, 0.75, 1.2, 1.2, 0.9, 0.6, NA))
})

test_that("kc_adjust matches eq. 62", {
  expect_equal(kc_adjust(1.2, u2 = 2, rh_min = 45, h = 2), 1.2)
  expect_equal(kc_adjust(1.2, u2 = 3, rh_min = 30, h = 3), 1.2 + 0.04 + 0.06)
  expect_equal(kc_adjust(0.3, u2 = 5, rh_min = 20, h = 3), 0.3)
})

test_that("etc multiplies its terms", {
  expect_equal(etc(5, 1.2, 0.5), 3)
})

test_that("water_balance conserves water and applies stress", {
  n = 60
  et0 = rep(5, n)
  kc = rep(1, n)
  rain = c(rep(0, 20), 80, rep(0, n - 21))
  wb = water_balance(et0, kc, rain, theta_fc = 0.3, theta_wp = 0.15, zr = 0.5)
  expect_equal(wb$taw[1], 75)
  expect_equal(wb$raw[1], 37.5)
  # depletion grows 5 mm/day until RAW, then stress reduces ETc
  expect_equal(wb$dr[1:7], seq(5, 35, 5))
  expect_true(all(wb$ks[wb$dr > 40] < 1))
  expect_equal(wb$ks[1:8], rep(1, 8))
  # balance: change in depletion = ETc - rain - irrigation + percolation
  expect_equal(tail(wb$dr, 1),
               sum(wb$etc_adj) - sum(rain) - sum(wb$irrigation) + sum(wb$dp))
  expect_gt(wb$dp[21], 0)

  auto = water_balance(et0, kc, theta_fc = 0.3, theta_wp = 0.15, zr = 0.5,
                       auto_irrigate = TRUE)
  expect_true(all(auto$ks == 1))
  expect_equal(sum(auto$irrigation), sum(auto$etc_adj) - tail(auto$dr, 1))
})
