bx = list(
  stats = {
    q3 = qnorm(.75)
    hw = c(q3, 1.5 * 2 * q3)
    c(-1 * rev(hw), 0, hw) |> as.matrix()
  },
  n = 1,
  conf = c(-1, 1) |> as.matrix(),
  out = qnorm(.995),
  group = 1,
  names = "1"
)

png('outlier.png')
  bxp(bx,
      ann = FALSE,
      axes = FALSE,
      border = 'orange2',
      lwd = 6,
      outcex = 4,
      whisklty = 'solid'
  )
dev.off()
