all_data %>%
  ggplot(data = ., aes(x = DSCI.mean, y = ((RMA.drought.Payment.indemnity..US../RMA.drought.Liability..US..)), color = Year)) +
  geom_point(alpha = 0.4,size = 0.4)


all_data %>%
  ggplot(data = ., aes(x = DSCI.mean, y = RMA.drought.per.acre, color = Year)) +
  geom_point(alpha = 0.4,size = 0.4)


all_data %>%
  mutate(loss_cost = ((RMA.drought.Payment.indemnity..US../RMA.drought.Liability..US..)*100)) -> all_data

m1 <- lmerTest::lmer(data = all_data, formula = loss_cost ~ DSCI.sum + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.sum + (1|Order))

summary(m1)

interactions::interact_plot(model = m1, modx = DSCI.sum,modx.values = c(0, 610, 5000, 10000),
                            pred = soc_M_sl4_100m)

all_data %>%
  ggplot(data = ., aes(x = soc_M_sl4_100m, y = loss_cost, color = Order)) +
  geom_point(alpha = 0.4,size = 0.4)

