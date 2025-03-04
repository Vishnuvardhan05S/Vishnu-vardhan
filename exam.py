# Create dataset
ball_data <- data.frame(
  Distance = factor(rep(c("15 yards", "30 yards", "45 yards"), each = 4)),
  Resting_Distance = c(5, 3, 6, 5, 16, 17, 16, 13, 15, 18, 15, 16)
)

# Boxplot for visualization
boxplot(Resting_Distance ~ Distance, data = ball_data,
        main = "Ball Resting Distance by Player Distance",
        xlab = "Player Distance", ylab = "Ball Resting Distance (feet)",
        col = c("red", "blue", "green"))

# Conduct One-Way ANOVA
anova_result <- aov(Resting_Distance ~ Distance, data = ball_data)
print(summary(anova_result))

# Check Assumptions
qqnorm(residuals(anova_result))
qqline(residuals(anova_result), col = "red")

# Levene's Test for Equal Variance
leveneTest(Resting_Distance ~ Distance, data = ball_data)

# Tukey's Post Hoc Test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)
plot(tukey_result)
