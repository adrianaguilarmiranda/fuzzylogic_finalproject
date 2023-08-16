sets_options("universe", seq(1, 100, 1))

# Based on Risk is 0 to 100, so the variables will be weighed the same
# Low credit, high income_ratio, young age, are deemed higher risk
variables <- set(
  credit = fuzzy_partition(varnames = c(low = 60, okay= 30, good = 20), sd = 5.0),
  income_ratio = fuzzy_partition(varnames = c(small= 20, normal = 38, high = 57), sd = 10.0),
  age= fuzzy_partition(varnames = c(young = 45, mid = 27, elder = 18), sd = 7.5),
  risk = fuzzy_partition(varnames = c(minor = 40, medium = 65, major= 80),FUN = fuzzy_cone, radius = 10)
)
## Fuzzy rules
rules <- set(
  fuzzy_rule(credit %is% good && income_ratio %is% small && age %is% elder, risk %is% minor),
  fuzzy_rule(credit %is% good && income_ratio %is% high && age %is% young, risk %is% major),
  fuzzy_rule(credit %is% bad, risk %is% major), 
  fuzzy_rule(credit %is% okay || income_ratio %is% normal || age %is% mid, risk %is% medium),
  fuzzy_rule(credit %is% okay && age %is% young, risk %is% medium),
  fuzzy_rule(credit %is% good && income_ratio %is% normal && age %is% young, risk %is% medium)
)

## Plotting
model <- fuzzy_system(variables, rules)
print(model)
plot(model)

example.1 <- fuzzy_inference(model, list(credit = 660, income_ratio = 5, age = 21))

plot(example.1)
gset_defuzzify(example.1, "centroid")

example.2 <- fuzzy_inference(model, list(credit = 700, income_ratio = 20, age = 40))
plot(example.2)
gset_defuzzify(example.2, "largestofmax")


## Reset universe
sets_options("universe", NULL)