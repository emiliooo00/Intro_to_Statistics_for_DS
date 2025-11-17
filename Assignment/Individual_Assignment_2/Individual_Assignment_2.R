# 載入資料
data("ToothGrowth")

# (a) 變異數同質性：Levene test
# install.packages("car") # 若未安裝
library(car)
leveneTest(len ~ supp, data = ToothGrowth)

# 根據 Levene 結果選擇 var.equal：
# 若 Levene p-value >= 0.05 → var.equal = TRUE，否則 FALSE
tt_a <- t.test(len ~ supp, data = ToothGrowth, var.equal = TRUE)  # 先給 Welch 範例
tt_a

# 你需要回填至 LaTeX 的欄位（參考 tt_a 輸出）：
# t 值：      unname(tt_a$statistic)
# df：        unname(tt_a$parameter)
# p 值：      tt_a$p.value
# 平均數差：  diff(rev(tapply(ToothGrowth$len, ToothGrowth$supp, mean))) # OJ-VC
# 信賴區間：  tt_a$conf.int


# 篩選 2 mg/day 子集
tg2 <- subset(ToothGrowth, dose == 2)

# 正態性檢查：各組分開做
shapiro.test(tg2$len[tg2$supp == "OJ"])
shapiro.test(tg2$len[tg2$supp == "VC"])

# 變異數同質性
leveneTest(len ~ supp, data = tg2)

# 視覺化（可選）：QQ plot & 箱線圖
par(mfrow = c(1,3))
boxplot(len ~ supp, data = tg2, main = "2mg len by supp")
qqnorm(tg2$len[tg2$supp == "OJ"]); qqline(tg2$len[tg2$supp == "OJ"])
qqnorm(tg2$len[tg2$supp == "VC"]); qqline(tg2$len[tg2$supp == "VC"])
par(mfrow = c(1,1))



par(mfrow = c(1,3))

boxplot(len ~ supp, data = tg2, main = "Tooth Length at 2mg")

qqnorm(tg2$len[tg2$supp == "OJ"], main = "QQ Plot - OJ")
qqline(tg2$len[tg2$supp == "OJ"])

qqnorm(tg2$len[tg2$supp == "VC"], main = "QQ Plot - VC")
qqline(tg2$len[tg2$supp == "VC"])

#-----------------------Q2-------------------------------------------

# 基本摘要
mean(temp$Temp)
sd(temp$Temp)
length(temp$Temp)

# t-test
t.test(temp$Temp, mu = 13.9)

# 1% 雙尾臨界值
qt(0.995, df = length(temp$Temp) - 1)

