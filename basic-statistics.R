# 方差或标准差是表示一组数据的波动性的大小的指标,标准差是方差的算术平方根,因此方差或标准差可以判断一组数据的稳定性:方差或标准差越大,数据越不稳定;
# 平均数可以反映一组数据的平均水平;
# 众数是一组数据中出现次数最多的数,即众数可以反映一组数据的多数水平;
# 中位数是一组数据中最中间位置的数(奇数个数据时)或最中间的两个数的平均数(偶数个数据时),所以中位数可以反映一组数据的中间位置水平.
# 关于各种分布的解释：http://eyejava.iteye.com/blog/324189

my.sample <- c(21,23,25,20,11,30,19,26,27,25)
print("the my.sample is:")
my.sample
#print mean
sprintf("the mean of my.sample is: %s", mean(my.sample))

#print median(中位数)
sprintf("the median of my.sample is: %s", median(my.sample))

#R not include standard function for mode(众数)，and mode is the most of number in matrix. It's: 25. 

sprintf("the min of my.sample is: %s", min(my.sample))
sprintf("the max of my.sample is: %s", max(my.sample))
print("the range is:")
range(my.sample)

#print quantile(分位数)
print("the my.sample quantile, default seq is 0.25:")
quantile(my.sample)

print("the my.sample quantile by 0.1 seq, code is: quantile(my.sample, seq(0, 1, 0.1))")
quantile(my.sample, seq(0, 1, 0.1))

#define my variance.(方差)
my.var <- function(x) {
	m <- mean(x)
	return(sum((x - m) ^ 2) / (length(x) - 1))
}

sprintf("the my.sample variance(my.var) is: %f", my.var(my.sample))
sprintf("the my.sample variance(R standard function: var) is: %f", var(my.sample))

#define my standard deviation(标准差) = sqrt(variance)
my.sd <- function(x) { 
	return(sqrt(my.var(x)))
}

sprintf("the my.sample standard deviation(my.sd) is: %f", my.sd(my.sample))
sprintf("the my.sample standard deviation(R standard function: sd) is: %f", sd(my.sample))

#直方图, 由于binwidth设置为1, 因此数据呈钟形。大部分数据处于中间,与均值和中位数接近。但这只是 因所选的直方图类型而造成的假象
library('ggplot2')
data.file <- file.path('data', '01_heights_weights_genders.csv') 
heights.weights <- read.csv(data.file, header = TRUE, sep = ',') 
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 1)

#当采用一个较大的区间宽度值时,数据的很多结构就不见了。虽然还有个顶峰,但是之
#前看到的对称性已经几乎不存在了。这叫做过平滑(oversmoothing),与之相反的问题则称为欠平滑(undersmoothing),也是很危险的.
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 5)

#欠平滑(过度对称)
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 0.001)

#使用密度曲线图(density plot)来避免直方图的问题.
ggplot(heights.weights, aes(x = Height)) + geom_density()

#使用密度曲线图(density plot) 并区分男性和女性
ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density()

#将男性和女性分开输出. 显示正态分布.
#分布的均值,决定钟形曲线的中心所在; 分布的方差,决定钟形曲线的宽度.
ggplot(heights.weights, aes(x = Weight, fill = Gender)) + geom_density() + facet_grid(Gender ~ .)

# 用这段代码生成的曲线的基本形状是一样的;改变m和s只是移动其中心或者伸缩其宽度。
# 曲线的具体形状在改变,但是其整体轮廓并没有变化。
# 要注意的是,钟形并不是判断数据是否为正态分布的充分条件,因为还存在其他钟形分布,稍后会介绍其中一种。
# 利用正态分布,可以定义一些关于数据形状的定性概念
m <- 0
s <- 1
ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) + geom_density()

#当构建一条密度曲线时,数据的众数就在钟形的峰值处. 使用密度曲线图能够很容易的观察出众数.
#当谈到数据的众数个数时,会用到以下术语:
#只有一个众数的分布叫单峰 (unimodal;
#有两个众数的分布叫双峰(bimodal);
#有两个以上众数的分布叫多峰 (multimodal)

#正态分布所定义的众数有一个特点:它只有一个众数,同时也是数据的均值和中位数.
#并且小于众数的数据和大于众数的数据的可能性是一样的.

#众数右侧观察到极值的可能性要大于其左侧,这种图形称为 伽玛分布gamma distribution

#我们要根据另一个定性的区别来划分出两类数据:窄尾分布(thin-tailed)数据 和 重尾分布(heavy-tailed)数据
#窄尾分布所产生的值通常都在均值附近,99%的可能性 都是这样。比如,正态分布在99%的情况下所产生的数据偏离均值都不会超过三个标准差。
#相比之下,另一个钟形分布——柯西分布(Cauchy distribution)大约只有90%的值落在三个标准差范围内.
#距离均值越远,这两个分布的特点越不同:正态分布几乎不可能产生出距离均值有六个标准差的值,然而柯西分布仍有5%的可能性
#对正态分布的本质特性做个总结:
#正态分布是单峰的、对称的分布,也是钟形的窄尾分布。柯西分布也是单峰的、对称的分布,也是钟形曲线,却是重尾分布.

#个人总结的什么是thin-tailed, 什么是heavy-tailed.
#看曲线图的一头和一尾是否相差很大. 如果相差很大就是heavy-tailed, 相差很小就是thin-tailed.
#柯西分布的方差和标准差都比正态分布的标准差和方差要大. 说明数据比正态分布更离散, 更不稳定.
set.seed(1)
normal.values <- rnorm(250, 0, 1) 
cauchy.values <- rcauchy(250, 0, 1) 
range(normal.values) 
range(cauchy.values)
ggplot(data.frame(X = normal.values), aes(x = X)) + geom_density() 
ggplot(data.frame(X = cauchy.values), aes(x = X)) + geom_density()

#由上得出的正态分布的分类: 一般正态分布和柯西分布. 也可以从窄尾分布和重尾分布来进行分类.
#与正态分布对应的是偏态分布.
#偏态分布包括了: 一个有点偏斜的伽马分布, 和一个非常偏斜的指数分布.
#伽玛分布只有正值
gamma.values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X = gamma.values), aes(x = X)) + geom_density()

#基本的预测数据的问题, 将分为2类:
#1.回归问题: 根据以知的某个信息, 预测另一个信息的值.
#2.分类问题: 给已知的数据贴上标签.	
#研究回归问题时, 通常使用散点图来进行观察:
#绘制散点图
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point()	 
#使用增加平滑曲线去观察散点图.
#数据量越大, 曲线周围的阴影范围越小, 说明数据的预测越精准.
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + geom_smooth()

#可以根据减小数据量来观察, 数据量越小的情况下, 阴影范围越大, 预测越不精准.
ggplot(heights.weights[1:20,], aes(x = Height, y = Weight)) + geom_point() +geom_smooth() 
ggplot(heights.weights[1:200,], aes(x = Height, y = Weight)) + geom_point() +geom_smooth() 
ggplot(heights.weights[1:2000,], aes(x = Height, y = Weight)) + geom_point() +geom_smooth()

#对于分类, 我们需要分析出数据合理的分类标签是什么, 如下列代码, 使用性别分类.
#通过观察散点图 根据身高和体重来预测性别. 散点图根据加入的分类特征: 性别.
#当给定一个人的身高和体重, 数据点落在超平面的一边我们认为女性.
#现实的分类模型往往会采用非常多的特征来预测数据.
ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) + geom_point()

heights.weights <- transform(heights.weights, Male = ifelse( Gender == 'Male', 1, 0))
logit.model <- glm(Male ~ Height + Weight, data = heights.weights, family = binomial(link = 'logit'))
ggplot(heights.weights, aes(x =Height, y = Weight, color = Gender)) + 
geom_point() +
stat_abline(intercept = -coef(logit.model)[1]/coef(logit.model)[2], slope = -coef(logit.model)[3] / coef(logit.model)[2], geom = 'abline',color = 'black')