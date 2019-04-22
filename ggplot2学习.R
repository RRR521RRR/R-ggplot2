###设置工作目录
setwd("C:/Users/aceracer/Desktop/R工作目录")

library(ggplot2)
library(gcookbook)
library(readxl)
data <- read_excel('600848.xlsx')
###快速绘图
qplot(carat,price,data = diamonds,colour = color)
qplot(carat,data = diamonds,geom = "bar",
      main = "xxxw",xlab = "wo",ylab = "hhh")
###散点图
ggplot(mpg,aes(displ, hwy, colour = class)) + geom_point()
###图片输出
ggsave(filename = '散点图.png',width = 10,height = 10,dpi = 300)
###折线图
ggplot(data,aes(close,volume)) + geom_line() + geom_point()
###离散  直方图
ggplot(mpg, aes(fl)) + geom_bar(fill="lightblue", colour="black")
##########

ggplot(mpg, aes(hwy))+ geom_density(kernel = "gaussian")
ggplot(mpg, aes(hwy))+geom_dotplot()


# 第3章　条形图
# 3.2绘制簇状条形图 
#将分类变量映射到fill参数，并运行命令geom_bar(position="dodge") 
#使得两组条形在水平方向上错开排列，否则，系统会输出堆积条形图
#其中stat="identity"表明取用样本点对应纵轴值
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
        geom_bar(position="dodge", stat="identity", colour="black") +
        scale_fill_brewer(palette="Paste7")

# 3.3　绘制频数条形图
ggplot(diamonds, aes(x=cut)) + geom_bar()
# 3.4　条形图着色
ggplot(uspopchange, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")
#颜色的映射 设定是在aes() 内部完成的，而颜色的重新设定 是在aes() 外部完成的
#reorder(),根据条形图的高度进行排序比按照字母顺序对分类变量排序更有意义。
ggplot(uspopchange, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
        geom_bar(stat="identity", colour="black") +
        scale_fill_manual(values=c("#669933","#FFCC66","#881122","#113355")) +
        xlab("State")
# 3.5对正负条形图分别着色
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >=0
#我们可以通过scale_fill_manual() 参数对图形颜色进行调整，
#设定参数guide=FALSE 可以删除图例
#通过设定边框颜色（colour）和边框线宽度（size）为图形填加一个细黑色边框
#边框线宽度（size） 是用来控制边框线宽度的参数，单位是毫米
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
        geom_bar(stat="identity", position="identity", 
                 colour="black", size=0.05) +
        scale_fill_manual(values=c("#CCEEFF","#FFDDDD"), guide=FALSE)
# 3.6　调整条形宽度和条形间距
#通过设定geom_bar() 函数的参数width 可以使条形变得更宽或者更窄
#该参数的默认值为0.9；更大的值将使绘制的条形更宽，反之则是更窄
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.9)
#添加条形组距 position_dodge 函数中width 参数的默认值也是0.9
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
      geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))

# 3.7　绘制堆积条形图
# 使用geom_bar() 函数，并映射一个变量给填充色参数（fill）
# 通过guides() 函数对图例顺序进行调整
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
      geom_bar(stat="identity", colour="black") +
      guides(fill=guide_legend(reverse=TRUE)) +
      scale_fill_brewer(palette="Pastel1")

# 3.9　添加数据标签
# 在绘图命令中加上geom_text() 即可为条形图添加数据标签。
# 运行命令时，需要分别指定一个变量映射给x 、y 和标签本身。
# 通过设定vjust （竖直调整数据标签位置）
# 可以将标签位置移动至条形图顶端的上方或者下方
# 在条形图顶端下方
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=Weight), vjust=1.5, colour="white")
# 在条形图顶端上方
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=Weight), vjust=-0.2)
# 将y轴上限变大
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
      geom_bar(stat="identity") +
      geom_text(aes(label="Weight"), vjust=-0.2) +
      ylim(0, max(cabbage_exp$Weight)*1.05)
# 对于簇状条形图，需要设定position=position_dodge() 
# 使用字号（size） 来缩小数据标签的字体大小以匹配条形宽度。
# 数据标签的默认字号是5，将字号设定为3使其看起来更小
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
      geom_bar(stat="identity", position="dodge") +
      geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(0.9), size=3)
# 3.10　绘制Cleveland点图
tophit <- tophitters2001[1:25, ] # 取出tophitters数据集中的前25个数据
# 根据变量avg 对变量name 进行排序,借助reorder(name,avg) 函数实现
ggplot(tophit, aes(x=avg, y=reorder(name,avg))) +
      geom_point(size=3, colour='blue') +　　　# 使用更大的点
      theme_bw() +
  #geom_segment() 函数用“以数据点为端点的线段”代替贯通全图的网格线。
  #注意geom_segment() 函数需要设定x 、y 、xend 和yend 四个参数
      geom_segment(aes(yend=name), xend=0, colour="grey50")+
      #删除垂直网格线，并将水平网格线的线型修改为虚线
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

#第4章　折线图

# 4.2　向折线图添加数据标记
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()

# 4.3　绘制多重折线图
# 在分别设定一个映射给x 和y 的基础上，再将另外一个（离散型）变量
# 映射给颜色（colour ）或者线型（linetype ）即可
# 将supp映射给颜色（colour）
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()
# 将supp映射给线型（linetype）
ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line()
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() +
        geom_point(size=4, shape=21) # 使用有填充色的点
# 如果x 变量是因子，你必须同时告诉ggplot() 用来分组的变量
# 不可缺少group=supp 语句.
# 否则，ggplot() 会不知如何将数据组合在一起绘制折线图，从而会报错
ggplot(tg, aes(x=factor(dose), y=length, colour=supp, group=supp)) + geom_line()

# 4.4　修改线条样式
# 通过geom_line()设置线型（linetype ）、线宽（size ）和颜色（colour ）参数
# 可以分别修改折线的线型、线宽和颜色。
# 4.5　修改数据标记样式
# 设定函数geom_point() 的大小（size ）、颜色（colour ）和填充色（fill ）
ggplot(tg, aes(x=dose, y=length, colour=supp, group=supp)) +
  geom_line(linetype="dashed", size=1, colour="blue") +
  geom_point(shape=22, size=3, fill="white")

# 4.6　绘制面积图
# 运行geom_area() 函数即可绘制面积图
ggplot(mpg, aes(hwy))+geom_area(stat = "bin",colour="black", fill="blue", alpha=.2)

# 4.7　绘制堆积面积图
# 运行geom_area() 函数，并映射一个因子型变量给填充色（fill ）
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()+
  scale_fill_brewer(palette="Drak2")
  

# 4.9　添加置信域
# 运行geom_ribbon() ，然后分别映射一个变量给ymin 和ymax 
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y),alpha=0.2) +
  geom_line()

# 第5章　散点图
# 5.1　绘制基本散点图
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=21,size=2.2)

# 5.2　使用点形和颜色属性，并基于某变量对数据进行分组
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) + geom_point()
# 分组变量必须是分类变量，换言之，它必须是因子型或者字符串型的向量。
# 如果分组变量以数值型变量进行存储，则需要将它转化为因子型变量之后，
# 才能以其作为分组变量。可以将一个变量同时映射给shape 和colour 属性。
# 当有多个分组变量时，可以将它们分别映射给这两个图形属性。
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) +
      geom_point()

# 5.3　使用不同于默认设置的点形
# 通过调用scale_shape_manual() 函数可以使用其他点形；
# 调用scale_colour_brewer() 或者scale_colour_manual() 函数可以使用其他调色板
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) +
      geom_point() +
      scale_shape_manual(values=c(1,2)) +
      scale_colour_brewer(palette="Set1")

# 5.4　将连续型变量映射到点的颜色或大小属性上
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) + 
      geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) + 
      geom_point()
# 使用guide_legend()函数以离散的图例代替色阶
ggplot(heightweight, aes(x=ageYear, y=heightIn, fill=weightLb)) +
      geom_point(shape=21, size=2.5) +
      scale_fill_gradient(low="blue", high="red", 
      breaks=seq(70,170,by=20),guide=guide_legend())

ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) +
      geom_point(alpha=.5) +
      scale_size_area() + # 使数据点面积正比于变量值
      scale_colour_brewer(palette="Set1")

# 5.5　处理图形重叠
# 设定alpha 参数可以使数据点半透明，通过设定alpha=.1 和alpha=.01 
# 使数据点分别具有90%和99%的透明度
ggplot(diamonds, aes(x=carat, y=price))+ geom_point(alpha=.01)
# 另外一个解决方案是将数据点分箱 （bin）并以矩形来表示
# 默认情况下，stat_bin_2d() 函数分别在x 轴和y 轴方向上将数据分割为30个组
ggplot(diamonds, aes(x=carat, y=price))+ 
      stat_bin2d(bins=50) +
      scale_fill_gradient(low="lightblue", high="red", limits=c(0, 6000))


# 5.6　添加回归模型拟合线
# 运行stat_smooth() 函数并设定method=lm 即可向散点图中添加线性回归拟合线
# 默认情况下，stat_smooth() 函数会为回归拟合线添加95%的置信域，
# 置信域对应的置信水平可通过设置level 参数来进行调整。
# 设定参数se=FALSE 时，系统将不会对回归拟合线添加置信域
# 99%置信域
ggplot(heightweight, aes(x=ageYear, y=heightIn))+ 
      geom_point(colour='red') + stat_smooth(method=lm, level=0.99)
# 没有置信域
ggplot(heightweight, aes(x=ageYear, y=heightIn))+ 
      geom_point() + stat_smooth(method=lm, se=FALSE)
# 拟合loess 曲线（局部加权多项式）
ggplot(heightweight, aes(x=ageYear, y=heightIn))+ 
      geom_point(colour="grey60") + stat_smooth(method=loess)
#分组
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
      geom_point() +scale_colour_brewer(palette="Set1")+ geom_smooth()

# 5.9　向散点图添加模型系数
# 调用annotate() 函数向其手动添加文本
# 设置parse=TRUE 调用R的数学表达式语法来输入公式
ggplot(heightweight, aes(x=ageYear, y=heightIn))+ 
      geom_point(colour='red') + stat_smooth(method=lm)+
      annotate("text", label="r^2 == 0.42", parse = TRUE, x=16.5, y=52)

# 5.10　向散点图添加边际地毯,调用geom_rug() 函数
# 边际地毯图本质上是一个一维的散点图，它可被用于展示每个坐标轴上数据的分布情况
ggplot(faithful, aes(x=eruptions, y=waiting)) + 
      geom_point() +geom_rug(position="jitter", size=.2)

# 5.11　向散点图添加标签
# 调用annotate() 函数或者geom_text() 函数可以为一个或几个数据点添加标签
ggplot(subset(countries, Year==2009 & healthexp>2000),
       aes(x=healthexp, y=infmortality)) +
      geom_point()+ annotate("text", x=4350, y=5.4, label="Canada") +
      annotate("text", x=7400, y=6.8, label="USA")
# 根据数据集自动向散点图添加数据标签,使用geom_text() 函数
# 设定vjust=0 时，标签文本的基线会与数据点对齐
# 设定vjust=1 时，标签文本的顶部会与数据点对齐
# 左对齐，可设置hjust=0 
# 右对齐，可设定hjust=1
ggplot(subset(countries, Year==2009 & healthexp>2000),
       aes(x=healthexp, y=infmortality)) +
        geom_point(size=0.5)+ 
        geom_text(aes(label=Name), size=3,colour="red",vjust=1)
# 5.12　绘制气泡图,令点的面积正比于变量值
# 调用geom_point() 和scale_size_area() 函数即可绘制气泡图
cdat <- subset(countries, Year==2009 &
                 Name %in% c("Canada", "Ireland", "United Kingdom", "United States",
                             "New Zealand", "Iceland", "Japan", "Luxembourg",
                             "Netherlands", "Switzerland"))
# 将GDP映射给半径size
ggplot(cdat, aes(x=healthexp, y=infmortality, size=GDP)) +
      geom_point(shape=21, colour="black", fill="cornsilk")+ 
      scale_size_area(max_size=15)
#其他用法
# 对男性组和女性组求和
hec <- HairEyeColor[,,"Male"] + HairEyeColor[,,"Female"]

# 转化为长格式（long format）
library(reshape2)
hec <- melt(hec, value.name="count")
ggplot(hec, aes(x=Eye, y=Hair)) +
  geom_point(aes(size=count), shape=21, colour="black", fill="cornsilk") +
  scale_size_area(max_size=20, guide=FALSE) +
  geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22, label=count), vjust=1,
            colour="grey60", size=4)

# 第12章　配色
colors() #会显示各种默认颜色
# 12.2　将变量映射到颜色上
ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point()
# 在ggplot()中因子化
ggplot(mtcars, aes(x=wt, y=mpg, colour=factor(cyl))) + geom_point()

# 12.3　对离散型变量使用不同的调色板
# scale_fill_brewer() scale_colour_brewer() ColorBrewer调色板
# scale_fill_manual() scale_colour_manual() 自定义颜色
RColorBrewer::display.brewer.all() #显示调色板

# 12.4　对离散型变量使用自定义调色板
# 用scale_colour_manual() 函数来自定义颜色
# 使用RGB值
+scale_colour_manual(values=c("#CC6666", "#7777DD"))

# 12.6　对连续型变量使用自定义调色板
# 两色渐变scale_colour_gradient() 
# 三色渐变scale_colour_gradient2() 
# 四色渐变scale_colour_gradientn()
