# 函数参数除了 default，其他直接转换 hiplot 前端支持的所有选项
#
# @hiplot start
# @appname helloworld
# @alias An-Example
# @apptitle
# Hiplot Hello World
# Hiplot 示例插件
# @target basic
# @tag test dotplot
# @author your name
# @url your project link
# @citation any reference you should link to
# @version 0.1.0
# @release 2021-01-01
# @description
# en: One sentence to describe this plugin.
# zh: 插件一段话简单介绍。
# @main helloworld
# @library ggplot2 readr
# @param data export::data::hiplot-textarea::{"default": "data.txt", "required": true}
# en: Data Table
# zh: 数据表
# @param x export::dataArg::data::{"index":1, "default": "mpg", "required": true}
# en: X Axis Variable
# zh: X 轴变量
# @param y export::dataArg::data::{"index":2, "default": "vs", "blackItems": "carb", "required": false}
# en: X Axis Variable
# zh: Y 轴变量
# @param size export::extra::slider::{"default":2, "min":0.5, "max":5, "step":0.5, "class":"col-12"}
# en: Dot Size
# zh: 点大小
# @param add_line export::extra::switch::{"default": true, "class":"col-12"}
# en: Add Line
# zh: 添加线图
# @return ggplot::["pdf", "png"]::{"cliMode": true, "title": "A test plot", "width":4, "height": 4, "theme": "theme_bw"}
# @data
# # You can write the code to generate the example data
# # 'data.txt' described in parameter data, or you can
# # omit this tag and submit prepared data files.
# # File size <100Kb is recommended.
# # 此处可以编写生成示例数据（建议小于 100Kb）的代码
# # 示例数据文件需要跟数据表格参数对应起来
# # 或者忽略该标签，提交已经准备好的示例数据
# library(readr)
# data("mtcars")
# write_tsv(mtcars, "data.txt")
# @hiplot end

library(ggplot2)
helloworld <- function(data, x, y, size = 2, add_line = TRUE) {
  if (y == "") stop("y must be provided!")
  p <- ggplot(data, aes_string(x = x, y = y))
  p <- p + geom_point(size = size)
  if (add_line) {
    p <- p + geom_line()
  }
  # Here export a ggplot object
  # Or the whole main function generate a basic R plot
  return(p)
}
