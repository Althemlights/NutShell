#XiaoHe简介
此代码是XiaoHe第一版稳定版本，可提供生成difftest版本代码和用于流片版本代码，支持mill和sbt分别编译，可在linux和win环境下分别测试CPU。
目前已将NutShell无用代码基本剔除干净，具体core内代码可见XiaoHe目录下，主要分为前端后端访存以及isa，顶层位于top目录下。
#测试流程
##搭建环境
1.首先搭建香山xs-env环境，用此仓库代替NutShell\
2.source setup.csh
##生成支持difftest版本代码并测试程序
1.source testall.csh
##生成支持流片版本代码
1.在Settings.scala中设置为SoCTestSettings\
2.在SSDcore.scala中关闭EnableDifftest\
3.mill chiselModule.runMain top.ysyx BOARD=soctest\
4.自行配置SOC环境并将rtl略作修改复制到vsrc目录下即可测试流片程序
#注意
目前代码使用的SRAM并非Chisel模块的sram，而是以及替换了流片标准所用的SRAM具体可见resources\
