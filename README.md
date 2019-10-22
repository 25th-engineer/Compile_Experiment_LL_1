# Compile_Experiment_LL1
合肥工业大学编译原理实验LL(1)文法分析，``Scala``实现，<br>消除了``形式上``的``左递归``，但存在bug，不能直接用；过实验指导书的``样例``没问题。
****
* [CSDN博客位置](https://blog.csdn.net/u25th_engineer/article/details/102643982)
* [博客园位置](https://www.cnblogs.com/25th-engineer/p/11707006.html)
****
## 开发环境：
### 硬件： 
>>>>Dell G3 3579；
### 软件：
>>>>OS：Ubuntu 16.04.06；
>>>>IDE：InterlliJ IDEA Ultimate Edition（2019.1.3）；
>>>>编程语言：Scala、Java。
## 完成情况：
>>代码行数：
>>>>①纯粹自己写的、不含GUI代码、不含注释、不含空行：876；
>>>>②含参考而来的GUI代码与各类注释：1664；
>>数据结构与算法设计：<br>
①核心代码部分，均为自己参照书上的算法，用极为朴素的方法实现（Scala，876）；<br>②值得一提的是，FOLLOW集采用“先定位”，后“记忆搜索”的方法，对于不含左递归的文法，时空不甚可观；<br>③消除了形式上的左递归，但含bug；<br>④GUI的编写参考“菊花侠”大佬的手笔，Java封装实现界面。
