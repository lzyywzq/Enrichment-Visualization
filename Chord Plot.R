Chordplot <- function(data=chord,UPDN=UPDN){
  # 01. 和弦图
  chord <- chord_dat(data=circ, genes = genedata)  # 生成带有选定基因列表的矩阵
  chord <- chord_dat(data=circ, process = GO$Term) #生成带有选定GO term的列表矩阵
  
  chord <- chord_dat(data=circ, genes=genedata,process = GO$Term) # 构建数据
  GOChord (         data=chord,
                    title = paste0("GO_CC_",UPDN) ,
                    space = 0.02 , # go term处间隔大小
                    limit = c(1,5), #第一个值是至少分配给一个基因的go term数目，第二个数值是至少分配给一个Go term的基因数
                    gene.order ='logFC',gene.space=0.25,gene.size=4,
                    lfc.col = c('firebrick3','white','royalblue3'), #上下调基因颜色
                    # ribbon.col=brewer.pal(length(GO$Term)),"set3", #GO term颜色
                    process.label = 8 # Go terms字体大小
  )}
