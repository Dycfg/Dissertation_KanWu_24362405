# 📦 Required Library
# install.packages("DiagrammeR") # 如果尚未安装
library(DiagrammeR)

grViz("
digraph conceptual_framework {

  graph [layout = dot, rankdir = TB, fontsize = 12, labelloc = t,
         label = '']

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 12,
        color = black, penwidth = 1.5]

  # Explanatory / Experimental Design Variables
  SwardType [label = 'Sward Type', fillcolor = lightblue]
  Management [label = 'Management', fillcolor = lightblue]
  Year [label = 'Year', fillcolor = lightblue]
  Block [label = 'Block (Random)', fillcolor = lightblue]

  # Ecosystem Service Pathways
  SoilQuality [label = 'Soil Quality', fillcolor = palegreen]
  Biodiversity [label = 'Biodiversity', fillcolor = palegreen]

  # Model & Response
  Model [label = 'Multivariate Mixed Models', style = filled, fillcolor = gray90]
  DMY [label = 'Dry Matter Yield (DMY)', style = filled, fillcolor = gray90]

  # Arrows: Experiment → Model
  SwardType -> Model [penwidth = 2]
  Management -> Model [penwidth = 2]
  Year -> Model [penwidth = 2]
  Block -> Model [penwidth = 2]

  # Arrows: Model → Outcome
  Model -> DMY [penwidth = 2]

  # Arrows: Ecosystem services → Outcome
  SoilQuality -> DMY [penwidth = 2]
  Biodiversity -> DMY [penwidth = 2]
}
")
